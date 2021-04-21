# finalpsm
# Documentation
#' Wrapper for perfoming propensity-score matching with finalfit
#' @description matchit is the main command of the package MatchIt, which enables parametric models for causal inference to work better by selecting well-matched subsets of the original treated and control groups. This function acts as a wrapper to facilitate propensity-score matching with a tidyverse approach.
#' @param matchit_out Output from the finalpsm::matchit() function.
#' @param dependent Character vector of length 1: quoted name of dependent variable. Can be continuous, a binary factor, or a survival object of form Surv(time, status).
#' @param explanatory Character vector of any length: quoted name(s) of explanatory variables (default = NULL - the strata and explanatory variables from the matchit_out object will be used)
#' @param subclass Logical value whether subclass should be included as a random-effect (if this form of matching is used).
#' @param balance Logical value whether a balance table should be supplied in the output (default = T).
#' @param metrics Logical value whether the model metrics should be supplied in the output (default = T).
#' @param fit Logical value whether the model fit object should be supplied in the output (default = T).
#' @return Nested list of the final model table, and if specified the balance table, model metrics, and model fit.
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import finalfit
#' @import stringr
#' @import lme4
#' @importFrom purrr discard
#' @export

finalpsm <- function(matchit_out, dependent, explanatory = NULL, subclass = T, balance = T, metrics = T, fit = T){
  require(dplyr);require(stringr);require(finalfit);require(tibble)
  require(tidyr);require(lme4);require(purrr)

  # Extract from  matchit_out---------------------------
  data <- matchit_out$data %>% # Get matched dataset
    dplyr::mutate_at(vars(matches("subclass")), factor) %>%
    dplyr::filter(match == "Matched")

  object <- matchit_out$object # Get matchit object

  formula_text <- summary(object)[[1]]$formula %>% deparse() %>%
    paste0(collapse = "") %>% stringr::str_squish()

  strata_binary <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,1]
  strata <- stringr::str_remove(strata_binary, "_01")

  if(is.null(explanatory)==F){
    # ensure stratifying variable always included (and first)
    explanatory <- explanatory[! explanatory %in% c(strata_binary, strata)]
    explanatory <- c(strata, explanatory)}

  if(is.null(explanatory)==T){
    explanatory <- formula_text %>%
      stringr::str_replace_all("~", "\\+") %>%
      stringr::str_replace_all(strata_binary, strata) %>%
      stringr::str_split(" \\+ ") %>%
      unlist()}

  # Check type of model
  if(stringr::str_detect(dependent, "Surv\\(")==T){type = "coxph"}
  if(stringr::str_detect(dependent, "Surv\\(")==F){
    if(dplyr::pull(data, dependent) %>% class()=="factor"){type = "logistic"}
    if(dplyr::pull(data, dependent) %>% class()=="numeric"){type = "linear"}}


  if("subclass" %in% names(data)){subclass}else{subclass=F}

  # Check if coxph model
  out <- NULL
  if(type == "coxph"){

    formula = as.formula(finalfit::ff_formula(dependent= dependent, explanatory =  explanatory))


    model_fit <- suppressWarnings(eval(bquote(survival::coxph(formula = .(formula),
                                                              data = data,
                                                              weights = data$weights))))
    model_metric = NULL; if(metrics == TRUE){model_metric <- finalfit::ff_metrics(model_fit)}


    psm <-  suppressWarnings(finalfit::fit2df(model_fit)) %>%
      tibble::as_tibble() %>%
      dplyr::rename("fit_id" = explanatory, "hr_psm" = HR) %>%
      dplyr::mutate_all(as.character)

    var_status <- stringr::str_split_fixed(dependent, ", ", 2)[,2] %>% stringr::str_remove(pattern = "\\)")

    summary <- data %>%
      dplyr::mutate(var_status = pull(., var_status) %>% factor()) %>%
      finalfit::summary_factorlist(dependent = "var_status",  explanatory =  explanatory, fit_id = T) %>%
      dplyr::select(fit_id, `FALSE`, `TRUE`)

    model_table <- data %>%
      finalfit::finalfit.coxph(dependent= dependent,
                               explanatory =  explanatory, keep_fit_id = T) %>%
      tibble::as_tibble() %>%
      dplyr::rename_at(vars(contains("Dependent:")), function(x){x="label"}) %>%
      dplyr::rename(level = ` `, "hr_uni" = `HR (univariable)`, "hr_multi" = `HR (multivariable)`) %>%
      dplyr::left_join(psm, by = "fit_id") %>%
      dplyr::left_join(summary, by = "fit_id") %>%
      dplyr::select(-fit_id, -index) %>%
      dplyr::mutate_at(vars(starts_with("hr_")), function(x){ifelse(x=="-", NA, x)}) %>%
      dplyr::mutate(label = ifelse(label=="", NA, label)) %>%
      tidyr::fill(label, .direction = "down") %>%
      tidyr::pivot_longer(cols = starts_with("hr_")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "\\)|p=")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "p")) %>%
      dplyr::mutate(hr = paste0(stringr::str_split_fixed(value, ", ", 2)[,1], ")"),
                    p = stringr::str_split_fixed(value, ", ", 2)[,2]) %>%
      dplyr::mutate(hr = ifelse(hr==")", "-", hr),
                    name = stringr::str_remove(name, "or_")) %>%
      dplyr::select(-value) %>%
      tidyr::pivot_wider(names_from = "name", values_from = c("hr","p")) %>%
      dplyr::rename_at(vars(contains("_uni"), contains("_multi"), contains("_psm")),
                       function(x){paste0(stringr::str_split_fixed(x, "_", 2)[,2],
                                          "_",
                                          stringr::str_split_fixed(x, "_", 2)[,1])}) %>%
      dplyr::select(label, level, `FALSE`, `TRUE`,
                    contains("uni_"),contains("multi_"),contains("psm_")) %>%
      dplyr::rename_at(vars(starts_with("hr_")), function(x){stringr::str_remove(x, pattern = "hr_")}) %>%

      dplyr::group_by(label) %>%
      dplyr::mutate(n = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = ifelse(n>1, "", label)) %>%
      dplyr::select(-n)

    out <- list("balance" = if(balance==T){finalpsm::balance_table(matchit_out)}else{NULL},
                "fit" = if(fit==T){model_fit}else{NULL},
                "table" = model_table,
                "metric" = model_metric)}

  if(type == "logistic"){

    formula = as.formula(finalfit::ff_formula(dependent= dependent,
                                              explanatory =  explanatory,
                                              random_effect = if("subclass" %in% names(data)&subclass==T){"subclass"}else{NULL}))

    if(subclass==F){
      model_fit <- suppressWarnings(eval(bquote(glm(formula = formula,
                                                    data = data,
                                                    weights = data$weights,
                                                    family  = "binomial"))))}

    if(subclass==T){
      model_fit <- suppressWarnings(eval(bquote(lme4::glmer(formula = formula,
                                                            data = data,
                                                            weights = data$weights,
                                                            family  = "binomial"))))}

    model_metric = NULL; if(metrics == TRUE){model_metric <- finalfit::ff_metrics(model_fit)}


    psm <-  suppressWarnings(finalfit::fit2df(model_fit)) %>%
      tibble::as_tibble() %>%
      dplyr::rename("fit_id" = explanatory, "or_psm" = OR) %>%
      dplyr::mutate_all(as.character)

    model_table <- data %>%
      finalfit::finalfit(dependent= dependent,
                         explanatory =  explanatory,
                         random_effect = if("subclass" %in% names(data)&subclass==T){"subclass"}else{NULL},
                         keep_fit_id = T) %>%
      tibble::as_tibble() %>%
      dplyr::rename_at(vars(contains("Dependent:")), function(x){x="label"}) %>%
      dplyr::rename_at(vars(contains("OR (multi")), function(x){x="or_multi"}) %>%
      dplyr::rename(level = ` `, "or_uni" = `OR (univariable)`) %>%
      dplyr::left_join(psm, by = "fit_id") %>%
      dplyr::select(-fit_id, -index) %>%
      dplyr::mutate_at(vars(starts_with("or_")), function(x){ifelse(x=="-", NA, x)}) %>%
      dplyr::mutate(label = ifelse(label=="", NA, label)) %>%
      tidyr::fill(label, .direction = "down") %>%
      tidyr::pivot_longer(cols = starts_with("or_")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "\\)|p=")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "p")) %>%
      dplyr::mutate(or = paste0(stringr::str_split_fixed(value, ", ", 2)[,1], ")"),
                    p = stringr::str_split_fixed(value, ", ", 2)[,2]) %>%
      dplyr::mutate(or = ifelse(or==")", "-", or),
                    name = stringr::str_remove(name, "or_")) %>%
      dplyr::select(-value) %>%
      tidyr::pivot_wider(names_from = "name", values_from = c("or","p")) %>%
      dplyr::rename_at(vars(contains("_uni"), contains("_multi"), contains("_psm")),
                       function(x){paste0(stringr::str_split_fixed(x, "_", 2)[,2],
                                          "_",
                                          stringr::str_split_fixed(x, "_", 2)[,1])}) %>%
      dplyr::select(label, level, levels(pull(data, dependent)),
                    starts_with("uni_"),starts_with("multi_"),starts_with("psm_")) %>%

      dplyr::group_by(label) %>%
      dplyr::mutate(n = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = ifelse(n>1, "", label)) %>%
      dplyr::select(-n)

    out <- list("balance" = if(balance==T){finalpsm::balance_table(matchit_out)}else{NULL},
                "fit" = if(fit==T){model_fit}else{NULL},
                "table" = model_table,
                "metric" = model_metric) %>%
      purrr::discard(is.null)}

  # linear models (including ATC / ATT)
  if(type == "linear"){

    formula = as.formula(finalfit::ff_formula(dependent= dependent,
                                              explanatory =  explanatory,
                                              random_effect = if("subclass" %in% names(data)&subclass==T){"subclass"}else{NULL}))

    if(subclass==F){
      model_fit <- suppressWarnings(eval(bquote(lm(formula = .(formula),
                                                   data = data,
                                                   weights = data$weights))))}

    if(subclass==T){
      model_fit <- suppressWarnings(eval(bquote(lme4::lmer(formula = .(formula),
                                                           data = data,
                                                           weights = data$weights))))}

    model_metric = NULL; if(metrics == TRUE){model_metric <- finalfit::ff_metrics(model_fit)}


    psm <-  suppressWarnings(finalfit::fit2df(model_fit)) %>%
      tibble::as_tibble() %>%
      dplyr::rename("fit_id" = explanatory, "beta_psm" = Coefficient) %>%
      dplyr::mutate_all(as.character)

    model_table <- data %>%
      finalfit::finalfit(dependent= dependent,
                         explanatory =  explanatory,
                         random_effect = if("subclass" %in% names(data)&subclass==T){"subclass"}else{NULL},
                         keep_fit_id = T) %>%
      tibble::as_tibble() %>%
      dplyr::rename_at(vars(contains("Dependent:")), function(x){x="label"}) %>%
      dplyr::rename_at(vars(contains("Coefficient (multi")), function(x){x="beta_multi"}) %>%
      dplyr::rename(level = ` `, "beta_uni" = `Coefficient (univariable)`) %>%
      dplyr::left_join(psm, by = "fit_id") %>%
      dplyr::select(-fit_id, -index) %>%
      dplyr::mutate_at(vars(starts_with("beta_")), function(x){ifelse(x=="-", NA, x)}) %>%
      dplyr::mutate(label = ifelse(label=="", NA, label)) %>%
      tidyr::fill(label, .direction = "down") %>%
      tidyr::pivot_longer(cols = starts_with("beta_")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "\\)|p=")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "p")) %>%
      dplyr::mutate(beta = paste0(stringr::str_split_fixed(value, ", ", 2)[,1], ")"),
                    p = stringr::str_split_fixed(value, ", ", 2)[,2]) %>%
      dplyr::mutate(beta = ifelse(beta==")", "-", beta),
                    name = stringr::str_remove(name, "beta_")) %>%
      dplyr::select(-value) %>%
      tidyr::pivot_wider(names_from = "name", values_from = c("beta","p")) %>%
      dplyr::rename_at(vars(contains("_uni"), contains("_multi"), contains("_psm")),
                       function(x){paste0(stringr::str_split_fixed(x, "_", 2)[,2],
                                          "_",
                                          stringr::str_split_fixed(x, "_", 2)[,1])}) %>%
      dplyr::select(label, level, levels(pull(data, dependent)),
                    starts_with("uni_"),starts_with("multi_"),starts_with("psm_")) %>%

      dplyr::group_by(label) %>%
      dplyr::mutate(n = 1:n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = ifelse(n>1, "", label)) %>%
      dplyr::select(-n)

    out <- list("balance" = if(balance==T){finalpsm::balance_table(matchit_out)}else{NULL},
                "fit" = if(fit==T){model_fit}else{NULL},
                "table" = model_table,
                "metric" = model_metric) %>%
      purrr::discard(is.null)}

  return(out)}
