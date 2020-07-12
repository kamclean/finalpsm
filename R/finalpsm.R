# finalpsm
# Documentation
#' Wrapper for perfoming propensity-score matching with finalfit
#' @description matchit is the main command of the package MatchIt, which enables parametric models for causal inference to work better by selecting well-matched subsets of the original treated and control groups. This function acts as a wrapper to facilitate propensity-score matching with a tidyverse approach.
#' @param matchit_out Dataframe
#' @param dependent Character vector of length 1: quoted name of dependent variable. Can be continuous, a binary factor, or a survival object of form Surv(time, status).
#' @param explanatory Character vector of any length: quoted name(s) of explanatory variables (default = NULL - the strata and explanatory variables from the matchit_out object will be used)
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

finalpsm <- function(matchit_out, dependent, explanatory = NULL, balance = T, metrics = T, fit = T){
  require(dplyr);require(stringr);require(finalfit);require(tibble)
  require(tidyr);require(lme4);require(purrr)

  data <- matchit_out$data # Get matched dataset
  object <- matchit_out$object # Get matchit object

  # Extract model info from matchit output
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

  out <- NULL
  if(dplyr::pull(data, dependent) %>% class()=="factor"){

    formula = as.formula(finalfit::ff_formula(dependent= dependent, explanatory =  explanatory))

    model_fit <- suppressWarnings(eval(bquote(glm(formula = .(formula),
                                                  data = data,
                                                  weights = data$weights,
                                                  family  = "binomial"))))

    model_metric = NULL; if(metrics == TRUE){model_metric <- finalfit::ff_metrics(model_fit)}


    psm <-  suppressWarnings(finalfit::fit2df(model_fit)) %>%
      tibble::as_tibble() %>%
      dplyr::rename("fit_id" = explanatory, "or_psm" = OR) %>%
      dplyr::mutate_all(as.character)

    model_table <- data %>%
      finalfit::finalfit(dependent= dependent,
                         explanatory =  explanatory, keep_fit_id = T) %>%
      tibble::as_tibble() %>%
      dplyr::rename_at(vars(contains("Dependent:")), function(x){x="label"}) %>%
      dplyr::rename(level = ` `, "or_uni" = `OR (univariable)`, "or_multi" = `OR (multivariable)`) %>%
      dplyr::left_join(psm, by = "fit_id") %>%
      dplyr::select(-fit_id, -index) %>%
      dplyr::mutate_at(vars(starts_with("or_")), function(x){ifelse(x=="-", NA, x)}) %>%
      dplyr::mutate(label = ifelse(label=="", NA, label)) %>%
      tidyr::fill(label, .direction = "down") %>%
      tidyr::pivot_longer(cols = starts_with("or_")) %>%
      dplyr::mutate(value = stringr::str_remove_all(value, "\\)|p=|p<")) %>%
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

    out <- list("balance" = if(balance==T){finalpsm::balance(matchit_out)}else{NULL},
                "fit" = if(fit==T){model_fit}else{NULL},
                "table" = model_table,
                "metric" = model_metric) %>%
      purrr::discard(is.null)}






  # coxph and linear models (including ATC / ATT)

  return(out)}
