# finalimp
# Documentation
#' Wrapper for performing finalfit modelling using imputed data
#' @description
#' @param data Output from the impute() function.
#' @param dependent Character vector of length 1: quoted name of dependent variable. Can be continuous, a binary factor, or a survival object of form Surv(time, status).
#' @param explanatory Character vector of any length: quoted name(s) of explanatory variables.
#' @param random_effect  Character vector of length 1, either, (1) name of random intercept variable, e.g. "var1", (automatically convered to "(1 | var1)"); or, (2) the full lme4 specification, e.g. "(var1 | var2)". Note parenthesis MUST be included in (2) but NOT included in (1).
#' @return Nested list of (1) the final model table, and (2) the underlying data, models, and metrics for the original and imputed datasets.
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @import finalfit
#' @import stringr
#' @importFrom mice pool
#' @export

finalimp <- function(data, dependent, explanatory,random_effect = NULL){

  # Check type of model
  if(stringr::str_detect(dependent, "Surv\\(")==T){model = "coxph"}
  if(stringr::str_detect(dependent, "Surv\\(")==F){
    if(dplyr::pull(data, dependent) %>% class()=="factor"){model = "glm"}
    if(dplyr::pull(data, dependent) %>% class()=="numeric"){model = "lm"}}

  if(is.null(random_effect)==F){model <- paste0(model, "mixed")}
  if(is.null(random_effect)==T){
    if(length(explanatory)==1){model <- paste0(model, "uni")}
    if(length(explanatory)>=2){model <- paste0(model, "multi")}}

  # formula depends
  formula <- paste0("finalfit::", model,
                    "(x, dependent = '", dependent,
                    "',explanatory = ", paste0("c('", paste0(explanatory, collapse = "', '"), "')"),
                    ", random_effect = ", ifelse(is.null(random_effect)==T, "NULL", paste0("'", random_effect, "'")), ")")

  if(stringr::str_detect(model, "mixed")==F){formula <- paste0(stringr::str_split_fixed(formula, ", random_effect = ", 2)[,1], ")")}

  impmodel <- data  %>%
    dplyr::group_by(imputed, m) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(data, function(x){eval(parse(text = formula))}),
                  metric = purrr::map_chr(model, function(x){suppressMessages(finalfit::ff_metrics(x)[[1]])}),
                  data = purrr::map(data, function(x){y = eval(parse(text = formula))

                  z <-  x %>% dplyr::mutate(predict = predict(y, type = "response", newdata  = x))
                  return(z)})) %>%
    dplyr::ungroup()

  est_unimp <- finalfit::fit2df((impmodel %>% filter(imputed=="original") %>% pull(model))[[1]],
                                remove_intercept=F) %>%
    dplyr::rename_with(.cols = 2, function(x){paste0(x, "_multi_unimp")}) %>%
    filter(explanatory!="(Intercept)")


  est_imp <- impmodel %>%
    filter(imputed=="imputed") %>%
    pull(model) %>%
    mice::pool() %>%
    summary(conf.int = TRUE, exponentiate = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::select(term, est = estimate, lci = `2.5 %`, uci = `97.5 %`, "p"= p.value) %>%
    dplyr::mutate(p = ifelse(p<0.001, "p<0.001", paste0("p=", format(round(p,3), nsmall=3))),
                  across(c(est, lci, uci), function(x){format(round(x,2), nsmall=2) %>% stringr::str_trim()}),
                  multi_imp = paste0(est, " (", lci, "-", uci, " ", p, ")")) %>%
    dplyr::select("explanatory"=term, multi_imp) %>%
    filter(explanatory!="(Intercept)") %>%
    dplyr::rename_with(.cols = "multi_imp", function(x){stringr::str_remove(names(est_unimp)[2], "un")})

  table <- data %>%
    dplyr::filter(m==0) %>%
    finalfit::summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
    finalfit::ff_merge(est_unimp) %>%
    finalfit::ff_merge(est_imp) %>%
    dplyr::select(-fit_id, -index)


  return(list("table" = table,"model" = impmodel))}
