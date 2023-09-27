# finalimp
# Documentation
#' Wrapper for performing finalfit modelling using imputed data
#' @description Wrapper for performing finalfit modelling using imputed data
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
#' @import broom.mixed
#' @import stringr
#' @importFrom mice pool
#' @export



finalimp <- function(data, dependent, explanatory,random_effect = NULL){



  unnest <- tidyr::unnest(data, cols= "data")

  # Check type of model
  if(stringr::str_detect(dependent, "Surv\\(")==T){modeltype = "coxph"}
  if(stringr::str_detect(dependent, "Surv\\(")==F){
    if(dplyr::pull(unnest, dependent) %>% class()=="factor"){modeltype = "glm"}
    if(dplyr::pull(unnest, dependent) %>% class()=="numeric"){modeltype = "lm"}}

  if(is.null(random_effect)==F){modeltype <- paste0(modeltype, "mixed")}
  if(is.null(random_effect)==T){
    if(length(explanatory)==1){modeltype <- paste0(modeltype, "uni")}
    if(length(explanatory)>=2){modeltype <- paste0(modeltype, "multi")}}

  # formula depends
  formula <- paste0("finalfit::", modeltype,
                    "(x, dependent = '", dependent,
                    "',explanatory = ", paste0("c('", paste0(explanatory, collapse = "', '"), "')"),
                    ", random_effect = ", ifelse(is.null(random_effect)==T, "NULL", paste0("'", random_effect, "'")), ")")

  if(stringr::str_detect(modeltype, "mixed")==F){formula <- paste0(stringr::str_split_fixed(formula, ", random_effect = ", 2)[,1], ")")}


  impmodel <- data  %>%
    dplyr::mutate(model = purrr::map(data, function(x){eval(bquote(eval(parse(text = formula))))}),
                  metric = purrr::map_chr(model, function(x){suppressMessages(finalfit::ff_metrics(x)[[1]])})) %>%
    dplyr::mutate(data = purrr::map(data,
                                    function(x){

                                      y = eval(parse(text = formula))

                                      if(stringr::str_detect(modeltype, "coxph")){predicttype = "survival"}else{predicttype = "response"}

                                      z <-  x %>% dplyr::mutate(predict = predict(y, type =predicttype, newdata  = x))
                  return(z)})) %>%
    dplyr::ungroup()

  pool <- finalpsm::pool2df(data = impmodel)

  table <- unnest %>%
    dplyr::filter(m==0) %>%
    finalfit::summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
    finalfit::ff_merge(pool$unimputed) %>%
    finalfit::ff_merge(pool$imputed) %>%
    dplyr::select(-fit_id, -index)


  return(list("table" = table,"model" = impmodel))}
