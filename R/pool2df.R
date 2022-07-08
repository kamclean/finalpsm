# pool2df
# Documentation
#' Pool imputed models together and output alongside unimputed model as fit2df tables.
#' @description Wrapper for performing finalfit modelling using imputed data
#' @param data Dataframe containing 3 columns - imputed and m (from impute function) and a column containing nested model fits for each.
#' @param model Column name for the nested model fits within the tibble (default = "model")
#' @return Returns a fit2df style table for (1) unimputed model (2) pooled imputed models (3) 1 and 2 joined.
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @import broom.mixed
#' @importFrom mice pool
#' @export


pool2df <- function(data, model = "model"){

  if("model" %in% names(data)&model != "model"){
    data <- data %>% select(-model)}

  if((!("model" %in% names(data)))&model == "model"){
  data <- data %>% mutate(model = pull(., model))}

  est_unimp <- finalfit::fit2df((data %>% filter(imputed=="original") %>% pull(model))[[1]],
                                remove_intercept=F) %>%
    dplyr::rename_with(.cols = 2, function(x){paste0(x, "_multi_unimp")}) %>%
    filter(explanatory!="(Intercept)")


  library(broom.mixed)# needed for  mice::pool() to work with glmmixed

  est_imp <- data %>%
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

  return(list("unimputed" = est_unimp, "imputed" = est_imp, "joined" = left_join(est_unimp, est_imp, by = "explanatory")))}

