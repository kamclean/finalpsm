# matchit
# Documentation
#' Wrapper for MatchIt package
#' @description matchit is the main command of the package MatchIt, which enables parametric models for causal inference to work better by selecting well-matched subsets of the original treated and control groups. This function acts as a wrapper to facilitate propensity-score matching with a tidyverse approach.
#' @param .data Dataframe
#' @param strata Column name of the binary treatment indicator (this variable must only have 2 levels).
#' @param explanatory Vector of column names containing variables used to estimate the propensity score (pre-treatment covariates).
#' @param id Vector of column names containing patient identification variables that are to be retained following matching (no use within MatchIt::matchit()).
#' @param dependent Vector of column names containing dependent (outcome) variables that are to be retained following matching (no use within MatchIt::matchit()).
#' @param method As per MatchIt::matchit(). This argument specifies a matching method. Currently, "exact" (exact matching), "full" (full matching), "genetic" (genetic matching), "nearest" (nearest neighbor matching), "optimal" (optimal matching), and "subclass" (subclassification) are available. The default is "nearest". Note that within each of these matching methods, MatchIt offers a variety of options.
#' @param keep_all Keep all variables within the original dataset (default = FALSE)
#' @param ... Additional arguments to be passed to a variety of matching methods.
#' @return Nested list of (1) "object" - the MatchIt::matchit() output and (2) "data": the matched dataset.
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @importFrom finalfit ff_formula
#' @importFrom MatchIt matchit match.data
#' @export

# Function
matchit <- function(.data, strata, explanatory, id = NULL, dependent = NULL, method = "full",keep_all = F, ...){
  require(tibble);require(dplyr);require(tidyr);require(tidyselect)
  require(finalfit);require(MatchIt)

  .data <- .data %>% tibble::rowid_to_column()

  data_match <- .data %>%
    dplyr::select(rowid, tidyselect::all_of(c(strata, explanatory))) %>%
    tidyr::drop_na()

  strata_binary <- paste0(strata, "_01")

  data_match <- data_match %>%
    dplyr::mutate(binary = pull(., strata) %>% as.numeric() %>%
                    factor(levels = unique(sort(.)), labels = c(0,1)) %>% as.character() %>% as.numeric()) %>%
    dplyr::rename_at(vars(binary), function(x){x=strata_binary})

  formula <- as.formula(finalfit::ff_formula(dependent = strata_binary, explanatory = c(explanatory)))

  object <- withCallingHandlers({eval(bquote(MatchIt::matchit(formula = .(formula),
                                                           method = method, data = data_match)))},
                             warning=function(w) {if(endsWith(conditionMessage(w), "to be the same as your original data."))
                               invokeRestart("muffleWarning")})

  data_out = MatchIt::match.data(object) %>% tibble::as_tibble()

  data_out <- data_out %>%
    dplyr::left_join(.data %>% dplyr::select(-tidyselect::any_of(names(data_out)[names(data_out)!="rowid"])),
                     by = "rowid")

  if(keep_all==F){data_out <- data_out %>%
    dplyr::select(tidyselect::all_of(c("rowid", id, strata,strata_binary, explanatory)),
                  tidyselect::any_of(c("distance", "weights","subclass")),
                  tidyselect::any_of(c(dependent)))}

  out <- list("object" = object, "data" = data_out)

  return(out)}
