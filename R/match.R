# matchit
# Documentation
#' Wrapper for MatchIt package
#' @description match is the main command of the package MatchIt, which enables parametric models for causal inference to work better by selecting well-matched subsets of the original treated and control groups. This function acts as a wrapper to facilitate propensity-score matching with a tidyverse approach.
#' @param .data Dataframe
#' @param strata Column name of the binary treatment indicator (this variable must only have 2 levels).
#' @param explanatory Vector of column names containing variables used to estimate the propensity score (pre-treatment covariates).
#' @param id Vector of column names containing patient identification variables that are to be retained following matching (no use within MatchIt::matchit()).
#' @param dependent Vector of column names containing dependent (outcome) variables that are to be retained following matching (no use within MatchIt::matchit()).
#' @param method As per MatchIt::matchit(). This argument specifies a matching method. Currently, "exact" (exact matching), "full" (full matching), "genetic" (genetic matching), "nearest" (nearest neighbor matching), "optimal" (optimal matching), and "subclass" (subclassification) are available. The default is "nearest". Note that within each of these matching methods, MatchIt offers a variety of options.
#' @param keep_col Keep all columns within the original dataset (default = FALSE)
#' @param keep_unmatch Keep unmatched patients within the output (default = TRUE)
#' @param caliper As per MatchIt::matchit. For methods that allow it, the width(s) of the caliper(s) to use in matching. Should be a numeric vector with each value named according to the variable to which the caliper applies. To apply to the distance measure, the value should be unnamed. See the individual methods pages for information on whether and how this argument is used. The default is NULL for no caliper.
#' @param replace As per MatchIt::matchit. For methods that allow it, whether matching should be done with replacement (TRUE), where control units are allowed to be matched to several treated units, or without replacement (FALSE), where control units can only be matched to one treated unit each. See the individual methods pages for information on whether and how this argument is used. Default is FALSE for matching without replacement.
#' @param ratio As per MatchIt::matchit. For methods that allow it, how many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. See the individual methods pages for information on whether and how this argument is used. The default is 1 for 1:1 matching.
#' @param ... Additional arguments to be passed to a variety of matching methods.
#' @return Nested list of (1) "object" - the MatchIt::matchit() output and (2) "data": the matched dataset.
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import MatchIt
#' @importFrom finalfit ff_formula
#' @export

# Function
match <- function(.data, strata, explanatory, id = NULL, dependent = NULL,
                    method = "full",keep_col = F, keep_unmatch = T,
                  caliper = NULL, replace = FALSE, ratio = 1, ...){

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
                                                           method = method, data = data_match,
                                                           caliper=caliper, replace = replace, ratio = ratio, ...)))},
                             warning=function(w) {if(endsWith(conditionMessage(w), "to be the same as your original data."))
                               invokeRestart("muffleWarning")})

  data_matchit = MatchIt::match.data(object) %>% tibble::as_tibble()

  # determine what patients were matched
  data_out <- object$model$data %>%
    dplyr::mutate(distance = object$model$fitted.values) %>%
    dplyr::left_join(.data %>% dplyr::select(-tidyselect::any_of(names(data_matchit)[names(data_matchit)!="rowid"])),
                     by = "rowid") %>%
    dplyr::left_join(data_matchit %>% dplyr::select(any_of(c("rowid", "distance", "weights","subclass"))),
                     by=c("rowid", "distance")) %>%
    dplyr::mutate(match = factor(ifelse(is.na(weights)==T, "Unmatched", "Matched"),
                                 levels = c("Unmatched", "Matched")))

  if(keep_unmatch == F){data_out %>%
      dplyr::filter(match =="Matched") %>%
      dplyr::select(-match)}


  if(keep_col==F){data_out <- data_out %>%
    dplyr::select(tidyselect::all_of(c("rowid", id, strata,strata_binary, explanatory)),
                  tidyselect::any_of(c("distance", "weights","subclass", "match")),
                  tidyselect::any_of(c(dependent)))}

  out <- list("object" = object, "data" = data_out)

  return(out)}
