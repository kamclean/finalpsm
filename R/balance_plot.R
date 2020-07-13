# balance_table--------------------------------
# Documentation
#' Derive formatted balance table from matchit output
#' @description Derive formatted balance table from matchit output
#' @param matchit_out Output from matchit function
#' @param type Selection for plot being produced to allow visual assessment of balance. This includes a jitter or density plot (propensity score balance overall), and covariate plot (propensity score balance for individual variables)
#' @import dplyr
#' @import MatchIt
#' @import tidyr
#' @import tibble
#' @import stringr
#' @import ggplot2
#' @import tidyselect
#' @import purrr

#' @export


balance_plot <- function(matchit_out, type = "covariate"){
  require(MatchIt);require(stringr);require(ggplot2);require(dplyr);require(tidyr);require(purrr)


  # Extract model info from matchit output
  object <- matchit_out$object

  formula_text <- summary(object)[[1]]$formula %>% deparse() %>%
    paste0(collapse = "") %>% stringr::str_squish()

  strata_binary <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,1]
  strata <- stringr::str_remove(strata_binary, "_01")
  dependent <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,2] %>%
    stringr::str_split(pattern = " \\+ ") %>% unlist()

  if(type=="jitter"){
    out <- matchit_out$data %>%
      dplyr::select(strata, distance, weights) %>%
      ggplot(aes(x = distance, y = eval(parse(text = strata)), colour = eval(parse(text = strata)))) +
      geom_jitter(aes(size = weights), alpha = 0.7) +
      xlab("Propensity score (distance)") +
      ylab("Strata") +
      labs(color = "Strata") +
      theme_bw()}

  if(type=="density"){
    out <- matchit_out$data %>%
      dplyr::select(strata, distance) %>%
      ggplot(aes(x = distance, group =eval(parse(text = strata)), colour = eval(parse(text = strata)))) +
      geom_density(alpha = 0.7) +
      xlab("Propensity score (distance)") +
      labs(color = "Strata") +
      theme_bw()}


    if(type=="covariate"){
    # Factor variable balance
    var_factor <- names(output$data[unlist(purrr::map(output$data, is.factor))])

    data_factor <- output$data %>%
      dplyr::select(all_of(c(dependent, strata)), distance) %>%
      dplyr::select_at(vars(any_of(var_factor[which(! var_factor %in% strata)]), strata, distance))

    balance_factor <- NULL
    if(ncol(data_factor)>=3){

      balance_factor <- data_factor %>%
        tidyr::pivot_longer(cols = -c(strata, distance)) %>%
        ggplot(aes(x = distance, y = value, group = eval(parse(text = strata)), color = eval(parse(text = strata)))) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "loess", se = F) +
        xlab("Propensity score (distance)") +
        ylab("") +
        theme_bw() +
        facet_wrap(~name, scales = "free_y") +
        labs(color = "Strata")}


    # Numeric variable balance
    var_numeric <- names(output$data[unlist(purrr::map(output$data, is.numeric))])

    data_numeric <- output$data %>%
      dplyr::select(all_of(c(dependent, strata)), distance) %>%
      dplyr::select_at(vars(any_of(var_numeric[which(! var_numeric %in% strata)]), strata, distance))

    balance_numeric <- NULL
    if(ncol(data_numeric)>=3){
    balance_numeric <- data_numeric %>%
      tidyr::pivot_longer(cols = -c(strata, distance)) %>%
      ggplot(aes(x = distance, y = value, group = eval(parse(text = strata)), color = eval(parse(text = strata)))) +
      geom_point(alpha = 0.2) +
      geom_smooth(method = "loess", se = F) +
      xlab("Propensity score") +
      ylab("") +
      theme_bw() +
      facet_wrap(~name, scales = "free_y") +
      labs(color = "Strata")}

    out <- list("factor" =balance_factor, "numeric"= balance_numeric)}


    return(out)}
