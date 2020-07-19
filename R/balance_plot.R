# balance_table--------------------------------
# Documentation
#' Derive formatted balance table from matchit output
#' @description Derive formatted balance table from matchit output
#' @param matchit_out Output from matchit function
#' @param type Selection for plot being produced to allow visual assessment of balance. This includes a jitter or density plot (propensity score balance overall), and covariate plot (propensity score balance for individual variables)
#' @param threshold Selection for plot being produced to allow a threshold of balance to be set regarding the standardised mean difference (default =  0.2)
#' @import dplyr
#' @import MatchIt
#' @import tidyr
#' @import tibble
#' @import stringr
#' @import ggplot2
#' @import tidyselect
#' @import purrr
#' @export

balance_plot <- function(matchit_out, type = "jitter", threshold = 0.2){
  require(MatchIt);require(stringr);require(ggplot2);require(dplyr);require(tidyr);require(purrr)

  # Extract model info from matchit output
  object <- matchit_out$object

  formula_text <- summary(object)[[1]]$formula %>% deparse() %>%
    paste0(collapse = "") %>% stringr::str_squish()

  strata_binary <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,1]
  strata <- stringr::str_remove(strata_binary, "_01")
  dependent <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,2] %>%
    stringr::str_split(pattern = " \\+ ") %>% unlist()

  if("match" %in% names(matchit_out$data)){data_full <- matchit_out$data}
  if(! "match" %in% names(matchit_out$data)){data_full <- object$model$data %>%
    dplyr::mutate(distance = output$object$model$fitted.values) %>%
    dplyr::left_join(dplyr::select(matchit_out$data, rowid, weights), by = "rowid") %>%
    dplyr::mutate(weights = ifelse(is.na(weights)==T, 1, weights),
                  match = factor(ifelse(rowid %in% matchit_out$data$rowid, "Matched", "Unmatched"),
                                 levels = c("Unmatched", "Matched")))}

  strata_level <- pull(data_full, strata) %>% levels()

  data_final <- data_full %>%
    dplyr::mutate(strata_match = as.character(eval(parse(text = strata)))) %>%
    dplyr::mutate(strata_match = ifelse(match=="Matched", paste0(strata_match, "\n(Matched)"),
                                        paste0(strata_match, "\n(Unmatched)"))) %>%
    dplyr::mutate(strata_match = factor(strata_match,
                                        levels = c(paste0(strata_level[[1]], "\n(Unmatched)"),
                                                   paste0(strata_level[[1]], "\n(Matched)"),
                                                   paste0(strata_level[[2]], "\n(Matched)"),
                                                   paste0(strata_level[[2]], "\n(Unmatched)"))))

  unmatched <- matchit_out$object$nn %>%
    tibble::as_tibble() %>%
    tail(2) %>% unlist() %>% sum()


  if(type=="jitter"){
    if(unmatched>0){
      out <- data_final %>%
        dplyr::select(strata_match, strata, distance, weights) %>%
        dplyr::mutate(weights = ifelse(is.na(weights)==T, 1, weights)) %>%
        ggplot(aes(x = distance, y = strata_match, colour = eval(parse(text = strata)))) +
        geom_jitter(aes(size = weights), alpha = 0.7, height = .3) +
        xlab("Propensity score (distance)") +
        scale_y_discrete(name = "Strata", drop=FALSE) +
        labs(color = "Strata") +
        theme_bw()}

    if(unmatched==0){
      out <- data_final %>%
        dplyr::select(strata, distance, weights) %>%
        ggplot(aes(x = distance, y = eval(parse(text = strata)), colour = eval(parse(text = strata)))) +
        geom_jitter(aes(size = weights), alpha = 0.7, height = .3) +
        xlab("Propensity score (distance)") +
        scale_y_discrete(name = "Strata", drop=FALSE) +
        labs(color = "Strata") +
        theme_bw()}}



  if(type=="density"){
    if(unmatched>0){
    out <- data_full %>%
      dplyr::select(match, strata, distance) %>%
      ggplot(aes(x = distance, group =eval(parse(text = strata)), colour = eval(parse(text = strata)))) +
      geom_density(alpha = 0.7) +
      xlab("Propensity score (distance)") +
      labs(color = "Strata") +
      theme_bw() +
      facet_wrap(~ match, scales ="free_y")}

    if(unmatched==0){
      out <- data_full %>%
        dplyr::select(match, strata, distance) %>%
        ggplot(aes(x = distance, group =eval(parse(text = strata)), colour = eval(parse(text = strata)))) +
        geom_density(alpha = 0.7) +
        xlab("Propensity score (distance)") +
        labs(color = "Strata") +
        theme_bw()}}


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

  if(type =="love"){

    out <- finalpsm::balance_table(matchit_out = matchit_out, threshold = threshold) %>%
      dplyr::select(label, unm_smd, mat_smd) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = c("unm_smd", "mat_smd"),
                          names_to = "Sample", values_to = "SMD") %>%
      dplyr::mutate(SMD = abs(as.numeric(SMD)),
                    label = factor(label) %>% forcats::fct_rev(),
                    Sample = ifelse(Sample=="unm_smd", "Unmatched", "Matched") %>% factor() %>% forcats::fct_rev()) %>%
      dplyr::mutate(balance = ifelse(SMD>threshold, "No", "Yes")) %>%
      tidyr::pivot_wider(names_from = "Sample", values_from = c("SMD", "balance")) %>%
      ggplot() +
      aes(y = label) +
      geom_segment(aes(x = SMD_Unmatched, xend = SMD_Matched, yend = label, colour = balance_Matched), arrow = arrow(type = "closed")) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = threshold, linetype = "dashed") +
      scale_colour_manual(name = "Balance", values = list("Yes" = "green","No" = "red")) +

      scale_x_continuous(name = "Absolute Standardised Mean Difference (SMD)") +
      scale_y_discrete(name = "Covariate") +
      theme_bw()}


    return(out)}
