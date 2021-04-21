# balance_table--------------------------------
# Documentation
#' Derive formatted balance table from matchit output
#' @description Derive formatted balance table from matchit output
#' @param matchit_out Output from matchit function
#' @param threshold Threshold below which the absolute standardised mean difference is considered balanced (default = 0.2). If formal threshold is required, this should be set to NULL.
#' @param p Test for significant differences between treatment groups to assess balance before and after matching (default = FALSE)
#' @import dplyr
#' @import magrittr
#' @import tidyr
#' @import tibble
#' @import purrr
#' @import tidyselect
#' @importFrom MatchIt match.data
#' @importFrom stringr str_split
#' @importFrom Hmisc wtd.mean wtd.var
#' @importFrom scales percent
#' @importFrom zoo na.locf
#' @importFrom stddiff stddiff.category stddiff.numeric
#' @export

balance_table <- function(matchit_out, threshold = 0.2, p=FALSE){
  require(dplyr); require(tidyr); require(purrr); require(tibble);
  require(magrittr); require(MatchIt); require(stringr);require(tidyselect)
  require(Hmisc); require(scales); require(stddiff); require(zoo)

  object <- matchit_out$object

  # Extract model info from matchit output
  formula_text <- summary(object)[[1]]$formula %>% deparse() %>%
    paste0(collapse = "") %>% stringr::str_squish()

  strata_binary <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,1]
  strata <- stringr::str_remove(strata_binary, "_01")
  dependent <- stringr::str_split_fixed(formula_text, " ~ ", 2)[,2] %>%
    stringr::str_split(pattern = " \\+ ") %>% unlist()

  # Get matched dataset
  data <- matchit_out$data %>%
    dplyr::mutate(strata_01 = dplyr::pull(., strata_binary))

  # Numeric balance data---------------------
  tab_bal_num <- NULL
  var_numeric <- data %>%
    dplyr::select(all_of(dependent)) %>%
    dplyr::select_if(is.numeric) %>%
    names()

  if(length(var_numeric)>=1){
    data_num <- data %>%
      dplyr::select(all_of(c(all_of(var_numeric), "weights", "strata_01"))) %>%
      dplyr::group_split(strata_01) %>%
      purrr::map_df(function(x){x %>%
          dplyr::group_by(strata_01) %>%
          dplyr::summarise(across(c(all_of(var_numeric), weights), function(x){list(x)}), .groups = "drop") %>%
          tidyr::pivot_longer(cols= - c("weights","strata_01"), names_to = "label", values_to = "data") %>%
          dplyr::select(strata_01, label, data, weights) %>%
          tidyr::unnest(cols = c(data, weights)) %>%
          dplyr::mutate_at(vars(data, weights), as.numeric)}) %>%
      dplyr::mutate(data_weighted = data*weights)

    test_num <- data_num %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(unm_p = t.test(data ~ strata_01) %>% broom::tidy() %>% pull(p.value),
                       mat_p = t.test(data_weighted ~ strata_01) %>% broom::tidy() %>% pull(p.value), .groups = "drop")

    smd_num <- data_num %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(unm_smd = stddiff::stddiff.numeric(as.data.frame(.), gcol = "strata_01", vcol = "data") %>%
                         tibble::as_tibble() %>% dplyr::pull(stddiff),
                       mat_smd = stddiff::stddiff.numeric(as.data.frame(.), gcol = "strata_01", vcol = "data_weighted") %>%
                         tibble::as_tibble() %>% dplyr::pull(stddiff), .groups = "drop")

    sum_num <- data_num %>%
      dplyr::group_by(strata_01, label) %>%
      dplyr::mutate(data_weighted = data*weights) %>%
      dplyr::summarise(unm_mean = mean(data, na.rm=T),
                       unm_sd = sd(data, na.rm=T),
                       mat_mean = Hmisc::wtd.mean(data, weights),
                       mat_sd = sqrt(Hmisc::wtd.var(data, weights)), .groups = "drop") %>%
      dplyr::filter(! label %in% c(strata_binary, strata, "strata_01")) %>%
      dplyr::mutate_at(vars(-label, -strata_01), function(x){signif(as.numeric(x), digits=3)}) %>%
      dplyr::mutate(unm = paste0(unm_mean, " (", unm_sd, ")"),
                    mat = paste0(mat_mean, " (", mat_sd, ")")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(level = "(SD)",
                    strata_01 = ifelse(strata_01==0, "con", "trt")) %>%
      dplyr::select(strata_01, label,level, unm, mat) %>%
      tidyr::pivot_wider(names_from = "strata_01", values_from = c("unm", "mat"))

    tab_bal_num <- sum_num %>%
      dplyr::left_join(smd_num, by = "label") %>%
      dplyr::left_join(test_num, by = "label") %>%
      dplyr::select(label, level,
                    unm_con, unm_trt,unm_smd, unm_p,
                    mat_con, mat_trt, mat_smd, mat_p)}

  # Factor balance data-------------------
  tab_bal_fac <- NULL

  var_factor <- data %>%
    dplyr::select(all_of(dependent)) %>%
    dplyr::select_if(is.factor) %>%
    names()

  if(length(var_factor)>=1){

    # Determine the weighted

    sum_fac <- data %>%
      dplyr::select_at(vars(c("strata_01", all_of(var_factor), "weights"))) %>%
      dplyr::group_split(strata_01) %>%
      purrr::map_df(function(x){x %>%
          dplyr::group_by(strata_01) %>%
          dplyr::summarise(across(c(all_of(var_factor), weights), function(x){list(x)}), .groups = "drop") %>%
          tidyr::pivot_longer(cols= -all_of(c("weights", "strata_01")), names_to = "label", values_to = "data") %>%
          dplyr::select(strata_01, label, data, weights) %>%
          tidyr::unnest(cols = everything()) %>%
          dplyr::group_by(strata_01, label, data) %>%
          dplyr::summarise(.groups = "drop",
                           n_unm = n(),
                           n_mat = sum(weights, na.rm = T) %>% round())})

    data_fac <- sum_fac %>%
      dplyr::group_by(strata_01, label,data ) %>%
      dplyr::mutate(unm = map(n_unm, function(x){rep(data, x)}),
                    mat = map(n_mat, function(x){rep(data, x)})) %>%
      dplyr::ungroup() %>%
      dplyr::select(-data, -n_mat, -n_unm) %>%
      tidyr::pivot_longer(cols = c("unm", "mat"), names_to = "match", values_to = "data") %>%
      tidyr::unnest(cols = "data") %>%
      dplyr::mutate(label_level = paste0(label, "___", data))

    smd_fac_unmatch <- data_fac %>%
      dplyr::filter(match == "unm") %>%
      dplyr::group_split(label) %>%
      purrr::map_df(function(x){stddiff::stddiff.category(data = as.data.frame(x),
                                                          gcol = "strata_01",
                                                          vcol = "label_level") %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "label_level")}) %>%
      dplyr::mutate(label_level = stringr::str_sub(label_level, 4, nchar(label_level))) %>%
      tidyr::separate(col = "label_level", into = c("label", "level"), sep = "___") %>%
      dplyr::select(label:level,"prop_unm_con" = p.c,"prop_unm_trt" = p.t, "unm_smd" = stddiff)  %>%
      dplyr::group_by(label) %>%
      tidyr::fill(unm_smd, .direction = "downup") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = as.character(label),
                    level = as.character(level))


    smd_fac_match <- data_fac %>%
      dplyr::filter(match == "mat") %>%
      dplyr::group_split(label) %>%
      purrr::map_df(function(x){stddiff::stddiff.category(data = as.data.frame(x),
                                                          gcol = "strata_01",
                                                          vcol = "label_level") %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "label_level")}) %>%
      dplyr::mutate(label_level = stringr::str_sub(label_level, 4, nchar(label_level))) %>%
      tidyr::separate(col = "label_level", into = c("label", "level"), sep = "___") %>%
      dplyr::select(label:level,"prop_mat_con" = p.c,"prop_mat_trt" = p.t, "mat_smd" = stddiff) %>%
      dplyr::group_by(label) %>%
      tidyr::fill(mat_smd, .direction = "downup") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = as.character(label),
                    level = as.character(level))

    test_fac <- sum_fac %>%
      tidyr::pivot_longer(cols = c("n_unm", "n_mat"), names_to = "match", values_to = "n") %>%
      tidyr::pivot_wider(names_from = c("strata_01"), values_from = "n") %>%
      dplyr::mutate(across(c(`0`, `1`), function(x){ifelse(is.na(x)==T, 0, x)})) %>%
      dplyr::select(-data) %>%
      dplyr::group_split(label, match) %>%

      purrr::map_df(function(x){

        info <- x %>% dplyr::select(match, label) %>% dplyr::distinct()

        p <- tryCatch(x %>% dplyr::select(`0`, `1`) %>% chisq.test(),
                      warning=function(w){return(x %>% dplyr::select(`0`, `1`) %>% fisher.test(workspace=2e+07,hybrid=TRUE))})  %>%
          broom::tidy() %>%
          dplyr::select("p" = p.value)


        return(out = dplyr::bind_cols(info, p))}) %>%
      dplyr::mutate(match = stringr::str_remove(match, "n_")) %>%
      tidyr::pivot_wider(names_from = c("match"), values_from = "p") %>%
      dplyr::select(label, "unm_p" = unm,"mat_p" = mat)


    tab_bal_fac <- sum_fac %>%
      dplyr::rename("level" = data) %>%
      dplyr::mutate(strata_01 = ifelse(strata_01==0, "con", "trt")) %>%
      tidyr::pivot_wider(names_from = "strata_01",
                         values_from = c("n_unm", "n_mat")) %>%
      dplyr::mutate(level = as.character(level)) %>%
      dplyr::left_join(smd_fac_unmatch, by = c("label", "level")) %>%
      dplyr::left_join(smd_fac_match, by = c("label", "level")) %>%
      dplyr::left_join(test_fac, by = c("label")) %>%
      dplyr::mutate(unm_trt = paste0(n_unm_trt, " (",
                                     scales::percent(prop_unm_trt, accuracy = 0.1,  suffix = ""), ")"),
                    unm_con = paste0(n_unm_con, " (",
                                     scales::percent(prop_unm_con, accuracy = 0.1,  suffix = ""), ")"),
                    mat_trt = paste0(n_mat_trt, " (",
                                     scales::percent(prop_mat_trt, accuracy = 0.1,  suffix = ""), ")"),
                    mat_con = paste0(n_mat_con, " (",
                                     scales::percent(prop_mat_con, accuracy = 0.1, suffix = ""), ")")) %>%
      dplyr::select(label, level,
                    unm_con, unm_trt,unm_smd,unm_p,
                    mat_con, mat_trt,mat_smd,mat_p)}

  # Create final table-------------------
  tab_bal_final <- dplyr::bind_rows(tab_bal_fac, tab_bal_num) %>%
    dplyr::mutate(label = factor(label, levels=dependent),
                  mat_p = ifelse(mat_p<0.001, "<0.001", format(round(mat_p, 3), nsmall=3)),
                  unm_p = ifelse(unm_p<0.001, "<0.001", format(round(unm_p, 3), nsmall=3))) %>%
    dplyr::arrange(label) %>%
    dplyr::mutate(across(c("unm_con", "unm_trt", "mat_con", "mat_trt"),
                         function(x){stringr::str_replace_all(x, "NA \\(0.0\\)|0 \\(NA\\)|NA \\(NA\\)", "0 (0.0)")})) %>%
    dplyr::group_by(label) %>%
    tidyr::fill(mat_smd, .direction = "downup") %>%
    dplyr::ungroup()

  if(is.null(threshold)==F){
    if(is.numeric(threshold)==T){

      tab_bal_final <- tab_bal_final %>%
        dplyr::mutate(unm_balance = ifelse(abs(as.numeric(unm_smd))<threshold, "Yes", "No"),
                      mat_balance = ifelse(abs(as.numeric(mat_smd))<threshold, "Yes", "No")) %>%
        dplyr::select(label, level,
                      unm_con, unm_trt,unm_smd,unm_p,unm_balance,
                      mat_con, mat_trt,mat_smd,mat_p, mat_balance) %>%
        dplyr::group_by(label) %>%
        dplyr::mutate(n = 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at(vars(label, unm_smd, mat_smd, unm_p, mat_p, unm_balance, mat_balance), as.character) %>%
        dplyr::mutate(label =  ifelse(n==1, label, ""),
                      unm_smd = ifelse(n==1, unm_smd, ""),
                      mat_smd = ifelse(n==1, mat_smd, ""),
                      unm_p = ifelse(n==1, unm_p, ""),
                      mat_p = ifelse(n==1, mat_p, ""),
                      unm_balance = ifelse(n==1, unm_balance, ""),
                      mat_balance = ifelse(n==1, mat_balance, "")) %>%
        dplyr::select(-n)}}



  if(p==F){tab_bal_final <- tab_bal_final %>% dplyr::select(-mat_p,-unm_p)}


  return(tab_bal_final)}
