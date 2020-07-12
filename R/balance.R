# balance--------------------------------
# Documentation
#' Derive formatted balance table from matchit output
#' @description Derive formatted balance table from matchit output
#' @param matchit_out Output from matchit function
#' @param threshold_smd Threshold below which the absolute standardised mean difference is considered balanced (default = 0.2). If formal threshold is required, this should be set to NULL.
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
#' @importFrom stddiff stddiff.category
#' @export

balance <- function(matchit_out, threshold_smd = 0.2){
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
    dplyr::select(any_of(c(dependent,strata,strata_binary, "distance", "weights", "subclass"))) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(strata_binary = dplyr::pull(., strata_binary))

  data_con <- dplyr::filter(data, strata_binary=="0")
  data_trt <- dplyr::filter(data, strata_binary=="1")

  matchit_summary <- summary(object, standardize = TRUE)
  # Unmatched data balance
  unm_data <- matchit_summary$sum.all %>%
    tibble::rownames_to_column(var = "lab_lvl") %>%
    tibble::as_tibble() %>%
    dplyr::filter(lab_lvl!="distance") %>%
    dplyr::select(lab_lvl,
                  unm_treated = "Means Treated",
                  unm_control = "Means Control",
                  unm_smd = "Std. Mean Diff.") %>%
    dplyr::mutate(unm_smd = abs(unm_smd)) %>%
    dplyr::select(lab_lvl, unm_treated, unm_control, unm_smd)

  mat_data <- matchit_summary$sum.matched %>%
    tibble::rownames_to_column(var = "lab_lvl") %>%
    tibble::as_tibble() %>%
    dplyr::filter(lab_lvl!="distance") %>%
    dplyr::select(lab_lvl,
                  mat_treated = "Means Treated",
                  mat_control = "Means Control",
                  mat_smd = "Std. Mean Diff.") %>%
    dplyr::mutate(mat_smd = abs(mat_smd)) %>%
    dplyr::select(lab_lvl, mat_treated, mat_control, mat_smd)

  # Get labels and levels for all variables
  metadata <- data %>%
    dplyr::select(names(.)[!names(.) %in% c("distance", "weights", "subclass")]) %>%
    purrr::map2(.x = ., .y = names(.),
                function(.x, .y){tibble::tibble("label" = .y, "class" = class(.x)) %>%
                    dplyr::mutate(level = ifelse(is.null(levels(.x))==T, NA, paste(levels(.x), collapse = ", "))) %>%
                    tidyr::separate_rows(level, sep = ", ") %>%
                    dplyr::mutate(lab_lvl = ifelse(class =="factor", paste0(label, level), label))}) %>%
    dplyr::bind_rows()

  tab_bal <- unm_data %>%
    dplyr::left_join(mat_data, by="lab_lvl") %>%
    dplyr::left_join(metadata, by="lab_lvl") %>%
    dplyr::select(lab_lvl, label, level, class, everything()) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_smd")), function(x){format(signif(x, 3),nsmall=3)})

  # Calculate numeric vaiable balance stats-------------
  # Numeric data
  tab_bal_num_final <- NULL
if(nrow(tab_bal %>% dplyr::filter(class == "numeric"))>0){
  data_con_num <- data_con %>%
    dplyr::select(tidyselect::all_of(tab_bal %>% dplyr::filter(class == "numeric") %>% dplyr::pull(label)), weights) %>%
    dplyr::summarise_all(function(x){stringr::str_split(paste(x, collapse = ", "), ", ")}) %>%
    tidyr::pivot_longer(cols= -"weights", names_to = "label") %>%
    dplyr::select(label, "data_control" = value, "weights_control" = weights)

  data_trt_num <- data_trt %>%
    dplyr::select(tidyselect::all_of(tab_bal %>% dplyr::filter(class == "numeric") %>% dplyr::pull(label)), weights) %>%
    dplyr::summarise_all(function(x){stringr::str_split(paste(x, collapse = ", "), ", ")}) %>%
    tidyr::pivot_longer(cols= - weights, names_to = "label") %>%
    dplyr::mutate(status = "treated", class = "numeric") %>%
    dplyr::select(label, "data_treated" = value, "weights_treated" = weights)

  tab_bal_num <- tab_bal %>%
    dplyr::filter(class == "numeric") %>%
    dplyr::left_join(data_con_num, by = "label") %>%
    dplyr::left_join(data_trt_num, by = "label")

  # Numeric balance data
  tab_bal_num_control <- tab_bal_num %>%
    dplyr::select(label, data_control,weights_control) %>%
    tidyr::unnest(cols = c(data_control, weights_control)) %>%
    dplyr::mutate_at(vars(ends_with("_control")), as.numeric) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(unm_control_mean = mean(data_control, na.rm=T),
                     unm_control_sd = sd(data_control, na.rm=T),
                     mat_control_mean = Hmisc::wtd.mean(data_control, weights_control),
                     mat_control_sd = sqrt(Hmisc::wtd.var(data_control, weights_control)))

  tab_bal_num_treated <- tab_bal_num %>%
    dplyr::select(label, data_treated,weights_treated) %>%
    tidyr::unnest(cols = c(data_treated, weights_treated)) %>%
    dplyr::mutate_at(vars(ends_with("_treated")), as.numeric) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(unm_treated_mean = mean(data_treated, na.rm=T),
                     unm_treated_sd = sd(data_treated, na.rm=T),
                     mat_treated_mean = Hmisc::wtd.mean(data_treated, weights_treated),
                     mat_treated_sd = sqrt(Hmisc::wtd.var(data_treated, weights_treated)))


  tab_bal_num_final <- tab_bal_num %>%
    dplyr::left_join(tab_bal_num_treated, by="label") %>%
    dplyr::left_join(tab_bal_num_control, by="label") %>%
    dplyr::mutate_at(vars(contains("_control_")), function(x){signif(as.numeric(x), digits=3)}) %>%
    dplyr::mutate_at(vars(contains("_treated_")), function(x){signif(as.numeric(x), digits=3)}) %>%
    dplyr::mutate(unm_control = paste0(unm_control_mean, " (", unm_control_sd, ")"),
                  unm_treated = paste0(unm_treated_mean, " (", unm_treated_sd, ")"),
                  mat_control = paste0(mat_control_mean, " (", mat_control_sd, ")"),
                  mat_treated = paste0(mat_treated_mean, " (", mat_treated_sd, ")")) %>%
    dplyr::select(lab_lvl:mat_smd) %>%
    dplyr::mutate_all(as.character)}


  # Calculate factor vaiable balance stats-------------------
  # Calculate numbers
  out_n <- as.data.frame(object$nn) %>%
    tibble::rownames_to_column(var = "outcome") %>%
    tibble::as_tibble()

  n_total <- out_n %>%
    dplyr::filter(outcome %in% c("All", "Matched")) %>%
    dplyr::mutate(outcome = c("unm", "mat")) %>%
    tidyr::pivot_wider(names_from = "outcome", values_from = c(Control, Treated)) %>%
    dplyr::mutate_all(as.numeric) %>%
    magrittr::set_colnames(c("unm_control_total","mat_control_total","unm_treated_total","mat_treated_total"))

  # Factor data
  data_fac <- tab_bal %>%
    dplyr::filter(class == "factor") %>%
    dplyr::bind_cols(dplyr::bind_rows(purrr::map(n_total,function(x){rep(x, nrow(.))}))) %>%
    dplyr::full_join(dplyr::filter(metadata, class=="factor"),
                     by=c("lab_lvl", "label", "level", "class")) %>%
    dplyr::mutate_at(vars(ends_with("_total")), zoo::na.locf) %>%
    dplyr::arrange(lab_lvl, label, level) %>%
    dplyr::select(-lab_lvl, -class, -unm_smd, -mat_smd) %>%
    dplyr::rename("unm_treated_prop" = unm_treated,"unm_control_prop" = unm_control,
                  "mat_treated_prop" = mat_treated,"mat_control_prop" = mat_control) %>%
    dplyr::group_split(label) %>%
    purrr::map(function(x){x %>%
        dplyr::mutate_at(vars(ends_with("_prop")), function(a){ifelse(is.na(a)==T,1-sum(a, na.rm=T),a)}) %>%

        dplyr::mutate(unm_control_n = round(unm_control_prop*unm_control_total, 0),
                      unm_treated_n = round(unm_treated_prop*unm_treated_total, 0),
                      mat_treated_n = round(mat_treated_prop*mat_treated_total, 0),
                      mat_control_n = round(mat_control_prop*mat_control_total, 0)) %>%

        dplyr::mutate(unm_treated = paste0(unm_treated_n, " (",
                                           scales::percent(unm_treated_prop, accuracy = 0.1,  suffix = ""), ")"),
                      unm_control = paste0(unm_control_n, " (",
                                           scales::percent(unm_control_prop, accuracy = 0.1,  suffix = ""), ")"),
                      mat_treated = paste0(mat_treated_n, " (",
                                           scales::percent(mat_treated_prop, accuracy = 0.1,  suffix = ""), ")"),
                      mat_control = paste0(mat_control_n, " (",
                                           scales::percent(mat_control_prop, accuracy = 0.1, suffix = ""), ")"))})

  # Balance stats
  fac_unm_smd <- data_fac %>%
    purrr::map(function(x){x %>%
        dplyr::mutate(lab_lvl = paste0(label, "___", level)) %$%
        dplyr::bind_cols("data" = c(rep(lab_lvl, unm_treated_n),
                                    rep(lab_lvl, unm_control_n)),
                         "strata" = c(rep(1, sum(unm_treated_n)),
                                      rep(0, sum(unm_control_n)))) %$%
        stddiff::stddiff.category(data = as.data.frame(.),
                                  gcol = "strata",
                                  vcol = "data") %>%
        as.data.frame() %>%
        tibble::rownames_to_column("lab_lvl") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(lab_lvl = gsub("NA ", "", lab_lvl)) %>%
        dplyr::mutate(label = str_split_fixed(lab_lvl, "___", 2)[,1],
                      level = str_split_fixed(lab_lvl, "___", 2)[,2]) %>%
        zoo::na.locf() %>%
        dplyr::select(label, level, "unm_smd" = stddiff) %>%
        dplyr::mutate(unm_smd = format(round(unm_smd, 3), nsmall=3))}) %>%
    dplyr::bind_rows()

  fac_mat_smd <- data_fac %>%
    purrr::map(function(x){x %>%
        dplyr::mutate(lab_lvl = paste0(label, "___", level)) %$%
        dplyr::bind_cols("data" = c(rep(lab_lvl, mat_treated_n),
                                    rep(lab_lvl, mat_control_n)),
                         "strata" = c(rep(1, sum(mat_treated_n)),
                                      rep(0, sum(mat_control_n)))) %$%
        stddiff::stddiff.category(data = as.data.frame(.),
                                  gcol = "strata",
                                  vcol = "data") %>%
        as.data.frame() %>%
        tibble::rownames_to_column("lab_lvl") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(lab_lvl = gsub("NA ", "", lab_lvl)) %>%
        dplyr::mutate(label = str_split_fixed(lab_lvl, "___", 2)[,1],
                      level = str_split_fixed(lab_lvl, "___", 2)[,2]) %>%
        zoo::na.locf() %>%
        dplyr::select(label, level, "mat_smd" = stddiff) %>%
        dplyr::mutate(mat_smd = format(round(mat_smd, 3), nsmall=3))}) %>%
    dplyr::bind_rows()

  tab_bal_fac <- dplyr::bind_rows(data_fac) %>%
    dplyr::left_join(fac_unm_smd, by=c("label", "level")) %>%
    dplyr::left_join(fac_mat_smd, by=c("label", "level")) %>%
    dplyr::mutate(lab_lvl = paste0(label, level),
                  class = "factor") %>%
    dplyr::select(lab_lvl, label, level, class, unm_treated, unm_control, unm_smd,
                  mat_treated, mat_control, mat_smd)

  # Create final table-------------------
  tab_bal_final <- dplyr::bind_rows(tab_bal_num_final, tab_bal_fac) %>%
    dplyr::arrange(match(lab_lvl, metadata$lab_lvl)) %>%
    dplyr::mutate(level = ifelse(class=="numeric", "Mean (SD)", level)) %>%
    dplyr::filter(! label %in% c("strata_binary", strata)) %>%
    dplyr::select(-lab_lvl, -class)

  if(is.null(threshold_smd)==F){
    if(is.numeric(threshold_smd)==T){

    tab_bal_final <- tab_bal_final %>%
    dplyr::mutate(unm_balance = ifelse(as.numeric(unm_smd)<threshold_smd, "Yes", "No"),
                  mat_balance = ifelse(as.numeric(mat_smd)<threshold_smd, "Yes", "No")) %>%
    dplyr::select(label, level,
                  unm_treated, unm_control, unm_smd, unm_balance,
                  mat_treated, mat_control, mat_smd, mat_balance)}}

  return(tab_bal_final)}
