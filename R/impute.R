# impute
# Documentation
#' Wrapper for the mice package
#' @description Impute missing at random data using the mice package in a tidyverse approach.
#' @param data Dataframe
#' @param impute Vector of column names containing variables for use in the imputation algorithm and for imputation.
#' @param fixed Optional vector of column names containing variables for use in the imputation algorithm but NOT for imputation.
#' @param ignore Optional vector of column names containing variables NOT for use in the imputation algorithm OR for imputation.
#' @param quiet Logical value to hide the output of the imputation process (default = T)
#' @param m The number of imputations to be performed (default = 5)
#' @param method The imputation method to be used for each column in data (see mice::mice() function)
#' @param maxit is the number of iterations for each imputation  (see mice::mice() function).
#' @param seed random seed parameter which is useful for reproducibility.
#' @param ... Additional arguments to be passed to the mice::mice() function.
#' @return Long dataset containing the original (unimputed) data, with each imputed dataset appended.
#' @import dplyr
#' @import mice
#' @import tidyr
#' @importFrom purrr map_df
#' @importFrom tibble rowid_to_column
#' @export

impute <- function(data, impute, fixed = NULL, ignore = NULL, quiet = T, m=5, maxit=5, meth='pmm', seed=500, ...){

  data <- data %>% dplyr::select(all_of(c(impute, fixed, ignore)))

  if(quiet==T){

    hush=function(code){
      sink("/dev/null")
      tmp = code
      sink()
      return(tmp)}


    mids <- hush(mice::mice(data = data %>% dplyr::select(-all_of(ignore)),
                            m = m, maxit=maxit,meth=meth,seed=seed, ...))}else{

                              mids <- mice::mice(data = data %>% dplyr::select(-all_of(ignore)),
                                                 m = m, maxit=maxit,meth=meth,seed=seed, ...)}


  impdata <- purrr::map_df(1:m,
                           function(x){mice::complete(mids, x) %>%
                               dplyr::mutate(m = x) %>%
                               dplyr::select(m, everything())}) %>%
    dplyr::bind_rows(mids$data %>% dplyr::mutate(m=0)) %>%
    dplyr::mutate(imputed = ifelse(m == 0, "original", "imputed")) %>%
    dplyr::select(-all_of(fixed)) %>%
    dplyr::bind_cols(data %>%
                       tibble::rowid_to_column() %>%
                       dplyr::select(rowid, all_of(fixed), all_of(ignore)) %>%
                       dplyr::slice(rep(1:n(), m+1))) %>%
    dplyr::select(imputed, m, rowid, names(data)) %>%
    dplyr::arrange(m)

  return(impdata)}
