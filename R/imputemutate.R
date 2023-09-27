# imputemutate
# Documentation
#' Wrapper for the mutate function to work across imputed datasets
#' @description Modify imputed data across all datasets at once
#' @param data Output from impute function
#' @param ... Mutations to perform on across all imputed datasets
#' @return Same output from impute function with mutations applied
#' @import dplyr
#' @importFrom purrr map
#' @export

imputemutate <- function(.data, ...){
  .data %>%
    dplyr::mutate(data = purrr::map(data,
                                    function(x){x %>% dplyr::mutate(...)}))}