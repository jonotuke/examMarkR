#' exam_convert_prop
#'
#' given a dataframe, some columns and a total row, converts
#' to proportions
#'
#' @param df a dataframe with totals in one row
#' @param cols tidyselection of columns
#' @param row number of row with totals
#'
#' @return dataframe with proportions
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse, targets)
#' df <- tibble(
#'   A1 = c(10, 1, 2),
#'   A2 = c(10, 2, 1)
#' )
#' df |> exam_convert_prop(A1, 1)
#' df |> exam_convert_prop(A1:A2, 1)
exam_convert_prop <- function(df, cols, row){
  df |>
    mutate(
      across({{cols}}, exam_get_prop, row)
    )
}
