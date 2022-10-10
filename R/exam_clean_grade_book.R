#' exam_clean_grade_book
#'
#' @param df a data frame from MyUni gradebook
#' @param cols tidyselect columns to clean
#'
#' @return cleaned dataframe
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse, targets, examMarkR)
#' df <- tibble(
#'   ID = 1:3,
#'   A1 = c(12, 12, NA),
#'   A2 = c("EX", 12, NA),
#'   test1 = c(12, 12, "EX")
#' )
#' df1 <- df |> exam_clean_grade_book(A1)
#' df1
#' df2 <- df |> exam_clean_grade_book(A1:A2)
#' df2
#' df3 <- df |> exam_clean_grade_book(starts_with("A"))
#' df3
exam_clean_grade_book <- function(df, cols){
  df |>
    mutate(
      across({{cols}}, exam_clean_marks)
    )
}
