#' Takes column of grades and gives summary
#'
#' @param grades vector of grades
#'
#' @return table of counts and prop.
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse)
#' grades <- c("FNS", "FNA", "FA", "P", "C", "D", "HD")
#' df <- tibble(
#'   grades = sample(grades, 100, replace = TRUE)
#' )
#' df |> exam_grade_summary(grades)
exam_grade_summary  <- function(df, grade_col){
  GS  <-
    df %>%
    count(grade = {{grade_col}}, .drop = FALSE) %>%
    mutate(
      N = sum(n),
      Prop = round(n / N, 2))
  return(GS)
}
# pacman::p_load(tidyverse)
# grades <- c("FNS", "FNA", "FA", "P", "C", "D", "HD")
# df <- tibble(
#   grades = sample(grades, 100, replace = TRUE)
# )
# df |> exam_grade_summary(grades)
