#' Get grade plot
#'
#' @param df data frame of marks
#' @param grade_col column with grades
#'
#' @return ggplot
#' @export
#'
#' @examples
#' # pacman::p_load(tidyverse)
#' grades <- c("FNS", "FNA", "FA", "P", "C", "D", "HD")
#' df <- tibble(
#'   grades = sample(grades, 100, replace = TRUE)
#' )
#' df |> exam_grade_summary(grades)
#' df |> exam_plot_grades(grades)
exam_plot_grades  <- function(df, grade_col){
  p  <-
    df %>%
    exam_grade_summary({{grade_col}}) %>%
    ggplot(aes(grade, Prop, fill = grade)) +
    geom_bar(stat = "identity", show.legend = FALSE, col = "black") +
    geom_text(aes(label = glue::glue("{Prop}({n})")), vjust = -1, col = "black", size = 5) +
    scale_fill_brewer(palette = "Set1") +
    labs(x = "Grade", y = "Proportion") +
    scale_x_discrete(breaks = c("FNS", "FNA", "FA", "P", "C", "D", "HD"))
  return(p)
}
# pacman::p_load(tidyverse)
# grades <- c("FNS", "FNA", "FA", "P", "C", "D", "HD")
# df <- tibble(
#   grades = sample(grades, 100, replace = TRUE)
# )
# df |> exam_grade_summary(grades)
# df |> exam_plot_grades(grades)
