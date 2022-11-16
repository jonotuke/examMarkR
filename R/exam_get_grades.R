#' exam_get_grade
#'
#' convert mark to grade
#' @param mark marks
#'
#' @return grades
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse)
#' tibble(
#'   mark = 0:100
#' ) |>
#'   mutate(
#'     grade = exam_get_grade(mark)
#'   ) |>
#'   ggplot(aes(mark, grade, col = grade)) +
#'   geom_point()
exam_get_grade <- function(mark) {
    grade <- case_when(
      mark == 0 ~ "FNS",
      between(mark, 1, 44) ~ "FNA",
      between(mark, 45, 49) ~ "FA",
      between(mark, 50, 64) ~ "P",
      between(mark, 65, 74) ~ "C",
      between(mark, 75, 84) ~ "D",
      between(mark, 85, 100) ~ "HD"
    )
  grade <- factor(grade, levels = c("FNS","FNA","FA","P","C","D","HD"))
  return(grade)
}
# pacman::p_load(tidyverse)
# tibble(
#   mark = 0:100
# ) |>
#   mutate(
#     grade = exam_get_grade(mark)
#   ) |>
#   ggplot(aes(mark, grade, col = grade)) +
#   geom_point()

