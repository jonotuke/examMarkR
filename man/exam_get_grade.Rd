% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exam_get_grades.R
\name{exam_get_grade}
\alias{exam_get_grade}
\title{exam_get_grade}
\usage{
exam_get_grade(mark)
}
\arguments{
\item{mark}{marks}
}
\value{
grades
}
\description{
convert mark to grade
}
\examples{
pacman::p_load(tidyverse)
tibble(
  mark = 0:100
) |>
  mutate(
    grade = exam_get_grade(mark)
  ) |>
  ggplot(aes(mark, grade, col = grade)) +
  geom_point()
}
