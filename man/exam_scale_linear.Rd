% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exam_scale_linear.R
\name{exam_scale_linear}
\alias{exam_scale_linear}
\title{Linear interpolation norm}
\usage{
exam_scale_linear(x, boundary_pts)
}
\arguments{
\item{x}{marks}

\item{boundary_pts}{gives mapping from raw to norm. Always add (0,0) and (100, 100)}
}
\value{
norm marks
}
\description{
Linear interpolation norm
}
\examples{
boundary_pts  <- tribble(
  ~x, ~y,
  25, 50,
  75, 90
)
df  <- tibble(
  x = 0:100
) \%>\%
  mutate(
    y = exam_scale_linear(x, boundary_pts = boundary_pts)
  )
df \%>\% ggplot(aes(x, y)) + geom_point()
}
