#' exam_calculate_total
#'
#' Uses the proportions for each assessment and the
#' weightings to get the total for each student. Also
#' renormalises for exemptions as indicated by NAs.
#'
#' @param df dataframe with proportions for each piece
#' of asssessment.
#' @param weights dataframe with column with assessment names and
#' column with weights
#'
#' @return vector of total marks.
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse, targets)
#' df <- tibble(
#'   A1 = c(1, 0.5, 0),
#'   A2 = c(0, NA, 1),
#'   OQ1 = c(1, 1, NA)
#' )
#' weights <- tibble(
#'   assessment = colnames(df),
#'   weight = c(40, 40, 20)
#' )
#' weights
#' df$total <- exam_calculate_total(df, weights)
#' df
exam_calculate_total <- function(df, weights){
  # Check inputs
  stopifnot("weight" %in% colnames(weights))
  stopifnot(sum(weights$weight) == 100)
  stopifnot("assessment" %in% colnames(weights))
  stopifnot(all(weights$assessment %in% colnames(df)))
  # Set up list of totals
  n_students <- nrow(df)
  totals <- numeric(n_students)
  n_assessment <- nrow(weights)
  for(i in 1:n_students){
    total <- 0
    score <- 0
    for(j in 1:n_assessment){
      assessment <- weights$assessment[j]
      if(!is.na(df[i, assessment])){
        score <- score + as.numeric(df[i, assessment]) * weights$weight[j]
        total <- total + weights$weight[j]
      }
    }
    totals[i] <- score / total * 100
  }
  return(totals)
}
