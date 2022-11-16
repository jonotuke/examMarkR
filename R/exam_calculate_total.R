#' exam_calculate_total
#'
#' calculates the weighted mark
#'
#' @param ID student ID
#' @param df marks dataframe
#' @param weights tibble with assessment and weight
#'
#' @return tibble with ID and mark
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse, targets)
#'df <- tibble(
#'  ID = LETTERS[1:3],
#'  A1 = c(1, 0.5, 0),
#'  A2 = c(0, NA, 1),
#'  OQ1 = c(1, 1, NA)
#')
#'df
#'weights_main <- tibble(
#'  assessment = colnames(df)[-1],
#'  weight = c(40, 40, 20)
#')
#'weights_C <- tibble(
#'  assessment = colnames(df)[-1],
#'  weight = c(50, 50, 0)
#')
#'marks <- LETTERS[1:2] |>
#'  map_dfr(exam_calculate_total, df, weights_main) |>
#'  bind_rows(
#'    exam_calculate_total("C", df, weights)
#'  )
#'marks
#'df |> left_join(marks, "ID")
exam_calculate_total <- function(ID, df, weights){
  # Check inputs
  stopifnot("weight" %in% colnames(weights))
  stopifnot(sum(weights$weight) == 100)
  stopifnot("assessment" %in% colnames(weights))
  stopifnot(all(weights$assessment %in% colnames(df)))
  stopifnot("ID" %in% colnames(df))
  # Set the mark and total to zero
  total <- 0
  mark <- 0
  # Get number of assessments
  n_assessment <- nrow(weights)
  # Find the row for the given student
  i <- which(df$ID == ID)
  # For each assessment
  for(j in 1:n_assessment){
    assessment <- weights$assessment[j]
    if(!is.na(df[i, assessment])){
      # Calculate mark and total
      mark <- mark + as.numeric(df[i, assessment]) * weights$weight[j]
      total <- total + weights$weight[j]
    }
  }
  # adjust mark if missing due to EX
  mark <- mark / total * 100
  return(tibble(ID, mark))
}
# pacman::p_load(tidyverse, targets)
# df <- tibble(
#   ID = LETTERS[1:3],
#   A1 = c(1, 0.5, 0),
#   A2 = c(0, NA, 1),
#   OQ1 = c(1, 1, NA)
# )
# df
# weights_main <- tibble(
#   assessment = colnames(df)[-1],
#   weight = c(40, 40, 20)
# )
# weights_C <- tibble(
#   assessment = colnames(df)[-1],
#   weight = c(50, 50, 0)
# )
# marks <- LETTERS[1:2] |>
#   map_dfr(exam_calculate_total, df, weights_main) |>
#   bind_rows(
#     exam_calculate_total("C", df, weights)
#   )
# marks
# df |> left_join(marks, "ID")
