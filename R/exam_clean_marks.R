#' exam_clean_marks
#'
#' converts missing to 0, and EX to missing
#'
#' @param x column from MyUni gradebook that contains marks
#'
#' @return cleaned column
#' @export
#'
#' @examples
exam_clean_marks  <- function(x){
  x[is.na(x)]  <- 0
  x[x=="EX"]  <- NA
  x  <- as.numeric(x)
  return(x)
}
# x <- c(0, "EX", NA)
# exam_clean_marks(x)
