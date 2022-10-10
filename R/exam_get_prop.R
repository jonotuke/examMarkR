#' exam_get_prop
#'
#' @param x numeric vector
#' @param total index of total entry
#'
#' @return proportions
#' @export
#'
#' @examples
#' x <- 1:10
#' exam_get_prop(x, 10)
exam_get_prop <- function(x, total){
  x <- x / x[total]
  return(x)
}
