#' Converts XLSX to CSV
#'
#' @param XLSX filename of excel
#'
#' @return filename of CSV
#' @export
#'
#' @examples
#' exam_xlsx_csv("inst/extdata/example-grade-roster.xlsx")
exam_xlsx_csv <- function(XLSX){
  CSV <- str_replace(XLSX, "xlsx", "csv")
  XLSX |>
    readxl::read_excel(col_names = FALSE) |>
    write_csv(CSV,na = "",col_names = FALSE)
  return(CSV)
}
# pacman::p_load(tidyverse, targets)
# exam_xlsx_csv("inst/extdata/example-grade-roster.xlsx")
