#' Converts CSV to XLSX
#'
#' @param CSV filename of CSV
#'
#' @return filename of XLSX
#' @export
#'
#' @examples
#' exam_csv_xlsx("inst/extdata/example-grade-roster-filled.csv")
exam_csv_xlsx <- function(CSV){
  XLSX <- str_replace(CSV, "csv", "xlsx")
  CSV |>
    read_csv(col_names = FALSE) |>
    writexl::write_xlsx(XLSX,col_names = FALSE)
  return(XLSX)
}
# pacman::p_load(tidyverse, targets)
# exam_csv_xlsx("inst/extdata/example-grade-roster-filled.csv")
