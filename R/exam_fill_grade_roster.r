#' exam_fill_grade_roster
#'
#' fills the grade roster
#'
#' @param df marks data frame
#' @param IDs column that has the studnet IDs
#' @param total column that has the final mark
#' @param infile empty grade roster
#' @param outfile filled grade roster
#' @param RAA values to give academic RAA
#'
#' @return filled grade roster
#' @export
#'
#' @examples
#' df <- tibble(
#'   ID = 1:10,
#'   marks = c(0, 40, 44, 48, 50, 51, 65, 75, 85, 100)
#' )
#' exam_fill_grade_roster(
#'   df, ID, marks,
#'   "inst/extdata/example-grade-roster.csv",
#'   "inst/extdata/example-grade-roster-filled.csv"
#' )
#' exam_fill_grade_roster(
#'   df, ID, marks,RAA = 40:49,
#'   "inst/extdata/example-grade-roster.csv",
#'   "inst/extdata/example-grade-roster-filled-2.csv"
#' )
exam_fill_grade_roster  <- function(
    df, IDs, total, infile, outfile,
    RAA = 45:49
){
  # Read in the grade roster
  grade_roster  <- read_lines(infile)
  # Find start of data
  i <- which(str_detect(grade_roster, "EmplID"))
  # Get header
  header  <- grade_roster[1:(i - 1)]
  # Write out header
  write_lines(header, outfile)
  # data
  data  <- read_csv(infile, skip = i-1)
  # Get IDs
  IDs  <- df %>% pull({{IDs}})
  # Convert to no-a version
  IDs <- as.numeric(str_remove(IDs, "a"))
  # Get totals
  total  <- df %>% pull({{total}})
  # Add totals
  for(i in 1:nrow(data)){
    ID  <- data$EmplID[i]
    index  <- which(IDs == ID)
    if(length(index) > 0){
      # Add total
      data$`Mark/Grade Input`[i]  <- total[index]
      # Add supp code
      if(total[index] %in% RAA){
        data$`Transcript Note ID`[i]  <- "US10"
      }
      # Add FNS
      if(total[index] == 0){
        data$`Mark/Grade Input`[i]  <- "FNS"
      }
    }
  }
  # Remove extra column if needed
  if("X13" %in% colnames(data)){
    data <-
      data %>%
      select(-X13)
  }
  write_csv(data, outfile, append = TRUE, na = "", col_names = TRUE)
}
# df <- tibble(
#   ID = 1:10,
#   marks = c(0, 40, 44, 48, 50, 51, 65, 75, 85, 100)
# )
# exam_fill_grade_roster(
#   df, ID, marks,
#   "inst/extdata/example-grade-roster.csv",
#   "inst/extdata/example-grade-roster-filled.csv"
# )
# exam_fill_grade_roster(
#   df, ID, marks,RAA = 40:49,
#   "inst/extdata/example-grade-roster.csv",
#   "inst/extdata/example-grade-roster-filled-2.csv"
# )


