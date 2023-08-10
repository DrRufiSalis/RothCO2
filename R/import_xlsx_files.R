#' Import all XLSX files from a path
#'
#' @return A list of data frames
#' @export
import_xlsx_files <- function(path) {
  xlsx_files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
  xlsx_data <- lapply(xlsx_files, readxl::read_xlsx)
  names(xlsx_data) <- basename(xlsx_files)
  return(xlsx_data)
}
