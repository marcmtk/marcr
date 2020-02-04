#' read_cobas_lgd
#'
#' Read lgd files from a COBAS Liat.
#'
#'
#'
#' @param file path to file <chr>
#'
#' @return
#' @export
#'
#' @examples
#'
#' curve_data <- read_cobas_lgd("data-raw/inf_b.lgd")
read_cobas_lgd <- function(file) {
  demarcations <- grep("\\/\\/", readLines(file))
  len_dem <- length(demarcations)
  
  targets <- read.table(file,
                        skip = demarcations[len_dem - 1],
                        header = FALSE,
                        nrows = demarcations[len_dem] - demarcations[len_dem - 1] - 2,
                        col.names = c("seq", "names", "colours"),
                        colClasses = "character")
  
  read.table(file,
             skip = demarcations[len_dem],
             header = FALSE,
             col.names = c("cycle", targets$names),
             colClasses = "numeric") %>%
    tidyr::gather("channel", "flourescence", -cycle) %>%
    dplyr::mutate(folder = fs::path_dir(file),
                  file = tools::file_path_sans_ext(basename(file)))
}
