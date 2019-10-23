#' read_cobas_lgd
#'
#' Read lgd files from a COBAS Liat.
#'
#'
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_cobas_lgd <- function(file) {
  targets <- read.table(file,
                        skip = 11,
                        header = FALSE,
                        nrows = 4,
                        col.names = c("seq", "names", "colours"),
                        colClasses = "character")

  read.table(file,
             skip = 17,
             header = FALSE,
             col.names = c("cycle", targets$names),
             colClasses = "numeric") %>%
    tidyr::gather("channel", "flourescence", -cycle)
}
