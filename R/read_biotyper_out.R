#' Read biotyper logfiles
#'
#' Bruker biotyper generates logs in `D:/Data/mbt-out/` if the folder exists.
#' These files can be used to survey answers from the machine.
#'
#' @param file path to file <chr>
#'
#' @return
#' @export
#'
#' @examples
#'
#' one_file <- read_biotyper_out("data-raw/biotyper_1.astm")
#'
#' more_files <- list.files("data-raw", pattern = ".astm|.out", full.names = TRUE) %>%
#'   purrr::map_dfr(read_biotyper_out, .id = "run_id")
read_biotyper_out <- function(file) {
  f <- readLines(file, warn = FALSE) %>%
    .[c(-1, -length(.))] %>%
    split(cumsum(stringr::str_detect(., "P\\|"))) %>%
    purrr::keep(~ length(.x) == 13) %>%
    tibble::as_tibble()

  if (nrow(f) == 0) {
    return(NULL)
  }
  m <- tidyr::gather(f) %>%
    dplyr::filter(stringr::str_detect(value, "^O\\|")) %>%
    tidyr::separate(value, LETTERS[1:8],
      sep = "[\\|\\^]",
      fill = "right", extra = "merge"
    ) %>%
    tidyr::separate(C, c("C1", "C2"),
      sep = "\\-",
      fill = "right", extra = "merge"
    ) %>%
    tidyr::separate(F, c("F1", "F2"),
      sep = "T",
      fill = "right", extra = "merge"
    ) %>%
    dplyr::mutate(
      time = lubridate::ymd_hms(paste(F1, F2)),
      D = dplyr::na_if(D, "EMPTY!")
    ) %>%
    dplyr::select(key,
      sample = C1, isolate = C2,
      spot_location = D, time, instrument_id = H
    )

  g <- f %>%
    dplyr::slice(4:dplyr::n()) %>%
    tidyr::gather() %>%
    tidyr::separate(value, into = LETTERS[1:13], sep = "[\\^\\|]") %>%
    dplyr::select(
      organism = J, organism_id = I,
      score = L, key, guess_number = H
    ) %>%
    dplyr::left_join(maldi_translation_table, by = c("organism" = "before")) %>%
    dplyr::mutate(
      clinical_name =
        replace(
          clinical_name,
          is.na(clinical_name),
          organism[is.na(clinical_name)]
        )
    )

  g  %>%
    dplyr::left_join(m, by="key") %>%
    dplyr::rename(spot_number = key) %>% # Nesting structure is run_id -> spot_number -> guess_number
    dplyr::mutate(score = as.numeric(score), # Spot number is not equal to spot location
           sample = as.character(sample),
           isolate = as.integer(isolate))
}
