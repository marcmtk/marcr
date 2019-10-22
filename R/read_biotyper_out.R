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
read_biotyper_out <- function(file) {
  f <- readLines(file) %>%
    .[c(-1, -length(.))] %>%
    split(cumsum(stringr::str_detect(., "P\\|"))) %>%
    purrr::keep(~ length(.x) == 13) %>%
    tibble::as_tibble()

<<<<<<< HEAD
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
=======
  if(nrow(f) == 0) return(NULL)
  m <- tidyr::gather(f) %>%
    dplyr::filter(stringr::str_detect(value, "^O\\|")) %>%
    tidyr::separate(value, LETTERS[1:8], sep = "[\\|\\^]",
                    fill = "right", extra = "merge") %>%
    tidyr::separate(C, c("C1", "C2"), sep = "\\-",
                    fill = "right", extra = "merge") %>%
    tidyr::separate(F, c("F1", "F2"), sep = "T",
                    fill = "right", extra = "merge") %>%
    dplyr::mutate(time = lubridate::ymd_hms(paste(F1, F2)),
           D    = dplyr::na_if(D, "EMPTY!")) %>%
    dplyr::select(key, sample = C1, isolate = C2,
                  spot_location = D, time, instrument_id = H)
>>>>>>> d625de3ddf38625a4d008b6114dda77997507a5d

  g <- f %>%
    dplyr::slice(4:dplyr::n()) %>%
    tidyr::gather() %>%
    tidyr::separate(value, into = LETTERS[1:13], sep = "[\\^\\|]") %>%
<<<<<<< HEAD
    dplyr::select(
      bakterie = J, organisme_id = I,
      score = L, key, guess_number = H
    ) %>%
    dplyr::left_join(maldi_translation_table, by = c("bakterie" = "before")) %>%
    dplyr::mutate(
      clinical_name =
        replace(
          clinical_name,
          is.na(clinical_name),
          bakterie[is.na(clinical_name)]
        )
    )

  g %>%
    dplyr::left_join(m, by = "key") %>%
    dplyr::rename(spot_number = key) %>% # Nesting structure is run_id -> spot_number -> guess_number
    dplyr::mutate(
      score = as.numeric(score), # Spot number is not equal to spot location
      sample = as.character(sample),
      isolate = as.integer(isolate)
    )
=======
    dplyr::select(bakterie = J, organisme_id = I,
                  score = L, key, guess_number = H) %>%
    dplyr::left_join(maldi_translation_table, by = c("bakterie" = "before")) %>%
    dplyr::mutate(clinical_name =
             replace(clinical_name,
                     is.na(clinical_name),
                     bakterie[is.na(clinical_name)]))

  g  %>%
    dplyr::left_join(m, by="key") %>%
    dplyr::rename(spot_number = key) %>% # Nesting structure is run_id -> spot_number -> guess_number
    dplyr::mutate(score = as.numeric(score), # Spot number is not equal to spot location
           sample = as.character(sample),
           isolate = as.integer(isolate))
>>>>>>> d625de3ddf38625a4d008b6114dda77997507a5d
}
