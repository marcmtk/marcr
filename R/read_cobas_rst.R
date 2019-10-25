#' Title read_cobas_rst
#'
#' Reads a rst file from a COBAS Liat. Most information is available through
#' middleware, but information on invalid runs is currently not.
#'
#' @param file path to file <chr>
#'
#' @return
#' @export
#'
#' @examples
#'
#' result <- read_cobas_rst("data-raw/valid_pos.rst")
#'
#' many_results <- list.files("data-folder",
#'   full.names = TRUE, pattern = ".rst"
#' ) %>%
#'   purrr::map_dfr(read_cobas_rst)
read_cobas_rst <- function(file) {
  detect_result <- function(result_string, agent) { # 3 mulige outputs, TRUE, FALSE, NA
    # TRUE hvis "agent detected"
    # FALSE hvis "agent not detected"
    # NA hvis agent ikke detekteret
    lowered <- stringr::str_to_lower(result_string)
    agent <- stringr::str_to_lower(agent)
    dplyr::case_when(
      !stringr::str_detect(lowered, agent) ~ NA,
      stringr::str_detect(lowered, paste(agent, "detected")) ~ TRUE,
      stringr::str_detect(lowered, paste(agent, "not detected")) ~ FALSE
    )
  }
  con <- file(file)
  raw <- readLines(con)
  close(con)
  report_start <- which(grepl("\\[Report\\]", raw))
  hldata_start <- which(grepl("\\[HLData\\]", raw))
  report <- raw[seq(report_start + 1, hldata_start - 2)]
  indented_lines <- which(grepl("(^\\s+)|(^Warning:)", report))
  report[indented_lines[1] - 1] <- paste0(
    report[indented_lines[1] - 1],
    report[indented_lines] %>% trimws() %>% paste(collapse = ", ")
  )
  report <- report[-indented_lines]
  sep_regex <- ":"
  report_tbl <- tibble::tibble(x = report) %>%
    tidyr::separate(x, c("var", "value"),
                    sep = sep_regex,
                    fill = "right", extra = "merge"
    )

  hldata <- raw[seq(hldata_start + 1, length(raw))]
  hldata_tbl <- tibble::tibble(x = hldata) %>%
    tidyr::separate(x, c("var", "value"),
                    sep = sep_regex,
                    fill = "right", extra = "merge"
    ) %>%
    dplyr::filter(!var %in% c("Observation", "Interpretation", "Time/Date"))

  dplyr::bind_rows(report_tbl, hldata_tbl) %>%
    dplyr::mutate(value = trimws(value)) %>%
    tidyr::spread(var, value) %>%
    dplyr::mutate(
      AnalysisDateTime = anytime::anytime(AnalysisDateTime),
      `Tube Exp` = readr::parse_date(`Tube Exp`)
    ) %>%
    dplyr::mutate(
      infl_a = detect_result(`Report Results`, "Influenza A"),
      infl_b = detect_result(`Report Results`, "Influenza B"),
      rsv = detect_result(`Report Results`, "RSV"),
      invalid = stringr::str_detect(
        `Report Results` %>% stringr::str_to_lower(),
        "invalid"
      ),
      warning = stringr::str_detect(
        `Report Results` %>% stringr::str_to_lower(),
        "warning"
      ),
      file = tools::file_path_sans_ext(basename(file))
    )
}

