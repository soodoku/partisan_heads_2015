source("scripts/00_config.R")

check_sources <- function() {
  sources <- readr::read_csv(data_path("benchmark_sources.csv"), show_col_types = FALSE)
  for (i in seq_len(nrow(sources))) {
    stopifnot(sha256_file(data_path(sources$file[i])) == sources$extract_sha256[i])
  }
}
check_sources()

survey <- read_survey()
respondents <- prepare_respondents(survey)
guesses <- prepare_guesses(survey, respondents)
recall <- recall_extremity(
  survey, respondents,
  readr::read_csv(data_path("recall_name_crosswalk.csv"), show_col_types = FALSE),
  readr::read_csv(data_path("politician_scores.csv"), show_col_types = FALSE)
)
respondents <- left_join(respondents, recall, by = "respondent_id")

cces <- readr::read_csv(data_path("cces_2012.csv"), col_types = readr::cols(.default = "c"))
gss <- readr::read_csv(data_path("gss_2012.csv"), show_col_types = FALSE)
benchmarks <- build_benchmarks(cces, gss)

readr::write_csv(sample_flow(survey), table_path("sample_flow.csv"))
readr::write_csv(respondents, data_path("respondents.csv"))
readr::write_csv(guesses, data_path("guesses.csv"))
readr::write_csv(benchmarks, data_path("benchmarks.csv"))
