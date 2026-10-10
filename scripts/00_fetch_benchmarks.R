# Downloads the 2012 CCES common content and the 2012 GSS, keeps the columns the
# benchmarks need and freezes them in data/. Run by hand; the build reads
# only the frozen extracts. The full downloads (~110 MB) stay in a temporary directory.

source("scripts/00_config.R")

download_dir <- file.path(tempdir(), "benchmarks")
dir.create(download_dir, showWarnings = FALSE)

cces_url <- "https://dataverse.harvard.edu/api/access/datafile/2456347"
gss_url <- "https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/2012_stata.zip"

cces_file <- file.path(download_dir, "commoncontent2012.tab")
gss_zip <- file.path(download_dir, "2012_stata.zip")
options(timeout = 900)
if (!file.exists(cces_file)) download.file(cces_url, cces_file, mode = "wb")
if (!file.exists(gss_zip)) download.file(gss_url, gss_zip, mode = "wb")
gss_file <- utils::unzip(gss_zip, exdir = download_dir)

cces_columns <- c(
  "V101", "V103", "pid3", "pid7", "birthyr", "gender", "educ", "race", "employ",
  "union", "faminc", "religpew", "pew_bornagain"
)
# The .tab file contains unbalanced quotation marks in free-text columns; with quote
# processing on, every reader tried silently merges rows (30,140 of 54,535 survive).
cces <- utils::read.delim(cces_file, quote = "", colClasses = "character", na.strings = character(0))
cces <- cces[, cces_columns]
# Marginal counts printed in the CCES 2012 codebook (cces_guide_2012.pdf).
stopifnot(
  nrow(cces) == 54535L, !anyDuplicated(cces$V101),
  identical(as.vector(table(cces$gender)), c(25590L, 28945L)),
  identical(as.vector(table(factor(cces$pid3, 1:5))), c(20246L, 14484L, 15013L, 2313L, 2479L)),
  identical(as.vector(table(factor(cces$union, 1:3))), c(3554L, 11991L, 38736L)),
  identical(as.vector(table(factor(cces$pew_bornagain, 1:2))), c(16916L, 37489L))
)

gss_columns <- c(
  "id", "wtssall", "partyid", "class", "relig", "reborn", "god", "racecen1", "sex",
  "age", "educ", "degree", "wrkstat", "union"
)
gss <- haven::read_dta(gss_file)
names(gss) <- tolower(names(gss))
gss <- haven::zap_labels(gss[, gss_columns])

readr::write_csv(cces, data_path("cces_2012.csv"), na = "")
readr::write_csv(gss, data_path("gss_2012.csv"))

sources <- data.frame(
  file = c("cces_2012.csv", "gss_2012.csv"),
  source = c(
    paste("CCES 2012 Common Content, doi:10.7910/DVN/HQEVPK, file commoncontent2012.tab;", cces_url),
    paste("General Social Survey 2012 cross-section, NORC;", gss_url)
  ),
  download_sha256 = c(sha256_file(cces_file), sha256_file(gss_file)),
  extract_sha256 = c(sha256_file(data_path("cces_2012.csv")), sha256_file(data_path("gss_2012.csv")))
)
readr::write_csv(sources, data_path("benchmark_sources.csv"))
unlink(download_dir, recursive = TRUE)
