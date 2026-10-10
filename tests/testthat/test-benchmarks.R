benchmarks <- read_derived("benchmarks.csv")

test_that("frozen extracts match their recorded hashes", {
  sources <- readr::read_csv(data_path("benchmark_sources.csv"), show_col_types = FALSE)
  for (i in seq_len(nrow(sources))) {
    expect_equal(sha256_file(data_path(sources$file[i])), sources$extract_sha256[i])
  }
})

test_that("CCES extract reproduces codebook marginals", {
  cces <- readr::read_csv(data_path("cces_2012.csv"), col_types = readr::cols(.default = "c"))
  expect_equal(nrow(cces), 54535L)
  expect_equal(as.vector(table(cces$gender)), c(25590L, 28945L))
  expect_equal(as.vector(table(factor(cces$union, 1:3))), c(3554L, 11991L, 38736L))
})

test_that("shares sum to one within party and attribute", {
  sums <- benchmarks |>
    group_by(source, spec, party, attribute) |>
    summarise(total = sum(share), .groups = "drop")
  expect_true(all(abs(sums$total - 1) < 1e-9))
})

test_that("weighted shares match a direct weighted mean", {
  cces <- readr::read_csv(data_path("cces_2012.csv"), col_types = readr::cols(.default = "c"))
  dem <- cces$pid3 == "1"
  union <- cces$union %in% c("1", "2", "3")
  w <- as.numeric(cces$V103)
  direct <- sum(w[dem & union & cces$union == "1"]) / sum(w[dem & union])
  computed <- benchmarks |>
    filter(source == "CCES", spec == "primary", party == "Democrat", category == "Union member") |>
    pull(share)
  expect_equal(computed, direct)
})
