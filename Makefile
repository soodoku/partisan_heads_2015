.PHONY: restore benchmarks analysis manuscript paper format lint test check ci-docker clean

restore:
	Rscript --vanilla -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::load(project = getwd()); renv::restore(prompt = FALSE)'

# Re-downloads the CCES and GSS files and rewrites the frozen extracts; not part of the build.
benchmarks:
	Rscript scripts/00_fetch_benchmarks.R

analysis:
	Rscript scripts/01_prepare.R
	Rscript scripts/02_estimate.R
	Rscript scripts/03_exhibits.R

manuscript:
	cd ms && latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

paper: analysis
	$(MAKE) manuscript

format:
	Rscript -e 'for (d in c("R", "scripts", "tests")) styler::style_dir(d)'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -v "$(CURDIR):/project" -w /project \
		-v r_renv_cache:/root/.cache/R/renv \
		-e RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest \
		rocker/verse:4.6.0 bash -c "tlmgr install latexmk fontspec booktabs float caption natbib xurl setspace adjustbox && make restore check"

clean:
	cd ms && latexmk -C main.tex
