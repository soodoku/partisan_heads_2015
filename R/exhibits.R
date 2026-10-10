# Shared style and writers for figures, tables and manuscript macros.

colours <- c(truth = "grey15", in_party = "grey60", out_party = "#08519C")
figure_dpi <- 200

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

save_figure <- function(plot, name, width, height) {
  ggplot2::ggsave(figure_path(paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggplot2::ggsave(figure_path(paste0(name, ".png")), plot, width = width, height = height, dpi = figure_dpi)
}

pct <- function(x, digits = 0) formatC(100 * x, format = "f", digits = digits)
num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
interval <- function(lo, hi, scale = 1, digits = 2) {
  f <- function(x) formatC(scale * x, format = "f", digits = digits)
  paste0("[", f(lo), ", ", f(hi), "]")
}
tex_escape <- function(x) gsub("&", "\\\\&", gsub("%", "\\\\%", x))

# A booktabs tabular. `groups` optionally names blocks of rows by their first index.
write_tabular <- function(df, path, align, groups = NULL) {
  stopifnot(nchar(gsub("p\\{[^}]*\\}", "p", align)) == ncol(df))
  header <- paste(tex_escape(names(df)), collapse = " & ")
  body <- apply(df, 1, function(r) paste(tex_escape(r), collapse = " & "))
  lines <- character(0)
  for (i in seq_along(body)) {
    if (!is.null(groups) && as.character(i) %in% names(groups)) {
      if (i > 1) lines <- c(lines, "\\addlinespace")
      lines <- c(lines, sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\", ncol(df), groups[[as.character(i)]]))
    }
    lines <- c(lines, paste0(body[i], " \\\\"))
  }
  writeLines(c(
    sprintf("\\begin{tabular}{%s}", align), "\\toprule", paste0(header, " \\\\"), "\\midrule",
    lines, "\\bottomrule", "\\end{tabular}"
  ), path)
}

write_macros <- function(values, path) {
  stopifnot(!anyDuplicated(names(values)), all(grepl("^[A-Za-z]+$", names(values))))
  writeLines(sprintf("\\newcommand{\\%s}{%s}", names(values), unlist(values)), path)
}
