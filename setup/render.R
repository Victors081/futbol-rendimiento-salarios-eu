
source(file = "setup/functions.R")

## Render Quarto Report

quarto_render_move(
  input = "reports/01-report.qmd",
  output_file = "reports/01-report.html",
  output_dir = "reports/html"
)