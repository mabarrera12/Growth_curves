# Install the R packages required by the analysis.
# Run this once from the repository root:
# Rscript install_packages.R

packages <- c(
  "lubridate", "ggplot2", "dplyr", "tidyr", "tibble", "ggprism",
  "plotly", "htmltools", "openxlsx", "knitr"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0) {
  message("All required packages are already installed.")
} else {
  message(
    "Installing: ",
    paste(missing_packages, collapse = ", ")
  )

  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}
