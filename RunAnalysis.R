# ============================================================
# 96-WELL PLATE HOLD-TIME ANALYSIS
# ============================================================

source(file.path("R", "hold_time_analysis.R"))

read_input <- function(prompt) {
  if (interactive()) {
    x <- readline(prompt)
  } else {
    cat(prompt)
    x <- scan("stdin", what = character(), nmax = 1,
              sep = "\n", quiet = TRUE)
    if (!length(x))
      stop("No input received. Run the script from a terminal and enter a file path.")
  }
  trimws(x[1])
}

ask_file <- function(prompt, ext) {
  repeat {
    path <- read_input(prompt)
    
    # Clean pasted/dragged paths
    path <- gsub('^["\']|["\']$', "", path)
    path <- gsub("\\\\ ", " ", path)
    path <- path.expand(path)
    
    if (file.exists(path)) {
      if (!grepl(paste0("\\.", ext, "$"), path, ignore.case = TRUE))
        warning("Expected a .", ext, " file.")
      return(normalizePath(path))
    }
    
    cat("File not found: ", path, "\nPlease try again.\n", sep = "")
  }
}

cat("\n96-WELL PLATE HOLD-TIME ANALYSIS\n\n")

data_file   <- ask_file("Plate-reader .txt file path: ", "txt")
layout_file <- ask_file("Plate-layout .xlsx file path: ", "xlsx")

output_dir <- "Output" # Modify to de desired output directory
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("\nRunning analysis...\n")

results <- run_hold_time_analysis(
  data_file = data_file,
  layout_file = layout_file,
  output_dir = output_dir,
  threshold = 0.150
)

cat("\nAnalysis complete.\nResults saved in: ",
    normalizePath(output_dir), "\n", sep = "")
