# ============================================================
# RUN THE 96-WELL PLATE HOLD-TIME ANALYSIS
# ============================================================
# For normal use, this is the only file you need to edit.

source(file.path("R", "hold_time_analysis.R"))

# -------------------------------------------------------------------------
# USER SETTINGS
# -------------------------------------------------------------------------

# Plate-reader output file
file <- file.path("Example", "Example_data_1.txt")

# Plate-layout Excel file
file_layout <- file.path("Example", "plate_layout.xlsx")

# Folder where the HTML report and TSV summary will be written
output_dir <- "Example/Output"

# -------------------------------------------------------------------------
# RUN ANALYSIS
# -------------------------------------------------------------------------
# The analysis threshold remains 0.150, exactly as in the original script.

results <- run_hold_time_analysis(
  data_file = file,
  layout_file = file_layout,
  output_dir = output_dir,
  threshold = 0.150
)
