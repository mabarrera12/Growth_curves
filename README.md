# 96-Well Plate Hold-Time Analysis

R workflow for analyzing 96-well plate-reader OD600 time-series data using a plate-layout file. The workflow identifies populated wells, calculates a hold time for each well, summarizes replicate wells by sample, and produces an interactive HTML report plus a TSV summary.

## Repository structure

```text
96-well-hold-time/
├── README.md
├── .gitignore
├── install_packages.R
├── run_analysis.R
├── R/
│   └── hold_time_analysis.R
├── Data/
│   ├── Example_data_1.txt
│   └── plate_layout.xlsx
└── Output/
    └── .gitkeep
```

## Requirements

- R 4.1 or newer
- Required R packages:
  - lubridate
  - ggplot2
  - dplyr
  - tidyr
  - tibble
  - ggprism
  - plotly
  - htmltools
  - openxlsx
  - knitr

Install the required packages once from the repository root:

```bash
Rscript install_packages.R
```

You can also run `install_packages.R` from RStudio.

## Run the included example

From the repository root:

```bash
Rscript run_analysis.R
```

The example uses:

```text
Data/Example_data_1.txt
Data/plate_layout.xlsx
```

The generated files are written to:

```text
Output/Example_data_1_plate_analysis.html
Output/Example_data_1_plate_analysis.tsv
```

## Analyze your own data

1. Put your plate-reader `.txt` file in `Data/`.
2. Put your plate-layout `.xlsx` file in `Data/`.
3. Open `run_analysis.R`.
4. Change only these paths:

```r
file <- file.path("Data", "YOUR_DATA_FILE.txt")
file_layout <- file.path("Data", "YOUR_LAYOUT_FILE.xlsx")
```

5. Run:

```bash
Rscript run_analysis.R
```

Alternatively, from RStudio, open `run_analysis.R` and source the file from the repository root.

## Input format

### Plate-reader data

The plate-reader file is expected to be a tab-delimited text file containing:

- a `Time` column
- well columns named using standard 96-well identifiers such as `A1`, `A2`, ..., `H12`

Only wells populated in the plate-layout file are analyzed.

### Plate layout

The Excel layout should represent the 8 x 12 plate. The row names should correspond to rows `A` through `H`, the columns should correspond to `1` through `12`, and populated cells should contain the sample name. Wells with the same sample name are treated as replicates.

## Analysis method

The numerical analysis has been kept unchanged from the original script.

For each populated well:

1. Elapsed time is converted to hours relative to the first measurement.
2. A smoothing spline is fitted to OD600 versus time using `spar = 0.6`.
3. The second derivative is evaluated over 4,000 equally spaced time points.
4. The maximum second derivative is identified after excluding the first three derivative points.
5. The corresponding time is reported as the hold time.
6. If the mean OD600 for the well is below `0.150`, the hold time is assigned as `24` hours.

The growth-curve plots retain the original plotting parameters, including an x-axis range of 0-24 hours and a y-axis range of 0-1.5 OD600.

## Output

The analysis produces two files:

- `*_plate_analysis.html` - interactive HTML report containing the plate layout, sample summary, and individual well plots.
- `*_plate_analysis.tsv` - tab-delimited sample summary containing sample name, wells, mean hold time, standard deviation, number of replicates, and replicate-level hold times.

The returned `results` object in `run_analysis.R` also contains the individual-well table and intermediate analysis objects for users who want to inspect them in R.

## Which file should I edit?

For ordinary use, edit only:

```text
run_analysis.R
```

The analysis functions are stored in:

```text
R/hold_time_analysis.R
```

Keeping configuration separate from the analysis code makes the repository easier to use, review, and maintain.

## License

Before publishing the repository, choose a software license and add a `LICENSE` file. Common choices for open-source scientific code include MIT, Apache-2.0, and GPL-3.0. The choice is yours and has legal implications, so this repository does not select one automatically.
