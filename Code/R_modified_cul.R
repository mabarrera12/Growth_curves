# ============================================================
# 96-WELL PLATE HOLD-TIME ANALYSIS
# ============================================================

# Input files -------------------------------------------------
file <- "Data/KlebHR_2.4.25_Plate1.txt"   # Cytation 7 absorbance data
file_layout <- "Data/plate_layout.xlsx"


## CODE ----- DO NOT MODIFY
# Packages ----------------------------------------------------
pkg <- c("lubridate","ggplot2","dplyr","tidyr","tibble","ggprism",
         "plotly","htmltools","openxlsx","knitr")
missing_pkg <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing_pkg)) install.packages(missing_pkg)
invisible(lapply(pkg, library, character.only=TRUE))

# Plate organization ------------------------------------------
plate_layout <- read.xlsx(file_layout, colNames=TRUE, rowNames=TRUE)

# Locate non-empty wells and create IDs such as A1, B1, A2, etc.
cells_with_info <- which(!is.na(plate_layout), arr.ind=TRUE)
well_ids <- paste0(
  rownames(plate_layout)[cells_with_info[,"row"]],
  colnames(plate_layout)[cells_with_info[,"col"]]
)
print(well_ids)

# Group replicate wells by sample name.
sample_wells <- split(well_ids, plate_layout[cells_with_info])

# Convert plate layout to long format for plotting.
plate <- plate_layout |>
  rownames_to_column("Row") |>
  mutate(across(-Row, as.character)) |>
  pivot_longer(-Row, names_to="Col", values_to="Sample") |>
  mutate(
    Col=as.integer(Col),
    Row=factor(Row, levels=rev(LETTERS[1:8]))
  )

# Draw the 96-well plate layout.
plot_plate <- ggplot(plate, aes(Col, Row, fill=Sample)) +
  geom_point(shape=21, size=10, color="black", stroke=0.6) +
  geom_text(aes(label=Sample), size=3, na.rm=TRUE) +
  scale_x_continuous(breaks=1:12, position="top") +
  scale_fill_discrete(na.value="white") +
  coord_fixed() +
  labs(x=NULL, y=NULL, fill="Sample") +
  theme_void() +
  theme(
    axis.text=element_text(size=11, color="black"),
    legend.position="right"
  )

# Plate-reader data -------------------------------------------
# Import measurements and convert elapsed time to hours.
data <- read.delim(
  file, header=TRUE, sep="\t",
  fileEncoding="latin1", stringsAsFactors=FALSE
)

data$hours <- as.numeric(hms(data$Time)-hms(data$Time[1]))/3600

# Retain time and wells present in the plate layout.
wells_final <- data |>
  select(hours, all_of(well_ids)) |>
  mutate(across(everything(), ~as.numeric(as.character(.))))

# Hold-time calculation ---------------------------------------
# Fits a smoothing spline and finds the maximum second derivative.
inflection_time <- function(little_well, plot_graph=FALSE, th=0.150) {
  data.temp <- data.frame(
    x=wells_final$hours,
    y=wells_final[[little_well]]
  ) |> na.omit()
  
  spline_fit <- smooth.spline(data.temp$x, data.temp$y, spar=0.6)
  data.temp$y.smooth <- predict(spline_fit, data.temp$x)$y
  
  # Evaluate the second derivative across a dense time sequence.
  x_seq <- seq(min(data.temp$x), max(data.temp$x), length.out=4000)
  second_deriv <- predict(spline_fit, x_seq, deriv=2)$y
  
  # Add 3 because the derivative search begins at index 4.
  max_index <- which.max(second_deriv[4:length(second_deriv)])+3
  h_time <- x_seq[max_index]
  
  # Assign 24 hours when overall growth remains below threshold.
  if (mean(data.temp$y, na.rm=TRUE)<th) h_time <- 24
  
  # Growth-curve plot.
  p1 <- ggplot(data.temp, aes(x, y)) +
    geom_point(color="blue", size=0.7) +
    geom_line(aes(y=y.smooth), color="red", linewidth=0.5, alpha=0.7) +
    geom_vline(xintercept=h_time, color="darkblue", linetype="dashed") +
    geom_hline(yintercept=th, linetype="dotted") +
    coord_cartesian(xlim=c(0,24), ylim=c(0,1.5)) +
    labs(
      x="Hours", y="OD600",
      title=paste0(little_well, " time: ", round(h_time, 2), " h")
    ) +
    theme_prism()
  
  if (plot_graph) print(ggplotly(p1))
  list(h_time=h_time, plot=p1)
}

# Analyze every populated well -------------------------------
well_results <- lapply(well_ids, inflection_time)
names(well_results) <- well_ids

# Extract hold times as a named numeric vector.
inf_time <- sapply(well_results, `[[`, "h_time")

# Individual-well results table.
well_table <- data.frame(
  Sample=plate_layout[cells_with_info],
  Well=well_ids,
  INF_time=inf_time[well_ids],
  row.names=NULL
)

# Convert a stored ggplot into an interactive Plotly graph.
create_plot <- function(well_id) ggplotly(well_results[[well_id]]$plot)

# Sample summary ----------------------------------------------
# Determine the largest replicate count for table formatting.
max_rep <- max(lengths(sample_wells))

# Calculate sample means, SDs, replicate counts and values.
sample_results <- lapply(names(sample_wells), \(s) {
  ws <- sample_wells[[s]]
  vals <- inf_time[ws]
  
  reps <- setNames(
    as.list(c(vals, rep(NA, max_rep-length(vals)))),
    paste0("Rep_", seq_len(max_rep))
  )
  
  cbind(
    data.frame(
      Sample=s,
      Wells=paste(ws, collapse=", "),
      mean_time_to_inflection = mean(vals, na.rm=TRUE),
      SD_time_to_inflection = sd(vals, na.rm=TRUE),
      N = sum(!is.na(vals))
    ),
    as.data.frame(reps)
  )
}) |> bind_rows()

# HTML and TSV output -----------------------------------------
generate_html <- function() {
  plate_html <- ggplotly(plot_plate)
  summary_html <- HTML(knitr::kable(sample_results, format="html", digits=2,
                                    table.attr="style='width:100%;border-collapse:collapse;'"))
  
  sample_sections <- lapply(names(sample_wells), \(s) {
    ws <- sample_wells[[s]]
    div(style="margin-top:30px;",
        h2(paste0(s, " | Mean hold time: ", round(mean(inf_time[ws], na.rm=TRUE), 2), " h")),
        div(style="display:grid;grid-template-columns:repeat(3,1fr);gap:10px;",
            lapply(ws, \(w) div(create_plot(w)))))
  })
  
  html <- tagList(h1("96-well plate analysis"), h2("Plate layout"), plate_html,
                  h2("Sample summary"), summary_html, sample_sections)
  
  output_name <- sub("\\.txt$", "", file)
  save_html(html, paste0(output_name, "_plate_analysis.html"))
  write.table(sample_results, paste0(output_name, "_plate_analysis.tsv"),
              sep="\t", row.names=FALSE, quote=FALSE)
}

# Run report generation.
generate_html()

