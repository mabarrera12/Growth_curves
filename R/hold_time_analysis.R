# ============================================================
# 96-WELL PLATE HOLD-TIME ANALYSIS
# Core analysis functions
# ============================================================

# Required packages ---------------------------------------------------------
required_packages <- c(
  "lubridate", "ggplot2", "dplyr", "tidyr", "tibble", "ggprism",
  "plotly", "htmltools", "openxlsx", "knitr"
)

check_required_packages <- function() {
  missing_pkg <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkg) > 0) {
    stop(
      paste0(
        "Missing required R package(s): ",
        paste(missing_pkg, collapse = ", "),
        "\nRun: Rscript install_packages.R"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Plate organization --------------------------------------------------------
prepare_plate_layout <- function(layout_file) {
  plate_layout <- openxlsx::read.xlsx(
    layout_file,
    colNames = TRUE,
    rowNames = TRUE
  )

  # Locate non-empty wells and create IDs such as A1, B1, A2, etc.
  cells_with_info <- which(!is.na(plate_layout), arr.ind = TRUE)

  well_ids <- paste0(
    rownames(plate_layout)[cells_with_info[, "row"]],
    colnames(plate_layout)[cells_with_info[, "col"]]
  )

  # Group replicate wells by sample name.
  sample_wells <- split(well_ids, plate_layout[cells_with_info])

  # Convert plate layout to long format for plotting.
  plate <- plate_layout |>
    tibble::rownames_to_column("Row") |>
    dplyr::mutate(dplyr::across(-Row, as.character)) |>
    tidyr::pivot_longer(-Row, names_to = "Col", values_to = "Sample") |>
    dplyr::mutate(
      Col = as.integer(Col),
      Row = factor(Row, levels = rev(LETTERS[1:8]))
    )

  # Draw the 96-well plate layout.
  plot_plate <- ggplot2::ggplot(
    plate,
    ggplot2::aes(Col, Row, fill = Sample)
  ) +
    ggplot2::geom_point(
      shape = 21,
      size = 10,
      color = "black",
      stroke = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Sample),
      size = 3,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      position = "top"
    ) +
    ggplot2::scale_fill_discrete(na.value = "white") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Sample") +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 11, color = "black"),
      legend.position = "right"
    )

  list(
    plate_layout = plate_layout,
    cells_with_info = cells_with_info,
    well_ids = well_ids,
    sample_wells = sample_wells,
    plate = plate,
    plot_plate = plot_plate
  )
}

# Plate-reader data ---------------------------------------------------------
read_plate_reader <- function(data_file, well_ids) {
  # Import measurements and convert elapsed time to hours.
  data <- utils::read.delim(
    data_file,
    header = TRUE,
    sep = "\t",
    fileEncoding = "latin1",
    stringsAsFactors = FALSE
  )

  data$hours <- as.numeric(
    lubridate::hms(data$Time) - lubridate::hms(data$Time[1])
  ) / 3600

  # Retain time and wells present in the plate layout.
  wells_final <- data |>
    dplyr::select(hours, dplyr::all_of(well_ids)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.numeric(as.character(.))
      )
    )

  list(
    data = data,
    wells_final = wells_final
  )
}

# Hold-time calculation ----------------------------------------------------
# This preserves the original analysis exactly:
# - smoothing spline spar = 0.6
# - second derivative evaluated at 4000 points
# - derivative search starts at index 4
# - default threshold = 0.150
# - hold time = 24 h when mean OD remains below threshold
inflection_time <- function(
    little_well,
    wells_final,
    plot_graph = FALSE,
    th = 0.150
) {
  data.temp <- data.frame(
    x = wells_final$hours,
    y = wells_final[[little_well]]
  ) |>
    stats::na.omit()

  spline_fit <- stats::smooth.spline(
    data.temp$x,
    data.temp$y,
    spar = 0.6
  )

  data.temp$y.smooth <- stats::predict(
    spline_fit,
    data.temp$x
  )$y

  # Evaluate the second derivative across a dense time sequence.
  x_seq <- seq(
    min(data.temp$x),
    max(data.temp$x),
    length.out = 4000
  )

  second_deriv <- stats::predict(
    spline_fit,
    x_seq,
    deriv = 2
  )$y

  # Add 3 because the derivative search begins at index 4.
  max_index <- which.max(second_deriv[4:length(second_deriv)]) + 3
  h_time <- x_seq[max_index]

  # Assign 24 hours when overall growth remains below threshold.
  if (mean(data.temp$y, na.rm = TRUE) < th) {
    h_time <- 24
  }

  # Growth-curve plot.
  p1 <- ggplot2::ggplot(
    data.temp,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point(color = "blue", size = 0.7) +
    ggplot2::geom_line(
      ggplot2::aes(y = y.smooth),
      color = "red",
      linewidth = 0.5,
      alpha = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = h_time,
      color = "darkblue",
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = th,
      linetype = "dotted"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 24),
      ylim = c(0, 1.5)
    ) +
    ggplot2::labs(
      x = "Hours",
      y = "OD600",
      title = paste0(
        little_well,
        " time: ",
        round(h_time, 2),
        " h"
      )
    ) +
    ggprism::theme_prism()

  if (plot_graph) {
    print(plotly::ggplotly(p1))
  }

  list(
    h_time = h_time,
    plot = p1
  )
}

# Analyze every populated well ---------------------------------------------
analyze_wells <- function(
    well_ids,
    wells_final,
    plate_layout,
    cells_with_info,
    threshold = 0.150
) {
  well_results <- lapply(
    well_ids,
    function(well_id) {
      inflection_time(
        little_well = well_id,
        wells_final = wells_final,
        plot_graph = FALSE,
        th = threshold
      )
    }
  )

  names(well_results) <- well_ids

  # Extract hold times as a named numeric vector.
  inf_time <- sapply(well_results, `[[`, "h_time")

  # Individual-well results table.
  well_table <- data.frame(
    Sample = plate_layout[cells_with_info],
    Well = well_ids,
    INF_time = inf_time[well_ids],
    row.names = NULL
  )

  list(
    well_results = well_results,
    inf_time = inf_time,
    well_table = well_table
  )
}

# Sample summary -----------------------------------------------------------
build_sample_results <- function(sample_wells, inf_time) {
  # Determine the largest replicate count for table formatting.
  max_rep <- max(lengths(sample_wells))

  # Calculate sample means, SDs, replicate counts and values.
  sample_results <- lapply(
    names(sample_wells),
    function(s) {
      ws <- sample_wells[[s]]
      vals <- inf_time[ws]

      reps <- setNames(
        as.list(
          c(
            vals,
            rep(NA, max_rep - length(vals))
          )
        ),
        paste0("Rep_", seq_len(max_rep))
      )

      cbind(
        data.frame(
          Sample = s,
          Wells = paste(ws, collapse = ", "),
          mean_time_to_inflection = mean(vals, na.rm = TRUE),
          SD_time_to_inflection = stats::sd(vals, na.rm = TRUE),
          N = sum(!is.na(vals))
        ),
        as.data.frame(reps)
      )
    }
  ) |>
    dplyr::bind_rows()

  sample_results
}

# Plot helper ---------------------------------------------------------------
create_plot <- function(well_id, well_results) {
  plotly::ggplotly(well_results[[well_id]]$plot)
}

# HTML and TSV output ------------------------------------------------------
generate_html <- function(
    plot_plate,
    sample_results,
    sample_wells,
    inf_time,
    well_results,
    output_base
) {
  plate_html <- plotly::ggplotly(plot_plate)

  summary_html <- htmltools::HTML(
    knitr::kable(
      sample_results,
      format = "html",
      digits = 2,
      table.attr = "style='width:100%;border-collapse:collapse;'"
    )
  )

  sample_sections <- lapply(
    names(sample_wells),
    function(s) {
      ws <- sample_wells[[s]]

      htmltools::div(
        style = "margin-top:30px;",
        htmltools::h2(
          paste0(
            s,
            " | Mean hold time: ",
            round(mean(inf_time[ws], na.rm = TRUE), 2),
            " h"
          )
        ),
        htmltools::div(
          style = "display:grid;grid-template-columns:repeat(3,1fr);gap:10px;",
          lapply(
            ws,
            function(w) {
              htmltools::div(create_plot(w, well_results))
            }
          )
        )
      )
    }
  )

  html <- htmltools::tagList(
    htmltools::h1("96-well plate analysis"),
    htmltools::h2("Plate layout"),
    plate_html,
    htmltools::h2("Sample summary"),
    summary_html,
    sample_sections
  )

  html_file <- paste0(output_base, "_plate_analysis.html")
  tsv_file <- paste0(output_base, "_plate_analysis.tsv")

  htmltools::save_html(html, html_file)

  utils::write.table(
    sample_results,
    tsv_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  list(
    html_file = html_file,
    tsv_file = tsv_file
  )
}

# Main public entry point ---------------------------------------------------
run_hold_time_analysis <- function(
    data_file,
    layout_file,
    output_dir = "Output",
    threshold = 0.150
) {
  check_required_packages()

  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file, call. = FALSE)
  }

  if (!file.exists(layout_file)) {
    stop("Plate-layout file not found: ", layout_file, call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plate_info <- prepare_plate_layout(layout_file)

  # Preserve the original behavior of showing populated well IDs.
  print(plate_info$well_ids)

  reader_info <- read_plate_reader(
    data_file = data_file,
    well_ids = plate_info$well_ids
  )

  analysis <- analyze_wells(
    well_ids = plate_info$well_ids,
    wells_final = reader_info$wells_final,
    plate_layout = plate_info$plate_layout,
    cells_with_info = plate_info$cells_with_info,
    threshold = threshold
  )

  sample_results <- build_sample_results(
    sample_wells = plate_info$sample_wells,
    inf_time = analysis$inf_time
  )

  # Preserve the original output file naming; only the directory is cleaner.
  output_stem <- sub("\\.txt$", "", basename(data_file))
  output_base <- file.path(output_dir, output_stem)

  output_files <- generate_html(
    plot_plate = plate_info$plot_plate,
    sample_results = sample_results,
    sample_wells = plate_info$sample_wells,
    inf_time = analysis$inf_time,
    well_results = analysis$well_results,
    output_base = output_base
  )

  message("Analysis complete.")
  message("HTML report: ", output_files$html_file)
  message("TSV summary: ", output_files$tsv_file)

  invisible(
    list(
      plate_layout = plate_info$plate_layout,
      plate = plate_info$plate,
      plot_plate = plate_info$plot_plate,
      well_ids = plate_info$well_ids,
      sample_wells = plate_info$sample_wells,
      raw_data = reader_info$data,
      wells_final = reader_info$wells_final,
      well_results = analysis$well_results,
      inf_time = analysis$inf_time,
      well_table = analysis$well_table,
      sample_results = sample_results,
      output_files = output_files
    )
  )
}
