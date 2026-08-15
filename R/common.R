required_packages <- c(
  "dplyr", "fixest", "ggalluvial", "ggplot2", "ggrepel", "haven",
  "patchwork", "readr", "rlang", "scales", "stringi", "tibble", "tidyr"
)

load_reproduction_packages <- function() {
  missing <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Run `Rscript requirements.R` first.",
      call. = FALSE
    )
  }
  invisible(lapply(required_packages, library, character.only = TRUE))
}

get_code_root <- function() {
  root <- Sys.getenv("PM25_CODE_ROOT", unset = "")
  if (!nzchar(root)) {
    stop("PM25_CODE_ROOT is not set. Run the figures through `Rscript run_all.R`.", call. = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

get_data_root <- function() {
  root <- Sys.getenv("PM25_DATA_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- file.path(get_code_root(), "data")
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

validate_required_inputs <- function(data_root) {
  required <- c(
    "Data_base.csv",
    "coef_allmodels_pm25.csv",
    "PM25_mensual_pop_long.csv",
    file.path("outputs", "derived", "pm25_country_scenario_figure_data.csv"),
    file.path("outputs", "derived", "msa_temperature_style_previews", "world_coords.csv"),
    file.path("outputs", "derived", "msa_temperature_style_previews", "world_attributes.csv"),
    file.path(
      "outputs", "alternative_burden_ccpi_style_histPM_PI2020", "final",
      "global_summary_histPM_PI2020.csv"
    )
  )
  missing <- required[!file.exists(file.path(data_root, required))]
  panels <- Sys.glob(file.path(data_root, "final_country_a*_panel.dta"))
  if (length(panels) == 0) {
    missing <- c(missing, "final_country_a*_panel.dta")
  }
  if (length(missing) > 0) {
    stop("Missing required inputs:\n- ", paste(missing, collapse = "\n- "), call. = FALSE)
  }
  invisible(TRUE)
}

normalize_pm25_model_labels <- function(df) {
  if (!"model_label" %in% names(df)) return(df)
  df |>
    dplyr::mutate(
      model_label = dplyr::case_when(
        startsWith(model_label, "Model 4 IV annual directional") ~
          "Model 4 IV annual directional shift-share",
        startsWith(model_label, "Model 5 IV monthly wind shift") ~
          "Model 5 IV monthly wind shift-share",
        model_label == "Model 4 IV shift geom" ~
          "Model 4 IV annual directional shift-share",
        model_label == "Model 5 IV shift upwind" ~
          "Model 5 IV monthly wind shift-share",
        TRUE ~ model_label
      )
    )
}

scenario_order_lookup <- c(RCP26 = 26, RCP45 = 45, RCP85 = 85)
scenario_label_lookup <- c(
  RCP26 = "SSP1-2.6",
  RCP45 = "SSP2-4.5",
  RCP85 = "SSP5-8.5"
)

get_scenario_levels <- function(scenarios) {
  scenarios <- unique(as.character(scenarios))
  scenarios <- scenarios[!is.na(scenarios) & scenarios %in% names(scenario_order_lookup)]
  scenarios[order(scenario_order_lookup[scenarios])]
}

get_scenario_label_levels <- function(scenarios) {
  unname(scenario_label_lookup[get_scenario_levels(scenarios)])
}

theme_pm25_paper <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.25),
      axis.ticks.length = grid::unit(2, "pt"),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey75"),
      strip.text = ggplot2::element_text(face = "bold", color = "black"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = base_size + 1),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = base_size),
      plot.caption = ggplot2::element_text(hjust = 0, size = base_size - 1),
      axis.title = ggplot2::element_text(color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

save_plot_set <- function(plot_obj, stem, output_dir, width, height) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".pdf")), plot_obj,
    width = width, height = height, units = "in", device = grDevices::cairo_pdf
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".png")), plot_obj,
    width = width, height = height, units = "in", dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".tiff")), plot_obj,
    width = width, height = height, units = "in", dpi = 600,
    compression = "lzw", bg = "white", device = "tiff"
  )
}
