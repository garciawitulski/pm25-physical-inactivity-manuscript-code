# Manuscript Figure 5: distribution of productivity changes across strata.
code_root <- get_code_root()
source(file.path(code_root, "R", "common.R"))
load_reproduction_packages()
data_root <- get_data_root()
out_dir <- file.path(code_root, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
country_scenario <- readr::read_csv(
  file.path(data_root, "outputs", "derived", "pm25_country_scenario_figure_data.csv"),
  show_col_types = FALSE
)

plot_cost_distribution_alluvial_scenarios <- function(output_dir, country_scenario, stem = "figure_5_cost_distribution_alluvial") {
  scenario_label_levels_this <- get_scenario_label_levels(country_scenario$scenario)

  # Split country contributions by sign of the net cost change so the alluvial
  # can carry direction (savings vs additional costs) instead of just magnitude.
  base_df <- country_scenario |>
    dplyr::filter(!is.na(net_saved_costs), !is.na(region_wb), !is.na(income_group3)) |>
    dplyr::mutate(
      cost_sign = dplyr::if_else(net_saved_costs >= 0, "Savings", "Additional cost"),
      cost_sign = factor(cost_sign, levels = c("Savings", "Additional cost")),
      abs_cost_diff = abs(net_saved_costs)
    )

  region_levels <- base_df |>
    dplyr::group_by(region_wb) |>
    dplyr::summarise(total_costs = sum(abs_cost_diff, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_costs)) |>
    dplyr::pull(region_wb)

  # One ribbon per region x income x scenario cell (absolute magnitudes); the
  # savings-vs-added sign split is reported in figure 4, not drawn here.
  alluvial_df <- base_df |>
    dplyr::group_by(region_wb, income_group3, scenario_label) |>
    dplyr::summarise(abs_cost_diff = sum(abs_cost_diff, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      region_wb = factor(region_wb, levels = region_levels),
      income_group3 = factor(income_group3, levels = c("Low income", "Middle income", "High income")),
      scenario_label = factor(scenario_label, levels = scenario_label_levels_this)
    ) |>
    dplyr::filter(abs_cost_diff > 0)

  total_abs_all <- sum(alluvial_df$abs_cost_diff, na.rm = TRUE)
  total_savings <- sum(base_df$abs_cost_diff[base_df$cost_sign == "Savings"], na.rm = TRUE)
  total_additional <- sum(base_df$abs_cost_diff[base_df$cost_sign == "Additional cost"], na.rm = TRUE)

  income_levels <- levels(droplevels(alluvial_df$income_group3))
  scenario_nodes <- levels(droplevels(alluvial_df$scenario_label))
  region_nodes <- levels(droplevels(alluvial_df$region_wb))

  # Flows are coloured by origin region with a muted, colourblind-considerate
  # categorical palette, so each region's ribbon keeps its identity across the
  # three axes; strata are white with a thin grey outline. The savings-vs-added
  # split is carried by figure 4 and the caption, not by flow colour.
  # Hues deliberately avoid the scenario trio (azure, amber, crimson) so region
  # ribbons are never read as scenario colours: navy, teal, purple, rose,
  # green, olive, tan, slate.
  region_flow_palette_base <- c(
    "#1B5AA0", "#3D8B8B", "#8C6BB1", "#C26B8A",
    "#61A960", "#6E7F3E", "#A67C52", "#7A7F87"
  )
  region_palette <- stats::setNames(
    region_flow_palette_base[seq_along(region_nodes)],
    region_nodes
  )
  # Scenario names keep the co-benefit palette shared by manuscript figs 1-4
  # (blue = largest co-benefits, amber = intermediate, dark red = smallest),
  # applied to the stratum text rather than to heavy fills.
  scenario_cols <- c(
    "SSP1-2.6" = "#0072B2",
    "SSP2-4.5" = "#E69F00",
    "SSP5-8.5" = "#B2182B"
  )
  # Strata are filled solid (regions in their ribbon colour, income groups in a
  # light neutral, scenarios in the co-benefit palette); ribbons re-use the
  # region hue at lower alpha so each stratum visually feeds its own flows.
  inc_fill <- c(
    "Low income" = "#EDF0F3",
    "Middle income" = "#D4DAE1",
    "High income" = "#AEB8C2"
  )
  node_fill <- c(region_palette, inc_fill[income_levels], scenario_cols[scenario_nodes])

  # In-stratum label colours chosen for contrast against each fill.
  label_cols <- stats::setNames(
    rep("white", length(c(region_nodes, income_levels, scenario_nodes))),
    c(region_nodes, income_levels, scenario_nodes)
  )
  label_cols[income_levels] <- "#1F2933"
  label_cols["SSP2-4.5"] <- "#1F2933"

  region_summary <- alluvial_df |>
    dplyr::group_by(region_wb) |>
    dplyr::summarise(total_abs = sum(abs_cost_diff), .groups = "drop") |>
    dplyr::mutate(share = 100 * total_abs / total_abs_all)
  income_summary <- alluvial_df |>
    dplyr::group_by(income_group3) |>
    dplyr::summarise(total_abs = sum(abs_cost_diff), .groups = "drop") |>
    dplyr::mutate(share = 100 * total_abs / total_abs_all)
  scenario_summary <- alluvial_df |>
    dplyr::group_by(scenario_label) |>
    dplyr::summarise(total_abs = sum(abs_cost_diff), .groups = "drop") |>
    dplyr::mutate(share = 100 * total_abs / total_abs_all)

  fmt_usd <- function(x) {
    out <- character(length(x))
    finite_idx <- is.finite(x)
    big_idx <- finite_idx & abs(x) >= 1e9
    sml_idx <- finite_idx & !big_idx
    out[big_idx] <- sprintf("$%.1fB", x[big_idx] / 1e9)
    out[sml_idx] <- sprintf("$%.0fM", x[sml_idx] / 1e6)
    out
  }

  figure_base <- ggplot2::ggplot(
    alluvial_df,
    ggplot2::aes(
      axis1 = region_wb,
      axis2 = income_group3,
      axis3 = scenario_label,
      y     = abs_cost_diff
    )
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = region_wb),
      alpha    = 0.60,
      width    = 1 / 12,
      knot.pos = 0.38
    ) +
    ggalluvial::geom_stratum(
      ggplot2::aes(fill = ggplot2::after_stat(stratum)),
      width = 1 / 6, color = "white", linewidth = 0.55
    ) +
    ggplot2::scale_x_discrete(
      limits = c("Region", "Income group", "Scenario"),
      expand = c(0.10, 0.05),
      position = "top"
    ) +
    ggplot2::scale_y_continuous(expand = c(0.01, 0)) +
    # Legend restricted to the region entries only, so the neutral income greys
    # and the scenario co-benefit colours never appear as legend keys.
    ggplot2::scale_fill_manual(
      values = node_fill, breaks = region_nodes, name = NULL,
      labels = function(x) gsub(" \\(.*\\)$", "", x)
    ) +
    theme_pm25_paper(base_size = 9) +
    ggplot2::theme(
      axis.title      = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(size = 9.4, face = "bold", color = "#1F2933"),
      axis.text.y     = ggplot2::element_blank(),
      axis.line       = ggplot2::element_blank(),
      axis.ticks      = ggplot2::element_blank(),
      panel.grid      = ggplot2::element_blank(),
      panel.border    = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title    = ggplot2::element_blank(),
      legend.text     = ggplot2::element_text(size = 7.8, color = "#1F2933"),
      legend.key.height = grid::unit(0.32, "cm"),
      legend.key.width  = grid::unit(0.46, "cm"),
      legend.margin   = ggplot2::margin(t = 6, b = 0),
      plot.margin     = ggplot2::margin(8, 48, 8, 48)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                   override.aes = list(alpha = 1, color = NA))
    )

  # Clean stratum labels: node name only, no in-figure numeric annotations. The
  # shares and monetary values are carried by the caption and the burden table.
  # Labels for tall strata sit inside the box; labels for thin strata are
  # repelled outside (left of the region axis, right of the scenario axis) so
  # the small regions do not overprint each other.
  stratum_pos <- ggplot2::ggplot_build(figure_base)$data[[2]] |>
    tibble::as_tibble() |>
    dplyr::transmute(x = x, y = (ymin + ymax) / 2, h = ymax - ymin,
                     stratum = as.character(stratum)) |>
    dplyr::group_by(x) |>
    dplyr::mutate(frac = h / sum(h)) |>
    dplyr::ungroup()

  # All region labels sit outside-left of the axis (dark ink, right-aligned),
  # so long names never spill out of their coloured stratum; income and
  # scenario labels sit inside their strata when tall enough.
  labels_in <- stratum_pos |> dplyr::filter(frac >= 0.055, x > 1)
  labels_left <- stratum_pos |> dplyr::filter(x == 1 | (frac < 0.055 & x == 2))
  labels_right <- stratum_pos |> dplyr::filter(frac < 0.055, x == 3)

  figure_plot <- figure_base +
    ggplot2::geom_text(
      data = labels_in,
      ggplot2::aes(x = x, y = y, label = stratum, color = stratum),
      inherit.aes = FALSE, size = 2.7, fontface = "bold", lineheight = 0.95
    )
  if (nrow(labels_left) > 0) {
    figure_plot <- figure_plot +
      ggrepel::geom_text_repel(
        data = labels_left,
        ggplot2::aes(x = x - 0.115, y = y, label = stratum),
        inherit.aes = FALSE, direction = "y", hjust = 1,
        size = 2.35, fontface = "bold", color = "#1F2933",
        min.segment.length = 0, segment.color = scales::alpha("#8A94A0", 0.8),
        segment.size = 0.25, box.padding = 0.14, force = 0.6,
        max.overlaps = Inf, seed = 42
      )
  }
  if (nrow(labels_right) > 0) {
    figure_plot <- figure_plot +
      ggrepel::geom_text_repel(
        data = labels_right,
        ggplot2::aes(x = x + 0.115, y = y, label = stratum),
        inherit.aes = FALSE, direction = "y", hjust = 0,
        size = 2.35, fontface = "bold", color = "#1F2933",
        min.segment.length = 0, segment.color = scales::alpha("#8A94A0", 0.8),
        segment.size = 0.25, box.padding = 0.14, force = 0.6,
        max.overlaps = Inf, seed = 42
      )
  }
  figure_plot <- figure_plot +
    ggplot2::scale_color_manual(values = label_cols, guide = "none") +
    ggplot2::coord_cartesian(clip = "off")

  save_plot_set(
    plot_obj = figure_plot,
    stem = stem,
    output_dir = output_dir,
    width = 10.4,
    height = 7.4
  )

  figure_plot
}

plot_cost_distribution_alluvial_scenarios(
  output_dir = out_dir,
  country_scenario = country_scenario,
  stem = "figure_5_cost_distribution_alluvial"
)
message("Saved manuscript Figure 5.")
