# Manuscript Figure 1: IV identification, pooled estimates, and sector decomposition.
code_root <- get_code_root()
source(file.path(code_root, "R", "common.R"))
load_reproduction_packages()
data_root <- get_data_root()
paths <- list(root = data_root)
out_dir <- file.path(code_root, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("Writing manuscript Figure 1 to: ", out_dir)

model_levels <- c("OLS", "IV wind-bin", "IV directional", "Benchmark IV")
model_palette <- c(
  "OLS" = "#A3AAB6",
  "IV wind-bin" = "#6F7A89",
  "IV directional" = "#475569",
  "Benchmark IV" = "#163B73"
)
model_labels <- c(
  "Model 1 OLS lnpm25_10" = "OLS",
  "Model 3 IV wind" = "IV wind-bin",
  "Model 4 IV annual directional shift-share" = "IV directional",
  "Model 5 IV monthly wind shift-share" = "Benchmark IV"
)
outcome_labels <- c(PI0 = "Overall", PI1 = "Men", PI2 = "Women")
region_palette <- c(
  "East Asia & Pacific" = "#B97841",
  "Europe & Central Asia" = "#7D82B6",
  "Latin America & Caribbean" = "#4E958E",
  "Middle East & North Africa" = "#B76F99",
  "North America" = "#C7A34C",
  "South Asia" = "#9A7A59",
  "Sub-Saharan Africa" = "#86A95E"
)

coef_all <- readr::read_csv(
  file.path(paths$root, "coef_allmodels_pm25.csv"),
  show_col_types = FALSE
) |>
  normalize_pm25_model_labels()

coef_plot <- coef_all |>
  dplyr::filter(
    spec_type == "lnpm25_10",
    outcome %in% names(outcome_labels),
    model_label %in% names(model_labels)
  ) |>
  dplyr::mutate(
    model_short = factor(dplyr::recode(model_label, !!!model_labels),
                         levels = model_levels),
    outcome_label = factor(dplyr::recode(outcome, !!!outcome_labels),
                           levels = c("Overall", "Men", "Women")),
    ll = dplyr::if_else(is.na(ll), beta - 1.96 * se, ll),
    ul = dplyr::if_else(is.na(ul), beta + 1.96 * se, ul),
    value_label = sprintf("%+.2f", beta),
    is_benchmark = model_short == "Benchmark IV"
  )

bench_key <- "Model 5 IV monthly wind shift-share"
coef_bench <- coef_all |>
  dplyr::filter(
    outcome == "PI0",
    spec_type == "lnpm25_10",
    model_label == bench_key
  ) |>
  dplyr::slice(1)
beta_bench <- coef_bench$beta
se_bench <- coef_bench$se

# Benchmark coefficients by sex (used to add Men/Women dose-response curves
# alongside the Overall benchmark line in Panel B).
coef_bench_sex <- coef_all |>
  dplyr::filter(
    outcome %in% c("PI1", "PI2"),
    spec_type == "lnpm25_10",
    model_label == bench_key
  ) |>
  dplyr::transmute(
    outcome,
    sex_label = dplyr::recode(outcome, PI1 = "Men", PI2 = "Women"),
    beta = as.numeric(beta),
    se = as.numeric(se)
  )
sex_palette <- c("Men" = "#1F78B4", "Women" = "#B2182B", "Overall" = "#163B73")

panel_candidates <- Sys.glob(file.path(paths$root, "final_country_a*_panel.dta"))
if (length(panel_candidates) == 0) {
  stop("No processed country-year panel was found.")
}
panel_path <- panel_candidates[which.max(file.info(panel_candidates)$mtime)]
message("Using regression panel: ", basename(panel_path))
panel_df <- haven::read_dta(panel_path) |>
  dplyr::rename_with(trimws)
if (!"anio" %in% names(panel_df)) {
  year_name <- names(panel_df)[
    stringi::stri_trans_general(names(panel_df), "Latin-ASCII") == "ano"
  ]
  if (length(year_name) == 1) {
    panel_df <- dplyr::rename(panel_df, anio = !!rlang::sym(year_name))
  }
}
if (!"_m_shift" %in% names(panel_df)) {
  panel_df[["_m_shift"]] <- 3L
}

climate_vars <- c(
  "tmean_weighted", "precipitation_weighted", "cld_weighted",
  "frs_weighted", "vap_weighted", "wet_weighted"
)
socio_vars <- c("lnGDPpc", "deathr", "pop_urb")

pm_vec <- readr::read_csv(
  file.path(paths$root, "Data_base.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename_with(trimws) |>
  dplyr::filter(!is.na(pm25_mean), pm25_mean > 0) |>
  dplyr::pull(pm25_mean)

pm_q <- stats::quantile(pm_vec, probs = c(0.05, 0.95), na.rm = TRUE)
pm_min <- min(5, floor(pm_q[1]))
pm_max <- ceiling(pm_q[2])
pm_grid <- seq(pm_min, pm_max, by = 0.5)

coef_overall <- coef_all |>
  dplyr::filter(
    outcome == "PI0",
    spec_type == "lnpm25_10",
    model_label %in% names(model_labels)
  ) |>
  dplyr::mutate(model_short = factor(dplyr::recode(model_label, !!!model_labels),
                                     levels = model_levels))

curve_df <- tidyr::crossing(pm = pm_grid, coef_overall) |>
  dplyr::mutate(
    delta_x = log(pm / 5) / log(1.1),
    effect = beta * delta_x,
    eff_low = (beta - 1.96 * se) * delta_x,
    eff_high = (beta + 1.96 * se) * delta_x,
    is_bench = model_label == bench_key
  )

# Sex-specific benchmark curves (no CI ribbon, plotted as supporting lines so
# the heterogeneity from the abstract is visible in the figure itself).
sex_curve_df <- tidyr::crossing(pm = pm_grid, coef_bench_sex) |>
  dplyr::mutate(
    delta_x = log(pm / 5) / log(1.1),
    effect = beta * delta_x,
    sex_label = factor(sex_label, levels = c("Men", "Women"))
  )

needed <- c(
  "PI0", "lnpm25", "pm25_mean", climate_vars, socio_vars,
  "objectid", "anio", "_m_shift", "region_wb"
)
sample_df <- panel_df |>
  dplyr::mutate(lnpm25_10 = lnpm25 / log(1.1)) |>
  dplyr::filter(.data[["_m_shift"]] == 3) |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(needed), ~ !is.na(.x))) |>
  dplyr::filter(pm25_mean >= pm_min, pm25_mean <= pm_max)

# Full IV sample (no PM2.5 percentile restriction) used by the new Panel B
# binscatters and the IV dose-response. Includes the wind shift-share instrument.
needed_iv <- c(
  "PI0", "lnpm25", "pm25_mean", "lnz_wind", climate_vars, socio_vars,
  "objectid", "anio", "_m_shift"
)
sample_df_iv <- panel_df |>
  dplyr::mutate(lnpm25_10 = lnpm25 / log(1.1)) |>
  dplyr::filter(.data[["_m_shift"]] == 3) |>
  dplyr::filter(dplyr::if_all(dplyr::all_of(needed_iv), ~ !is.na(.x)))

nox_form <- stats::as.formula(paste0(
  "PI0 ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
  " | objectid + anio"
))
nox_fit <- fixest::feols(nox_form, data = sample_df, fixef.rm = "none")
resid_df <- sample_df |>
  dplyr::mutate(
    y_partial = as.numeric(stats::residuals(nox_fit)) +
      beta_bench * (lnpm25_10 - log(5) / log(1.1)),
    pm_bin = dplyr::ntile(pm25_mean, 20)
  )

binscatter_df <- resid_df |>
  dplyr::group_by(pm_bin) |>
  dplyr::summarise(
    pm_mid = mean(pm25_mean, na.rm = TRUE),
    y_mid = mean(y_partial, na.rm = TRUE),
    y_se = stats::sd(y_partial, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    y_lo = y_mid - 1.96 * y_se,
    y_hi = y_mid + 1.96 * y_se
  )

dens <- stats::density(sample_df$pm25_mean, from = pm_min, to = pm_max, n = 512)
y_lo0 <- min(stats::quantile(resid_df$y_partial, 0.01, na.rm = TRUE),
             min(curve_df$eff_low, na.rm = TRUE))
y_hi0 <- max(stats::quantile(resid_df$y_partial, 0.99, na.rm = TRUE),
             max(curve_df$eff_high, na.rm = TRUE))
span_y <- y_hi0 - y_lo0
y_min <- y_lo0 - 0.14 * span_y
y_max <- y_hi0 + 0.12 * span_y
band_h <- 0.07 * (y_max - y_min)
y_band0 <- y_min + 0.02 * (y_max - y_min)
dens_df <- tibble::tibble(pm = dens$x, d = dens$y) |>
  dplyr::mutate(d_scaled = y_band0 + (d / max(d, na.rm = TRUE)) * band_h)

anchor_df <- tibble::tibble(
  pm = c(5, 10, 15, 25),
  label = c("WHO 5", "10", "15", "25"),
  hjust = c(0, 0.5, 0.5, 0.5)
) |>
  dplyr::filter(pm >= pm_min, pm <= pm_max)

make_dose_panel <- function(colour_residuals = FALSE, title_suffix = "") {
  curve_sens <- curve_df |> dplyr::filter(!is_bench)
  curve_main <- curve_df |> dplyr::filter(is_bench)

  # Endpoint labels for the alternative-spec curves so the reader can tell
  # which grey line is which without having to consult panel A.
  curve_sens_labels <- curve_sens |>
    dplyr::group_by(model_short) |>
    dplyr::slice_max(pm, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(label = as.character(model_short))

  # Endpoint labels for the Men/Women curves (Overall reuses the annotation box).
  sex_curve_labels <- sex_curve_df |>
    dplyr::group_by(sex_label) |>
    dplyr::slice_max(pm, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 5, xmax = 10, ymin = -Inf, ymax = Inf,
                      fill = "#EEF2FF", alpha = 0.38) +
    ggplot2::annotate("rect", xmin = 10, xmax = 15, ymin = -Inf, ymax = Inf,
                      fill = "#FFF7ED", alpha = 0.34) +
    ggplot2::annotate("rect", xmin = 15, xmax = 25, ymin = -Inf, ymax = Inf,
                      fill = "#F8FAFC", alpha = 0.65) +
    ggplot2::geom_ribbon(
      data = dens_df,
      ggplot2::aes(x = pm, ymin = y_band0, ymax = d_scaled),
      fill = "#E5E7EB", color = NA
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "#6B7280", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 5, linetype = "dashed",
                        color = "#B91C1C", linewidth = 0.38) +
    ggplot2::geom_vline(
      data = anchor_df |> dplyr::filter(pm != 5),
      ggplot2::aes(xintercept = pm),
      linetype = "dotted", color = "#9CA3AF", linewidth = 0.25
    ) +
    ggplot2::geom_line(
      data = curve_sens,
      ggplot2::aes(x = pm, y = effect, group = model_short),
      color = "#9AA7B8", linewidth = 0.55, linetype = "longdash"
    ) +
    ggrepel::geom_text_repel(
      data = curve_sens_labels,
      ggplot2::aes(x = pm, y = effect, label = label),
      color = "#64748B", size = 2.05, fontface = "plain",
      direction = "y", hjust = 0, nudge_x = 0.5,
      segment.color = "#94A3B8", segment.size = 0.2, segment.alpha = 0.7,
      min.segment.length = 0, box.padding = 0.18, point.padding = 0.05,
      max.overlaps = Inf, seed = 17
    ) +
    ggplot2::geom_ribbon(
      data = curve_main,
      ggplot2::aes(x = pm, ymin = eff_low, ymax = eff_high),
      fill = "#163B73", alpha = 0.13
    ) +
    ggplot2::geom_line(
      data = curve_main,
      ggplot2::aes(x = pm, y = effect),
      color = "#163B73", linewidth = 1.15
    ) +
    # Sex-specific benchmark curves: dashed, thinner, anchored at the right
    # edge with sex labels so the heterogeneity is immediately visible. Colors
    # are hardcoded per geom_line so they don't conflict with the region color
    # scale used for the residualised scatter further down.
    ggplot2::geom_line(
      data = sex_curve_df |> dplyr::filter(sex_label == "Men"),
      ggplot2::aes(x = pm, y = effect, group = sex_label),
      color = sex_palette[["Men"]],
      linewidth = 0.75, linetype = "22"
    ) +
    ggplot2::geom_line(
      data = sex_curve_df |> dplyr::filter(sex_label == "Women"),
      ggplot2::aes(x = pm, y = effect, group = sex_label),
      color = sex_palette[["Women"]],
      linewidth = 0.75, linetype = "22"
    ) +
    ggrepel::geom_label_repel(
      data = sex_curve_labels |> dplyr::filter(sex_label == "Men"),
      ggplot2::aes(x = pm, y = effect, label = sex_label),
      color = sex_palette[["Men"]],
      size = 2.2, fontface = "bold",
      fill = scales::alpha("white", 0.92), label.size = 0,
      label.padding = grid::unit(0.13, "lines"),
      direction = "y", hjust = 0, nudge_x = 0.4,
      segment.color = NA, force = 0.3, seed = 19
    ) +
    ggrepel::geom_label_repel(
      data = sex_curve_labels |> dplyr::filter(sex_label == "Women"),
      ggplot2::aes(x = pm, y = effect, label = sex_label),
      color = sex_palette[["Women"]],
      size = 2.2, fontface = "bold",
      fill = scales::alpha("white", 0.92), label.size = 0,
      label.padding = grid::unit(0.13, "lines"),
      direction = "y", hjust = 0, nudge_x = 0.4,
      segment.color = NA, force = 0.3, seed = 19
    ) +
    ggplot2::geom_linerange(
      data = binscatter_df,
      ggplot2::aes(x = pm_mid, ymin = y_lo, ymax = y_hi),
      color = "#111827", linewidth = 0.28
    ) +
    ggplot2::geom_point(
      data = binscatter_df,
      ggplot2::aes(x = pm_mid, y = y_mid),
      color = "#111827", fill = "white", shape = 21,
      size = 1.85, stroke = 0.62
    ) +
    ggplot2::annotate(
      "label",
      x = pm_min + 0.45,
      y = y_max - 0.04 * span_y,
      label = sprintf(
        paste0(
          "Benchmark IV (Overall)\n%+.2f pp per 10%% PM2.5 increase\n95%% CI %.2f to %.2f\n",
          "Men: %+.2f   Women: %+.2f"
        ),
        beta_bench, beta_bench - 1.96 * se_bench, beta_bench + 1.96 * se_bench,
        coef_bench_sex$beta[coef_bench_sex$outcome == "PI1"],
        coef_bench_sex$beta[coef_bench_sex$outcome == "PI2"]
      ),
      hjust = 0, vjust = 1, size = 2.25, fontface = "bold",
      color = "#163B73", fill = scales::alpha("white", 0.94), label.padding = grid::unit(0.13, "lines")
    ) +
    ggplot2::geom_text(
      data = anchor_df,
      ggplot2::aes(x = pm, y = y_max + 0.02 * span_y, label = label, hjust = hjust),
      vjust = 0, size = 2.05, color = "#374151"
    ) +
    ggplot2::annotate(
      "text", x = pm_min + 0.45, y = y_band0 + 0.55 * band_h,
      label = "Observed PM2.5 density", hjust = 0,
      size = 1.9, color = "#6B7280"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(5 * ceiling(pm_min / 5), 5 * floor(pm_max / 5), by = 5),
      limits = c(pm_min - 0.35, pm_max + 4),
      expand = ggplot2::expansion(mult = c(0.005, 0.01)),
      name = expression(paste("Annual mean PM"[2.5], " (", mu, "g m"^{-3}, ")"))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max * 1.08),
      expand = ggplot2::expansion(mult = c(0.01, 0.04)),
      name = expression(paste(Delta, " inactivity vs PM"[2.5], " = 5 ", mu, "g/m"^{3}, " (pp)"))
    ) +
    ggplot2::labs(
      title = paste0("B | Benchmark IV dose-response", title_suffix),
      subtitle = NULL,
      caption = NULL
    ) +
    ggplot2::theme_classic(base_size = 8) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#111827"),
      plot.title = ggplot2::element_text(size = 10.5, face = "bold",
                                         color = "#111827"),
      plot.subtitle = ggplot2::element_text(size = 7.2, color = "#4B5563",
                                            margin = ggplot2::margin(b = 5)),
      plot.caption = ggplot2::element_text(size = 6.4, color = "#6B7280",
                                           hjust = 0, lineheight = 1.15,
                                           margin = ggplot2::margin(t = 4)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(size = 7.5),
      axis.text = ggplot2::element_text(size = 6.9),
      axis.line = ggplot2::element_line(linewidth = 0.28, color = "#111827"),
      axis.ticks = ggplot2::element_line(linewidth = 0.28, color = "#111827"),
      plot.margin = ggplot2::margin(7, 8, 6, 14)
    )

  if (colour_residuals) {
    # Region legend lives at the top of Panel A — suppress it here to avoid
    # duplication. We still use the region palette for the scatter colors.
    p <- p +
      ggplot2::geom_point(
        data = resid_df,
        ggplot2::aes(x = pm25_mean, y = y_partial, color = region_wb),
        size = 1.15, alpha = 0.30, stroke = 0, shape = 16,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = region_palette, name = "World Bank region", guide = "none") +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- p +
      ggplot2::geom_point(
        data = resid_df,
        ggplot2::aes(x = pm25_mean, y = y_partial),
        size = 1.05, alpha = 0.10, stroke = 0, shape = 16, color = "#334155"
      ) +
      ggplot2::theme(legend.position = "none")
  }

  p
}

# ---------------------------------------------------------------------------
# Panel B: IV identification + dose-response. Visualises the benchmark Model 5
# IV (instrument: lnz_wind, the monthly wind-adjusted shift-share). Mirrors the
# annotation density of the standalone options (a) and (e): in-plot slope and
# F boxes, equation subtitles, WHO threshold ticks, median/p95 markers, and
# Linear / Quadratic-at-median value tags.
# ---------------------------------------------------------------------------
make_iv_dose_panel <- function() {
  fs_form <- stats::as.formula(paste0(
    "lnpm25_10 ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
    " | objectid + anio"
  ))
  z_form  <- stats::as.formula(paste0(
    "lnz_wind ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
    " | objectid + anio"
  ))
  rf_form <- stats::as.formula(paste0(
    "PI0 ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
    " | objectid + anio"
  ))
  iv_form <- stats::as.formula(paste0(
    "PI0 ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
    " | objectid + anio | lnpm25_10 ~ lnz_wind"
  ))

  fs_fit <- fixest::feols(fs_form, data = sample_df_iv)
  z_fit  <- fixest::feols(z_form,  data = sample_df_iv)
  rf_fit <- fixest::feols(rf_form, data = sample_df_iv)
  iv_lin <- fixest::feols(iv_form, data = sample_df_iv, cluster = ~objectid)

  sd_q <- sample_df_iv |>
    dplyr::mutate(lnpm25_10_sq = lnpm25_10^2,
                  lnz_wind_sq  = lnz_wind^2)
  iv_q_form <- stats::as.formula(paste0(
    "PI0 ~ ", paste(c(climate_vars, socio_vars), collapse = " + "),
    " | objectid + anio | lnpm25_10 + lnpm25_10_sq ~ lnz_wind + lnz_wind_sq"
  ))
  iv_q <- tryCatch(
    fixest::feols(iv_q_form, data = sd_q, cluster = ~objectid),
    error = function(e) NULL
  )

  beta_lin <- as.numeric(iv_lin$coefficients[["fit_lnpm25_10"]])
  se_lin   <- as.numeric(summary(iv_lin)$coeftable["fit_lnpm25_10", "Std. Error"])
  fs_info <- tryCatch(fixest::fitstat(iv_lin, "ivf1"), error = function(e) NULL)
  fs_F_feols <- if (!is.null(fs_info) && !is.null(fs_info$ivf1$stat)) {
    as.numeric(fs_info$ivf1$stat)
  } else {
    NA_real_
  }
  kp_f_stata <- 59.79  # Published KP rk Wald F from Stata's ivreg2 (see 01_run_benchmark_iv.log)

  has_quad <- !is.null(iv_q) &&
    all(c("fit_lnpm25_10", "fit_lnpm25_10_sq") %in% names(iv_q$coefficients))
  if (has_quad) {
    bq1 <- as.numeric(iv_q$coefficients[["fit_lnpm25_10"]])
    bq2 <- as.numeric(iv_q$coefficients[["fit_lnpm25_10_sq"]])
    Vq  <- summary(iv_q)$cov.scaled[
      c("fit_lnpm25_10", "fit_lnpm25_10_sq"),
      c("fit_lnpm25_10", "fit_lnpm25_10_sq")
    ]
    p_nonlin <- as.numeric(summary(iv_q)$coeftable["fit_lnpm25_10_sq", "Pr(>|t|)"])
  }

  z_resid  <- as.numeric(stats::residuals(z_fit))
  pm_resid <- as.numeric(stats::residuals(fs_fit))
  pi_resid <- as.numeric(stats::residuals(rf_fit))
  fs_slope   <- stats::coef(stats::lm(pm_resid ~ z_resid))[["z_resid"]]
  rf_slope   <- stats::coef(stats::lm(pi_resid ~ z_resid))[["z_resid"]]
  iv_implied <- rf_slope / fs_slope

  binscatter <- function(x_resid, y_resid, n_bins = 30) {
    tibble::tibble(x = x_resid, y = y_resid) |>
      dplyr::filter(is.finite(x), is.finite(y)) |>
      dplyr::mutate(bin = dplyr::ntile(x, n_bins)) |>
      dplyr::group_by(bin) |>
      dplyr::summarise(
        x_mid = mean(x, na.rm = TRUE),
        y_mid = mean(y, na.rm = TRUE),
        n     = dplyr::n(),
        y_se  = stats::sd(y, na.rm = TRUE) / sqrt(n),
        .groups = "drop"
      ) |>
      dplyr::mutate(y_lo = y_mid - 1.96 * y_se,
                    y_hi = y_mid + 1.96 * y_se)
  }
  fs_bin <- binscatter(z_resid, pm_resid)
  rf_bin <- binscatter(z_resid, pi_resid)

  # Obradovich-style fan of fitted lines for the binscatter panels: each thin
  # line is one draw of (intercept, slope) from the OLS fit's joint coefficient
  # distribution on the residualised data (replaces the geom_smooth CI ribbon).
  fan_line_draws <- function(x, y, n_draw = 400, seed = 1) {
    d <- data.frame(x = x, y = y)
    d <- d[is.finite(d$x) & is.finite(d$y), ]
    ft <- stats::lm(y ~ x, data = d)
    V <- stats::vcov(ft)
    set.seed(seed)
    dr <- matrix(stats::rnorm(2 * n_draw), n_draw, 2) %*% chol(V)
    dr <- sweep(dr, 2, stats::coef(ft), "+")
    xr <- range(d$x)
    tibble::tibble(
      draw = rep(seq_len(n_draw), each = 2),
      x    = rep(xr, times = n_draw),
      y    = as.vector(rbind(dr[, 1] + dr[, 2] * xr[1],
                             dr[, 1] + dr[, 2] * xr[2]))
    )
  }
  fs_fan <- fan_line_draws(z_resid, pm_resid, seed = 20260711)
  rf_fan <- fan_line_draws(z_resid, pi_resid, seed = 20260712)

  # Theme matches the typography of clean_forest_panel (A) and sector_panel (C)
  # so the three panels feel visually coherent.
  sub_theme <- ggplot2::theme_classic(base_size = 9) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "#E6EAF0", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 8.7, face = "bold", color = "#1C2430",
                                          margin = ggplot2::margin(b = 3)),
      axis.title = ggplot2::element_text(size = 7.8, face = "bold", color = "#1C2430"),
      axis.text  = ggplot2::element_text(size = 7.2, color = "#4E5967"),
      axis.line  = ggplot2::element_line(linewidth = 0.28, color = "#111827"),
      axis.ticks = ggplot2::element_line(linewidth = 0.28, color = "#111827"),
      plot.margin = ggplot2::margin(4, 8, 4, 8)
    )

  fs_kp_txt <- sprintf("Kleibergen-Paap rk Wald F = %.1f", kp_f_stata)

  fs_plot <- ggplot2::ggplot(fs_bin, ggplot2::aes(x = x_mid, y = y_mid)) +
    ggplot2::geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 0, color = "#94A3B8", linewidth = 0.2) +
    ggplot2::geom_line(data = fs_fan,
                       ggplot2::aes(x = x, y = y, group = draw),
                       inherit.aes = FALSE,
                       color = "#163B73", alpha = 0.035, linewidth = 0.3) +
    ggplot2::geom_smooth(
      data = tibble::tibble(x = z_resid, y = pm_resid),
      ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
      method = "lm", formula = y ~ x, color = "#163B73",
      linewidth = 0.8, se = FALSE
    ) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = y_lo, ymax = y_hi),
                            color = "#1C2430", linewidth = 0.25) +
    ggplot2::geom_point(shape = 21, fill = "white", color = "#163B73",
                        stroke = 0.65, size = 1.9) +
    ggplot2::labs(
      x = "Imported-pollution index",
      y = expression(paste("Local ln(PM"[2.5], ")/log(1.1)  (residual)")),
      title = "First stage",
      subtitle = NULL
    ) +
    sub_theme +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 6.6, color = "#475569",
                                            margin = ggplot2::margin(b = 4))
    )

  rf_plot <- ggplot2::ggplot(rf_bin, ggplot2::aes(x = x_mid, y = y_mid)) +
    ggplot2::geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 0, color = "#94A3B8", linewidth = 0.2) +
    ggplot2::geom_line(data = rf_fan,
                       ggplot2::aes(x = x, y = y, group = draw),
                       inherit.aes = FALSE,
                       color = "#A6192E", alpha = 0.035, linewidth = 0.3) +
    ggplot2::geom_smooth(
      data = tibble::tibble(x = z_resid, y = pi_resid),
      ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
      method = "lm", formula = y ~ x, color = "#A6192E",
      linewidth = 0.8, se = FALSE
    ) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = y_lo, ymax = y_hi),
                            color = "#1C2430", linewidth = 0.25) +
    ggplot2::geom_point(shape = 21, fill = "white", color = "#A6192E",
                        stroke = 0.65, size = 1.9) +
    ggplot2::labs(
      x = "Imported-pollution index",
      y = "Physical inactivity (residual, pp)",
      title = "Reduced form",
      subtitle = NULL
    ) +
    sub_theme +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 6.6, color = "#475569",
                                            margin = ggplot2::margin(b = 4))
    )

  pm_med   <- as.numeric(stats::median(sample_df_iv$pm25_mean, na.rm = TRUE))
  pm_p95   <- as.numeric(stats::quantile(sample_df_iv$pm25_mean, 0.95, na.rm = TRUE))
  # Limit the grid to a small buffer above p95 -- extrapolating linear IV to PM
  # ~ 70 ug/m3 produces CI ribbons that exceed any plausible y-axis and clutter
  # the panel. Stopping near p95 keeps both curve and CI within the visible y.
  pm_grid_max <- ceiling(pm_p95) + 3
  pm_grid  <- seq(5, pm_grid_max, by = 0.5)
  ln5      <- log(5) / log(1.1)

  # Monte-Carlo coefficient draws for the Obradovich-style trajectory fans in
  # the cumulative- and marginal-effect panels (they replace the CI ribbons).
  # Draws are parametric: linear IV beta ~ N(beta_lin, se_lin^2); quadratic IV
  # (b1, b2) ~ MVN((bq1, bq2), Vq) with the cluster-robust vcov, via Cholesky.
  set.seed(20260710)
  n_draw <- 400
  lin_draws <- stats::rnorm(n_draw, beta_lin, se_lin)
  if (has_quad) {
    q_draws <- matrix(stats::rnorm(2 * n_draw), n_draw, 2) %*% chol(Vq)
    q_draws <- sweep(q_draws, 2, c(bq1, bq2), "+")
  }

  curve_df <- tibble::tibble(pm = pm_grid) |>
    dplyr::mutate(
      lnp10 = log(pm) / log(1.1),
      a = lnp10 - ln5,
      b = lnp10^2 - ln5^2,
      delta_lin = beta_lin * a,
      se_lin_d  = se_lin * abs(a),
      lo_lin = delta_lin - 1.96 * se_lin_d,
      hi_lin = delta_lin + 1.96 * se_lin_d
    )

  if (has_quad) {
    var_d <- with(curve_df,
                  a^2 * Vq[1, 1] + b^2 * Vq[2, 2] + 2 * a * b * Vq[1, 2])
    curve_df <- curve_df |>
      dplyr::mutate(
        delta_quad = bq1 * a + bq2 * b,
        se_quad_d  = sqrt(pmax(var_d, 0)),
        lo_quad    = delta_quad - 1.96 * se_quad_d,
        hi_quad    = delta_quad + 1.96 * se_quad_d
      )
  }

  marg_df <- tibble::tibble(pm = pm_grid) |>
    dplyr::mutate(
      lnp10 = log(pm) / log(1.1),
      me_lin = beta_lin,
      lo_lin = beta_lin - 1.96 * se_lin,
      hi_lin = beta_lin + 1.96 * se_lin
    )
  if (has_quad) {
    var_m <- with(marg_df,
                  Vq[1, 1] + 4 * lnp10^2 * Vq[2, 2] + 4 * lnp10 * Vq[1, 2])
    marg_df <- marg_df |>
      dplyr::mutate(
        me_quad   = bq1 + 2 * bq2 * lnp10,
        se_quad_m = sqrt(pmax(var_m, 0)),
        lo_quad   = me_quad - 1.96 * se_quad_m,
        hi_quad   = me_quad + 1.96 * se_quad_m
      )
  }

  # Long data for the draw fans (one thin curve per coefficient draw)
  if (has_quad) {
    a_vec <- curve_df$a; b_vec <- curve_df$b; ln_vec <- curve_df$lnp10
    cum_mat  <- q_draws %*% rbind(a_vec, b_vec)                    # n_draw x n_pm
    marg_mat <- matrix(q_draws[, 1], n_draw, length(ln_vec)) +
      q_draws[, 2] %*% t(2 * ln_vec)
    cum_fan <- tibble::tibble(
      draw = rep(seq_len(n_draw), each = length(pm_grid)),
      pm   = rep(pm_grid, times = n_draw),
      y    = as.vector(t(cum_mat))
    )
    marg_fan <- tibble::tibble(
      draw = rep(seq_len(n_draw), each = length(pm_grid)),
      pm   = rep(pm_grid, times = n_draw),
      y    = as.vector(t(marg_mat))
    )
  }
  lin_fan <- tibble::tibble(yd = lin_draws)

  # Pick y_top so the full Quadratic CI band fits with a generous visual margin.
  # The Linear IV does NOT use a ribbon in this panel (its CI grows linearly
  # with PM and would dominate the canvas), so y_top is driven by the quadratic
  # ribbon only, plus ~10 pp of breathing room.
  hi_quad_max <- if (has_quad) max(curve_df$hi_quad, na.rm = TRUE) else 0
  lo_quad_min <- if (has_quad) min(curve_df$lo_quad, na.rm = TRUE) else 0
  max_visible <- max(hi_quad_max, max(curve_df$delta_lin, na.rm = TRUE))
  min_visible <- min(lo_quad_min, 0)
  y_top <- ceiling((max_visible + 10) / 10) * 10
  y_bot <- floor((min_visible - 5) / 10) * 10
  # Defensive clip in case any value still escapes the chosen window.
  curve_df <- curve_df |>
    dplyr::mutate(
      lo_lin_c = pmax(lo_lin, y_bot),
      hi_lin_c = pmin(hi_lin, y_top),
      delta_lin_c = pmin(pmax(delta_lin, y_bot), y_top)
    )
  if (has_quad) {
    curve_df <- curve_df |>
      dplyr::mutate(
        lo_quad_c = pmax(lo_quad, y_bot),
        hi_quad_c = pmin(hi_quad, y_top),
        delta_quad_c = pmin(pmax(delta_quad, y_bot), y_top)
      )
  }

  thresh <- tibble::tribble(
    ~pm, ~label,
    5,   "WHO AQG",
    10,  "IT 4",
    15,  "IT 3",
    25,  "IT 2",
    35,  "IT 1"
  ) |> dplyr::filter(pm <= max(pm_grid))

  legend_levels <- c("log_pm_iv", "quadratic_iv")
  legend_labels <- expression(
    paste("Log(PM"[2.5], ") IV (constant semi-elasticity)"),
    "Quadratic IV"
  )

  cum_plot <- ggplot2::ggplot(curve_df, ggplot2::aes(x = pm)) +
    ggplot2::geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.25) +
    ggplot2::geom_vline(data = thresh, ggplot2::aes(xintercept = pm),
                        inherit.aes = FALSE,
                        color = "#B91C1C", linetype = "dotted",
                        linewidth = 0.3, alpha = 0.45) +
    ggplot2::geom_text(data = thresh,
                       ggplot2::aes(x = pm, y = y_top, label = label),
                       inherit.aes = FALSE,
                       angle = 90, hjust = 1.05, vjust = -0.4,
                       size = 2.0, color = "#B91C1C") +
    ggplot2::geom_vline(xintercept = pm_med, color = "#1B7837",
                        linetype = "dashed", linewidth = 0.45) +
    ggplot2::geom_vline(xintercept = pm_p95, color = "#1B7837",
                        linetype = "dashed", linewidth = 0.3, alpha = 0.5) +
    # Linear IV shown as a clean dashed reference line; its CI ribbon is
    # extremely wide at high PM2.5 (~+/-30 pp at p95) and would overflow the
    # y-axis. The numerical CI for the IV is shown in the panel subtitle.
    ggplot2::geom_line(ggplot2::aes(y = delta_lin_c,
                                     linetype = legend_levels[1]),
                       color = "#475569", linewidth = 0.7)

  if (has_quad) {
    cum_plot <- cum_plot +
      # Obradovich-style fan: one thin trajectory per joint coefficient draw
      # (replaces the delta-method CI ribbon; clipped by coord_cartesian)
      ggplot2::geom_line(data = cum_fan,
                         ggplot2::aes(x = pm, y = y, group = draw),
                         inherit.aes = FALSE,
                         color = "#163B73", alpha = 0.045, linewidth = 0.28) +
      ggplot2::geom_line(ggplot2::aes(y = delta_quad_c,
                                       linetype = legend_levels[2]),
                         color = "#163B73", linewidth = 1.05)
  }

  cum_plot <- cum_plot +
    ggplot2::annotate("text", x = pm_med, y = y_top - 2,
                      label = sprintf("median (%.1f)", pm_med),
                      angle = 90, hjust = 1, vjust = 1.3, size = 2.1,
                      color = "#1B7837", fontface = "bold") +
    ggplot2::annotate("text", x = pm_p95, y = y_top - 2,
                      label = sprintf("p95 (%.1f)", pm_p95),
                      angle = 90, hjust = 1, vjust = 1.3, size = 2.1,
                      color = "#1B7837", fontface = "bold") +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("dashed", "solid"), legend_levels),
      breaks = legend_levels,
      labels = legend_labels,
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(5, 10, 15, 25, 35, 50, 70),
      expand = ggplot2::expansion(mult = c(0.005, 0.02)),
      name = expression(paste("Annual mean PM"[2.5], " (", mu, "g m"^{-3}, ")"))
    ) +
    ggplot2::coord_cartesian(ylim = c(y_bot, y_top)) +
    ggplot2::labs(
      y = expression(paste(Delta, " inactivity vs WHO AQG (5 ", mu, "g/m"^{3}, "), pp")),
      title = "Cumulative effect (relative to WHO AQG)"
    ) +
    sub_theme +
    ggplot2::theme(
      legend.position = c(0.98, 0.04),
      legend.justification = c(1, 0),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.88),
                                                  color = NA),
      legend.key.width = grid::unit(1.0, "lines"),
      legend.key.height = grid::unit(0.30, "cm"),
      legend.text = ggplot2::element_text(size = 6.5),
      legend.margin = ggplot2::margin(1, 3, 1, 3)
    )

  if (has_quad) {
    me_range <- range(c(marg_df$lo_lin, marg_df$hi_lin,
                        marg_df$lo_quad, marg_df$hi_quad), na.rm = TRUE)
  } else {
    me_range <- range(c(marg_df$lo_lin, marg_df$hi_lin), na.rm = TRUE)
  }
  me_pad <- 0.22 * diff(me_range)  # 22% padding so the upper CI and the "Linear"/"Quadratic" annotation boxes have room

  ln_med  <- log(pm_med) / log(1.1)
  me_med_quad <- if (has_quad) bq1 + 2 * bq2 * ln_med else NA_real_
  se_med_quad <- if (has_quad) {
    sqrt(max(Vq[1, 1] + 4 * ln_med^2 * Vq[2, 2] + 4 * ln_med * Vq[1, 2], 0))
  } else NA_real_

  marg_plot <- ggplot2::ggplot(marg_df, ggplot2::aes(x = pm)) +
    ggplot2::geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.25) +
    ggplot2::geom_vline(data = thresh, ggplot2::aes(xintercept = pm),
                        inherit.aes = FALSE,
                        color = "#B91C1C", linetype = "dotted",
                        linewidth = 0.3, alpha = 0.45) +
    ggplot2::geom_vline(xintercept = pm_med, color = "#1B7837",
                        linetype = "dashed", linewidth = 0.45) +
    ggplot2::geom_vline(xintercept = pm_p95, color = "#1B7837",
                        linetype = "dashed", linewidth = 0.3, alpha = 0.5) +
    # Linear-IV fan: each draw has a constant marginal effect, so one thin
    # horizontal line per draw (replaces the grey CI ribbon)
    ggplot2::geom_segment(data = lin_fan,
                          ggplot2::aes(x = min(pm_grid), xend = max(pm_grid),
                                       y = yd, yend = yd),
                          inherit.aes = FALSE,
                          color = "#475569", alpha = 0.03, linewidth = 0.28) +
    ggplot2::geom_line(ggplot2::aes(y = me_lin),
                       color = "#475569", linetype = "dashed", linewidth = 0.7)

  # Top of the visible y for placing the two corner labels without overlap.
  marg_y_top <- me_range[2] + me_pad
  if (has_quad) {
    marg_plot <- marg_plot +
      ggplot2::geom_line(data = marg_fan,
                         ggplot2::aes(x = pm, y = y, group = draw),
                         inherit.aes = FALSE,
                         color = "#163B73", alpha = 0.045, linewidth = 0.28) +
      ggplot2::geom_line(ggplot2::aes(y = me_quad),
                         color = "#163B73", linewidth = 1.05) +
      ggplot2::annotate("point", x = pm_med, y = me_med_quad,
                        color = "#163B73", size = 2.4)
  }

  marg_plot <- marg_plot +
    ggplot2::scale_x_continuous(
      breaks = c(5, 10, 15, 25, 35, 50, 70),
      expand = ggplot2::expansion(mult = c(0.005, 0.02)),
      name = expression(paste("Annual mean PM"[2.5], " (", mu, "g m"^{-3}, ")"))
    ) +
    ggplot2::coord_cartesian(ylim = c(me_range[1] - me_pad, me_range[2] + me_pad)) +
    ggplot2::labs(
      y = "pp per +10% PM2.5",
      title = "Marginal effect by exposure level"
    ) +
    sub_theme

  panel_b_subtitle <- if (has_quad) {
    sprintf(
      paste0("Formal benchmark IV estimate: %+.2f percentage points per 10%% higher PM2.5 ",
             "(95%% CI %.2f to %.2f; SE %.2f). Quadratic-term P=%.2f."),
      beta_bench, beta_bench - 1.96 * se_bench, beta_bench + 1.96 * se_bench,
      se_bench, p_nonlin
    )
  } else {
    sprintf("IV beta = %+.2f pp per 10%% PM2.5 (SE %.2f).", beta_bench, se_bench)
  }

  panel_b <- (fs_plot + rf_plot) / (cum_plot + marg_plot) +
    patchwork::plot_layout(heights = c(1, 1)) +
    patchwork::plot_annotation(
      title = "A | Benchmark IV: identification and functional form",
      subtitle = NULL,
      caption = NULL,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold",
                                            color = "#1C2430",
                                            margin = ggplot2::margin(b = 2)),
        plot.subtitle = ggplot2::element_text(size = 7.4, color = "#475569",
                                              lineheight = 1.18,
                                              margin = ggplot2::margin(b = 6)),
        plot.caption = ggplot2::element_text(size = 6.4, color = "#6B7280",
                                              hjust = 0, lineheight = 1.15),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
    )

  panel_b
}

# F-statistics per specification (Kleibergen-Paap), used in Panel A.
fstat_lookup <- tibble::tibble(
  model_short = factor(c("OLS", "IV wind-bin", "IV directional", "Benchmark IV"),
                       levels = model_levels),
  fstat = c(NA_real_, 4.8, 37.8, 59.8),
  weak  = c(FALSE, TRUE, FALSE, FALSE)
)
fstat_label <- function(model, x_pos = 5.55) {
  row <- fstat_lookup |> dplyr::filter(model_short == model)
  if (is.na(row$fstat)) return(NA_character_)
  sprintf("F = %.1f", row$fstat)
}

bench_band_clean <- tibble::tibble(
  ymin = which(model_levels == "Benchmark IV") - 0.45,
  ymax = which(model_levels == "Benchmark IV") + 0.45
)

# Monte-Carlo draw clouds for the forest panels: with no continuous axis to
# fan across, each coefficient's sampling distribution is shown as a cloud of
# 400 translucent draws around the point estimate (replaces the CI whiskers).
set.seed(20260713)
forest_draws <- coef_plot |>
  dplyr::select(model_short, outcome_label, is_benchmark, beta, se) |>
  dplyr::group_by(model_short, outcome_label, is_benchmark) |>
  dplyr::reframe(bd = stats::rnorm(400, beta, se))

weak_iv_clean <- coef_plot |>
  dplyr::filter(model_short == "IV wind-bin") |>
  dplyr::distinct(outcome_label, model_short) |>
  dplyr::filter(as.character(outcome_label) == "Overall") |>
  dplyr::mutate(label = "limited first-stage strength (F=4.8)")

clean_forest_panel <- ggplot2::ggplot(
  coef_plot,
  ggplot2::aes(x = beta, y = model_short)
) +
  ggplot2::geom_rect(
    data = bench_band_clean,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#F0F5FB", color = NA, alpha = 0.9
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      color = "#B8C0CC", linewidth = 0.35) +
  ggplot2::geom_point(
    data = forest_draws,
    ggplot2::aes(x = bd, y = model_short, color = model_short),
    position = ggplot2::position_jitter(width = 0, height = 0.17, seed = 7),
    size = 0.3, alpha = 0.28, shape = 16
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = model_short, size = is_benchmark),
    shape = 21, color = "white", stroke = 0.75
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = 5.55, label = value_label, color = model_short),
    hjust = 1, size = 2.55, fontface = "bold"
  ) +
  ggplot2::facet_wrap(~ outcome_label, nrow = 1) +
  ggplot2::scale_color_manual(values = model_palette, guide = "none") +
  ggplot2::scale_fill_manual(values = model_palette, guide = "none") +
  ggplot2::scale_size_manual(values = c(`FALSE` = 2.8, `TRUE` = 4.0),
                             guide = "none") +
  ggplot2::scale_x_continuous(
    limits = c(-0.5, 5.75),
    breaks = 0:5,
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = "B | Pooled estimates across specifications and sex",
    subtitle = NULL,
    x = "Effect on physical inactivity (pp per 10% increase in PM2.5)",
    y = NULL,
    caption = NULL
  ) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(color = "#E6EAF0", linewidth = 0.25),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 7.7, face = "bold", color = "#1C2430"),
    axis.text.x = ggplot2::element_text(size = 7.2, color = "#4E5967"),
    axis.title.x = ggplot2::element_text(size = 7.8, face = "bold", color = "#1C2430"),
    strip.text = ggplot2::element_text(size = 8.7, face = "bold", color = "#1C2430"),
    strip.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 11, face = "bold", color = "#1C2430"),
    plot.subtitle = ggplot2::element_text(size = 7.5, color = "#404956",
                                          margin = ggplot2::margin(b = 6)),
    plot.caption = ggplot2::element_text(size = 6.6, color = "#6B7280",
                                         hjust = 0, lineheight = 1.12),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.spacing.x = grid::unit(1.0, "lines"),
    plot.margin = ggplot2::margin(6, 8, 6, 8)
  )

# Sector-specific IV sensitivity (Panel C). Each column of Table_IV_sources_PI0
# corresponds to a benchmark Model 5 IV using foreign emissions of a single
# source sector; values are reproduced here so the figure can stand alone.
sector_df <- tibble::tibble(
  sector = c("Agriculture", "Aviation", "Energy", "Industry",
             "Residential", "Transport", "Waste"),
  beta   = c(2.192, 3.811, 2.364, 2.337, 2.352, 3.067, 2.980),
  se     = c(0.651, 1.093, 0.796, 0.634, 0.930, 0.644, 1.020),
  fstat  = c(35.25, 25.02, 39.24, 43.95, 17.85, 55.62, 16.98)
) |>
  dplyr::mutate(
    ll = beta - 1.96 * se,
    ul = beta + 1.96 * se,
    weak = fstat < 10,
    value_label = sprintf("%+.2f", beta),
    fstat_label = sprintf("F=%.1f", fstat)
  ) |>
  dplyr::arrange(beta) |>
  dplyr::mutate(sector = factor(sector, levels = sector))

set.seed(20260714)
sector_draws <- sector_df |>
  dplyr::select(sector, beta, se) |>
  dplyr::group_by(sector) |>
  dplyr::reframe(bd = stats::rnorm(400, beta, se))

bench_x <- beta_bench  # vertical reference line at the Overall benchmark β

sector_panel <- ggplot2::ggplot(
  sector_df,
  ggplot2::aes(x = beta, y = sector)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      color = "#B8C0CC", linewidth = 0.35) +
  ggplot2::geom_vline(xintercept = bench_x, linetype = "dashed",
                      color = "#B91C1C", linewidth = 0.5, alpha = 0.8) +
  ggplot2::geom_point(
    data = sector_draws,
    ggplot2::aes(x = bd, y = sector),
    position = ggplot2::position_jitter(width = 0, height = 0.17, seed = 7),
    color = "#163B73", size = 0.3, alpha = 0.25, shape = 16,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_point(
    shape = 21, fill = "#163B73", color = "white", stroke = 0.8, size = 3.2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = 6.8, label = value_label),
    hjust = 1, size = 2.5, fontface = "bold", color = "#1C2430"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = 6.9, label = fstat_label,
                 color = ifelse(weak, "#B91C1C", "#475569")),
    hjust = 0, size = 2.1, fontface = "plain"
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_x_continuous(
    limits = c(-0.5, 8.3),
    breaks = 0:6,
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = "C | Sector decomposition of the benchmark IV (Overall)",
    subtitle = NULL,
    x = "Effect on physical inactivity (pp per 10% increase in PM2.5)",
    y = NULL,
    caption = NULL
  ) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(color = "#E6EAF0", linewidth = 0.25),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 7.7, face = "bold", color = "#1C2430"),
    axis.text.x = ggplot2::element_text(size = 7.2, color = "#4E5967"),
    axis.title.x = ggplot2::element_text(size = 7.8, face = "bold", color = "#1C2430"),
    plot.title = ggplot2::element_text(size = 11, face = "bold", color = "#1C2430"),
    plot.subtitle = ggplot2::element_text(size = 7.5, color = "#404956",
                                          margin = ggplot2::margin(b = 6)),
    plot.caption = ggplot2::element_text(size = 6.6, color = "#6B7280",
                                         hjust = 0, lineheight = 1.12),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(6, 8, 6, 8)
  )


# Assemble and export only the version used in the main manuscript.
v3_triple <- patchwork::wrap_elements(full = make_iv_dose_panel()) /
  clean_forest_panel /
  sector_panel +
  patchwork::plot_layout(heights = c(1.85, 0.60, 0.55))

save_plot_set(
  v3_triple,
  "figure_1_regression_models",
  out_dir,
  width = 7.8,
  height = 13.2
)
message("Saved manuscript Figure 1.")
