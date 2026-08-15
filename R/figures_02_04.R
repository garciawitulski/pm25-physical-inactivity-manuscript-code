# Manuscript Figures 2--4: exposure trajectories, burden maps, and regional burden.
options(stringsAsFactors = FALSE)
code_root <- get_code_root()
source(file.path(code_root, "R", "common.R"))
data_root <- get_data_root()
root <- data_root
fig_dir <- file.path(code_root, "outputs")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Style + helpers (BMJ style shared with the replication scripts)
# ---------------------------------------------------------------------------
BLUE <- "#2A6EBB"; TEAL <- "#3FA396"; MAGENTA <- "#C6417F"; GOLD <- "#E8A013"
INK <- "#1a1a1a"; INK2 <- "#4d4d4d"; MUTED <- "#7a7a7a"
GRID_HAIR <- "#ececec"; NULL_LINE <- "#333333"; SPINE <- "#8c8c8c"; LAND_NA <- "#D6D6D6"
PT_GRAY <- "#AEB8C2"   # neutral data-point fill for binscatters

SCEN_ORDER <- c("RCP26", "RCP45", "RCP85")
SCEN_LABS  <- c(RCP26 = "SSP1-2.6", RCP45 = "SSP2-4.5", RCP85 = "SSP5-8.5")
# scenario colours graded by co-benefit: blue = largest projected co-benefits,
# amber = intermediate, dark red = smallest; matches the blue = benefit /
# red = burden semantics of the diverging maps (identity always doubled by an
# explicit legend or panel title; hues are CVD-separable with distinct lightness)
FAN_STROKE <- c(RCP26 = "#0072B2", RCP45 = "#E69F00", RCP85 = "#B2182B")
FAN_ALPHA  <- c(RCP26 = 0.32, RCP45 = 0.50, RCP85 = 0.30)
OBS_LINE   <- "#ABABAB"   # observed-history hairlines (grey, so amber stays free)

pal_div         <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))
pal_div_benefit <- grDevices::colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))
pal_seq_pi <- grDevices::colorRampPalette(c("#FCEFF6", "#DA9BBC", "#8E2F62"))
pal_seq_pm <- grDevices::colorRampPalette(c("#FDF4DE", "#E8A013", "#6E4A05"))

REF_PM <- 5
xlab_pm <- expression(paste("Population-weighted PM"[2.5], " (", mu, "g/m"^3, ")"))
ylab_pp <- "Percentage-point difference in physical inactivity (95% CI)"

read_csv0 <- function(path) utils::read.csv(path, check.names = FALSE, na.strings = c("", "NA", "."), comment.char = "")
alpha_col <- function(col, a) grDevices::adjustcolor(col, alpha.f = a)
num <- function(x) suppressWarnings(as.numeric(x))

manifest <- data.frame(stem = character(), pdf = character(), png = character())
save_figure <- function(stem, width, height, plot_fun) {
  pdf_path <- file.path(fig_dir, paste0(stem, ".pdf"))
  png_path <- file.path(fig_dir, paste0(stem, ".png"))
  grDevices::cairo_pdf(pdf_path, width = width, height = height, family = "Arial")
  plot_fun(); grDevices::dev.off()
  grDevices::png(png_path, width = width, height = height, units = "in", res = 300,
                 type = "cairo-png", family = "Arial", bg = "white")
  plot_fun(); grDevices::dev.off()
  cat("wrote", stem, "\n")
  manifest <<- rbind(manifest, data.frame(stem = stem, pdf = pdf_path, png = png_path))
}

style_panel <- function(xlim, ylim, xat = NULL, yat = NULL, xlab = "", ylab = "",
                        grid_y = TRUE, xlabels = TRUE, cex_lab = 0.78) {
  plot.new(); plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "r")
  if (is.null(xat)) xat <- pretty(xlim, 6)
  if (is.null(yat)) yat <- pretty(ylim, 5)
  if (grid_y) abline(h = yat, col = GRID_HAIR, lwd = 0.7)
  axis(1, at = xat, labels = if (isTRUE(xlabels)) xat else FALSE, col = SPINE, col.ticks = SPINE,
       col.axis = INK2, cex.axis = 0.82, tck = -0.02, mgp = c(2, 0.45, 0), lwd = 0.8)
  axis(2, at = yat, col = SPINE, col.ticks = SPINE, col.axis = INK2,
       cex.axis = 0.82, tck = -0.02, mgp = c(2, 0.55, 0), las = 1, lwd = 0.8)
  if (nzchar(as.character(xlab)[1]) || is.expression(xlab)) mtext(xlab, side = 1, line = 1.9, cex = cex_lab, col = INK2)
  if (nzchar(as.character(ylab)[1]) || is.expression(ylab)) mtext(ylab, side = 2, line = 2.45, cex = cex_lab, col = INK2)
  invisible(list(xat = xat, yat = yat))
}

panel_letter <- function(txt, adj_x = -0.14, line = 0.55) {
  txt <- toupper(txt)
  mtext(txt, side = 3, line = line, at = par("usr")[1] + adj_x * diff(par("usr")[1:2]),
        font = 2, cex = 1.05, col = INK)
}

fig_note <- function(txt, cex = 0.6, start = 0.15, step = 0.72) {
  wrap_chars <- max(80, floor(grDevices::dev.size("in")[1] * 17.5))
  lines <- strwrap(txt, width = wrap_chars)
  for (i in seq_along(lines)) {
    mtext(lines[i], side = 1, line = start + (i - 1) * step, outer = TRUE,
          adj = 0.01, cex = cex, col = MUTED)
  }
}

wq <- function(x, w, p) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(p, function(pp) x[which(cw >= pp)[1]])
}
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  sum(x[ok] * w[ok]) / sum(w[ok])
}

reg_short <- function(x) {
  x <- gsub(" \\(.*\\)$", "", x)
  x <- gsub("Latin America & Caribbean", "Lat. America & Carib.", x)
  x <- gsub("Europe & Central Asia", "Europe & Cent. Asia", x)
  x
}

dr_curve <- function(x, beta, se, ref = REF_PM) {
  d <- log(x / ref) / log(1.1)
  list(est = beta * d, lo = beta * d - 1.96 * se * abs(d), hi = beta * d + 1.96 * se * abs(d))
}

# ---------------------------------------------------------------------------
# Robinson projection + map support
# ---------------------------------------------------------------------------
rob_lat <- seq(0, 90, by = 5)
rob_X <- c(1.0000, 0.9986, 0.9954, 0.9900, 0.9822, 0.9730, 0.9600, 0.9427, 0.9216,
           0.8962, 0.8679, 0.8350, 0.7986, 0.7597, 0.7186, 0.6732, 0.6213, 0.5722, 0.5322)
rob_Y <- c(0.0000, 0.0620, 0.1240, 0.1860, 0.2480, 0.3100, 0.3720, 0.4340, 0.4958,
           0.5571, 0.6176, 0.6769, 0.7346, 0.7903, 0.8435, 0.8936, 0.9394, 0.9761, 1.0000)
rob_fx <- stats::splinefun(rob_lat, rob_X, method = "natural")
rob_fy <- stats::splinefun(rob_lat, rob_Y, method = "natural")
project_robinson <- function(lon, lat) {
  list(x = 0.8487 * rob_fx(abs(lat)) * lon * pi / 180,
       y = 1.3523 * sign(lat) * rob_fy(abs(lat)))
}

cat("loading map support...\n")
prev_dir <- file.path(root, "outputs", "derived", "msa_temperature_style_previews")
wc <- read_csv0(file.path(prev_dir, "world_coords.csv"))
wa <- read_csv0(file.path(prev_dir, "world_attributes.csv"))
wc$lon <- num(wc$lon); wc$lat <- num(wc$lat); wc$`_ID` <- as.integer(wc$`_ID`)
wa$id <- as.integer(wa$id)
wc <- wc[wc$`_ID` %in% wa$id[wa$CONTINENT != "Antarctica"], ]

proj_parts <- list()
for (dat in split(wc, wc$`_ID`)) {
  id <- as.character(dat$`_ID`[1])
  miss <- is.na(dat$lon) | is.na(dat$lat)
  grp <- cumsum(miss)
  parts <- split(dat[!miss, c("lon", "lat")], grp[!miss])
  pp <- list()
  for (p in parts) {
    if (nrow(p) >= 3) {
      pr <- project_robinson(p$lon, p$lat)
      pp[[length(pp) + 1]] <- cbind(pr$x, pr$y)
    }
  }
  if (length(pp) > 0) proj_parts[[id]] <- pp
}
map_xlim <- range(unlist(lapply(proj_parts, function(pl) lapply(pl, function(m) range(m[, 1])))))
map_ylim <- range(unlist(lapply(proj_parts, function(pl) lapply(pl, function(m) range(m[, 2])))))

draw_world <- function(fill_by_id, border = "white", lwd = 0.18) {
  plot.new()
  plot.window(xlim = map_xlim, ylim = map_ylim, asp = 1, xaxs = "i", yaxs = "i")
  for (nm in names(proj_parts)) {
    col <- fill_by_id[[nm]]
    if (is.null(col) || is.na(col)) col <- LAND_NA
    for (m in proj_parts[[nm]]) polygon(m[, 1], m[, 2], col = col, border = border, lwd = lwd)
  }
}

map_fill <- function(objectid, values, zlim, palette_fun, n = 256) {
  pal <- palette_fun(n)
  v <- pmax(zlim[1], pmin(zlim[2], values))
  idx <- pmax(1L, pmin(n, floor((v - zlim[1]) / diff(zlim) * (n - 1)) + 1L))
  setNames(as.list(pal[idx]), as.character(objectid))
}

colorbar_h <- function(x0, x1, y0, y1, zlim, palette_fun, title = "", diverging = FALSE,
                       cex = 0.62, n = 256, fmt = "%.0f", truncated = FALSE,
                       dir_labels = NULL, nodata = FALSE, title_offset = 1.15) {
  pal <- palette_fun(n)
  xs <- seq(x0, x1, length.out = n + 1)
  for (i in seq_len(n)) rect(xs[i], y0, xs[i + 1], y1, col = pal[i], border = NA)
  rect(x0, y0, x1, y1, col = NA, border = SPINE, lwd = 0.5)
  labs_at <- if (diverging) c(zlim[1], 0, zlim[2]) else c(zlim[1], zlim[2])
  for (lv in labs_at) {
    lx <- x0 + (lv - zlim[1]) / diff(zlim) * (x1 - x0)
    segments(lx, y0, lx, y0 - 0.25 * (y1 - y0), col = SPINE, lwd = 0.5)
    lab <- sprintf(fmt, lv)
    # censored scale: flag that end values extend beyond the bar limits
    if (truncated && lv == zlim[1]) lab <- paste0("≤", lab)
    if (truncated && lv == zlim[2]) lab <- paste0("≥", lab)
    text(lx, y0 - 1.05 * (y1 - y0), lab, cex = cex, col = INK2, xpd = NA)
  }
  if (!is.null(dir_labels)) {
    text(x0, y1 + 0.55 * (y1 - y0), dir_labels[1], cex = cex - 0.04, col = "#B2182B",
         adj = c(0, 0), font = 3, xpd = NA)
    text(x1, y1 + 0.55 * (y1 - y0), dir_labels[2], cex = cex - 0.04, col = "#2166AC",
         adj = c(1, 0), font = 3, xpd = NA)
  }
  if (nodata) {
    sw <- 0.35 * (y1 - y0)
    sx <- x1 + 0.055 * (x1 - x0)
    rect(sx, (y0 + y1) / 2 - sw, sx + 0.014, (y0 + y1) / 2 + sw,
         col = LAND_NA, border = SPINE, lwd = 0.4, xpd = NA)
    text(sx + 0.019, (y0 + y1) / 2, "No data", cex = cex - 0.02, col = INK2,
         adj = c(0, 0.5), xpd = NA)
  }
  if (nzchar(title)) text((x0 + x1) / 2, y1 + title_offset * (y1 - y0), title,
                          cex = cex + 0.04, col = INK2, xpd = NA)
}

# ---------------------------------------------------------------------------
cat("loading data...\n")
panel <- read_csv0(file.path(root, "Data_base.csv"))
scen  <- read_csv0(file.path(root, "outputs", "derived", "pm25_country_scenario_figure_data.csv"))
glob  <- read_csv0(file.path(root, "outputs", "alternative_burden_ccpi_style_histPM_PI2020",
                             "final", "global_summary_histPM_PI2020.csv"))
mon   <- read_csv0(file.path(root, "PM25_mensual_pop_long.csv"))

for (v in c("PI0", "PI1", "PI2", "pm25", "pm25_mean", "lnz_wind", "lnpm25_10")) panel[[v]] <- num(panel[[v]])
panel$objectid <- as.integer(panel$objectid); panel$año <- as.integer(panel$año)
for (v in c("pop_est", "pm25_obs_2020_ugm3", "pm25_proj2050_ugm3", "pm25_change_factor",
            "PI2020_pct", "PI2050_pct", "deaths_per_100k", "costs_share_gdp",
            "delta_pi_pp_PI0", "delta_pi_pp_PI1", "delta_pi_pp_PI2",
            "pi_reduction_pp_PI0", "pi_reduction_pp_PI1", "pi_reduction_pp_PI2",
            "pi_reduction_pp_ci_low_PI0", "pi_reduction_pp_ci_high_PI0",
            "pi_reduction_pp_ci_low_PI1", "pi_reduction_pp_ci_high_PI1",
            "pi_reduction_pp_ci_low_PI2", "pi_reduction_pp_ci_high_PI2",
            "pproj_bounded_beta_low_PI0", "pproj_bounded_beta_high_PI0",
            "PI2050_pct_ci_low", "PI2050_pct_ci_high",
            "avoidable_deaths_2050", "saved_costs_2050",
            "avoidable_deaths_2050_ci_low", "avoidable_deaths_2050_ci_high",
            "saved_costs_2050_ci_low", "saved_costs_2050_ci_high",
            "gdp_total")) scen[[v]] <- num(scen[[v]])
scen$objectid <- as.integer(scen$objectid)
glob$delta_x <- num(glob$delta_x)
mon$pm25_pop_weighted <- num(mon$pm25_pop_weighted); mon$POP_EST <- num(mon$POP_EST)
mon$OBJECTID <- as.integer(mon$OBJECTID)

pop_by_obj <- aggregate(list(pop = scen$pop_est), by = list(objectid = scen$objectid), FUN = max)

# Five-year monthly exposure window ending with the estimation period.
mrec <- mon[mon$year >= 2016 & mon$year <= 2020 &
              is.finite(mon$pm25_pop_weighted) & is.finite(mon$POP_EST), ]

# =============================================================================
# figM2 - from exposure to inactivity, per scenario (manuscript Figure 2)
#   a-c: observed country PM2.5 trajectories 2000-2020 + projected paths to
#        2050 (per scenario columns)
#   d-f: observed country PI prevalence 2000-2020 + projected 2050 levels
#        (bounded-logit route), column-aligned with a-c
#   g:   Monte-Carlo wedge of the projected global bounded reduction
# =============================================================================
save_figure("figM2_exposure_pi_projection", 12.6, 11.4, function() {
  layout(rbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)), heights = c(1, 1.05, 1))
  par(oma = c(0.9, 0.6, 0.6, 0.3))

  # --- a-c: exposure fans
  # NOTE: the observed history uses the `pm25` annual country series, i.e. the
  # SAME series that the projection route anchors on (pm25_obs_2020 == pm25 at
  # 2020), so projected paths start exactly where the observed lines end.
  # The estimation exposure (pm25_mean) is a different aggregation.
  par(mar = c(2.9, 3.9, 1.8, 1.3))
  obs <- panel[is.finite(panel$pm25), c("objectid", "año", "pm25")]
  obs <- merge(obs, pop_by_obj, by = "objectid", all.x = TRUE)
  yrs <- sort(unique(obs$año))
  med_obs <- sapply(yrs, function(y) { dd <- obs[obs$año == y, ]; wq(dd$pm25, dd$pop, 0.5) })
  # Show every observed trajectory and projected endpoint.  The previous
  # 99.5th-percentile cap (about 80.5 ug/m3) clipped observed country-years
  # that reach 101.5 ug/m3 in panels a-c.
  fan_pm_max <- max(
    obs$pm25,
    scen$pm25_obs_2020_ugm3,
    scen$pm25_proj2050_ugm3,
    na.rm = TRUE
  )
  ylf <- c(0, ceiling((fan_pm_max * 1.03) / 5) * 5)
  letters_ <- c(RCP26 = "a", RCP45 = "b", RCP85 = "c")

  for (s in SCEN_ORDER) {
    d <- scen[scen$scenario == s & is.finite(scen$pm25_obs_2020_ugm3) &
                is.finite(scen$pm25_proj2050_ugm3) & is.finite(scen$pop_est), ]
    style_panel(c(2000, 2052.5), ylf, xat = c(2000, 2010, 2020, 2030, 2040, 2050),
                xlab = "Year", ylab = if (s == "RCP26") xlab_pm else "")
    abline(v = 2020, col = "#bfbfbf", lwd = 0.9, lty = "13")
    for (dd in split(obs, obs$objectid)) {
      dd <- dd[order(dd$año), ]
      lines(dd$año, dd$pm25, col = alpha_col(OBS_LINE, 0.45), lwd = 0.55)
    }
    segments(rep(2020, nrow(d)), d$pm25_obs_2020_ugm3, rep(2050, nrow(d)), d$pm25_proj2050_ugm3,
             col = alpha_col(FAN_STROKE[[s]], FAN_ALPHA[[s]]), lwd = 0.55)
    lines(yrs, med_obs, col = INK, lwd = 1.9)
    qq <- wq(d$pm25_proj2050_ugm3, d$pop_est, c(0.10, 0.50, 0.90))
    lines(c(2020, 2050), c(med_obs[yrs == 2020], qq[2]), col = INK, lwd = 1.9, lty = "42")
    segments(2050, qq[1], 2050, qq[3], col = INK, lwd = 2.4)
    points(2050, qq[2], pch = 16, col = INK, cex = 0.8)
    text(2050.9, qq[2], sprintf("%.0f", qq[2]), cex = 0.72, col = INK, adj = c(0, 0.5), xpd = NA)
    mtext(SCEN_LABS[[s]], side = 3, line = 0.55, cex = 0.82, col = INK, font = 2)
    panel_letter(letters_[[s]], adj_x = -0.13)
  }

  # data prep for the PI panels (drawn in row 3, and used by panel f)
  d_all <- scen[is.finite(scen$PI2020_pct) & is.finite(scen$PI2050_pct) & is.finite(scen$pop_est), ]
  proj_ids <- unique(d_all$objectid)
  obs_pi <- panel[is.finite(panel$PI0) & panel$objectid %in% proj_ids, c("objectid", "año", "PI0")]
  obs_pi <- merge(obs_pi, pop_by_obj, by = "objectid", all.x = TRUE)
  mean_pi <- sapply(yrs, function(y) { dd <- obs_pi[obs_pi$año == y, ]; wmean(dd$PI0, dd$pop) })
  ylp <- range(obs_pi$PI0, d_all$PI2050_pct, na.rm = TRUE) + c(-1, 1)
  col_dn <- "#2166AC"; col_up <- "#B2182B"

  # --- d: exposure-distribution shift under all three scenarios
  par(mar = c(3.1, 3.9, 1.8, 0.9))
  binw <- 2.5
  brk <- seq(0, ceiling(max(mrec$pm25_pop_weighted, na.rm = TRUE) / binw) * binw, by = binw)
  wobs <- tapply(mrec$POP_EST, cut(mrec$pm25_pop_weighted, brk), sum)
  wobs[is.na(wobs)] <- 0; wobs <- 100 * wobs / sum(wobs)
  wsh <- sapply(SCEN_ORDER, function(s) {
    f <- scen[scen$scenario == s & is.finite(scen$pm25_change_factor), c("objectid", "pm25_change_factor")]
    names(f)[1] <- "OBJECTID"
    dd <- merge(mrec, f, by = "OBJECTID", all.x = TRUE)
    dd <- dd[is.finite(dd$pm25_change_factor), ]
    x_sh <- dd$pm25_pop_weighted * dd$pm25_change_factor
    w <- tapply(dd$POP_EST, cut(pmin(x_sh, max(brk) - 1e-9), brk), sum)
    w[is.na(w)] <- 0
    100 * w / sum(w)
  })
  xmax <- 80
  style_panel(c(0, xmax), c(0, max(wobs, wsh) * 1.07), xlab = xlab_pm,
              ylab = "Share of population-months (%)")
  ii <- which(brk[-length(brk)] < xmax)
  for (i in ii) rect(brk[i], 0, brk[i + 1], wobs[i], col = "#D9D9D9", border = "white", lwd = 0.4)
  for (s in rev(SCEN_ORDER)) {
    lines(rep(brk[c(ii, max(ii) + 1)], each = 2)[-1][1:(2 * length(ii))],
          rep(wsh[ii, s], each = 2), col = FAN_STROKE[[s]], lwd = 1.6,
          lty = if (s == "RCP26") "solid" else "62")
  }
  abline(v = c(5, 35), col = MUTED, lwd = 0.9, lty = "13")
  text(5, par("usr")[4] * 0.985, "WHO guideline", cex = 0.6, col = MUTED, adj = c(-0.05, 1))
  text(35, par("usr")[4] * 0.985, "WHO interim target 1", cex = 0.6, col = MUTED, adj = c(-0.05, 1))
  legend("topright", inset = c(0.01, 0.10),
         legend = c("Observed 2016-2020", paste("Shifted,", SCEN_LABS)),
         col = c("#D9D9D9", FAN_STROKE), lwd = c(7, 1.6, 1.6, 1.6),
         lty = c("solid", "solid", "62", "62"), bty = "n", cex = 0.7, text.col = INK, seg.len = 1.6)
  panel_letter("d", adj_x = -0.13)

  # --- e: monthly exposure climatology
  cl_obs <- sapply(1:12, function(m) {
    dd <- mrec[mrec$month == m, ]
    c(wmean(dd$pm25_pop_weighted, dd$POP_EST), wq(dd$pm25_pop_weighted, dd$POP_EST, c(0.1, 0.9)))
  })
  cl_sh <- sapply(SCEN_ORDER, function(s) {
    f <- scen[scen$scenario == s & is.finite(scen$pm25_change_factor), c("objectid", "pm25_change_factor")]
    names(f)[1] <- "OBJECTID"
    dd <- merge(mrec, f, by = "OBJECTID", all.x = TRUE)
    dd <- dd[is.finite(dd$pm25_change_factor), ]
    sapply(1:12, function(m) {
      d2 <- dd[dd$month == m, ]
      wmean(d2$pm25_pop_weighted * d2$pm25_change_factor, d2$POP_EST)
    })
  })
  ylb <- range(cl_obs, cl_sh, 0, 45)
  style_panel(c(1, 12), ylb, xat = 1:12, xlab = "", ylab = xlab_pm, xlabels = FALSE)
  text(1:12, par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
       c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
       cex = 0.76, col = INK2, xpd = NA)
  mtext("Calendar month", side = 1, line = 1.9, cex = 0.78, col = INK2)
  polygon(c(1:12, 12:1), c(cl_obs[2, ], rev(cl_obs[3, ])), col = alpha_col("#9a9a9a", 0.18), border = NA)
  abline(h = c(5, 35), col = MUTED, lwd = 0.9, lty = "13")
  lines(1:12, cl_obs[1, ], col = INK, lwd = 2.2)
  for (s in SCEN_ORDER) lines(1:12, cl_sh[, s], col = FAN_STROKE[[s]], lwd = 1.9)
  legend("topright", inset = c(0.01, 0.01),
         legend = c("Observed (10th-90th pctile band)", paste("Shifted,", SCEN_LABS)),
         col = c(INK, FAN_STROKE), lwd = 2, bty = "n", cex = 0.7, text.col = INK, seg.len = 1.3)
  panel_letter("e", adj_x = -0.13)

  # --- f: global mean prevalence trajectory with scenario divergence
  par(mar = c(2.9, 3.9, 1.8, 4.6))
  glo <- lapply(SCEN_ORDER, function(s) {
    d <- d_all[d_all$scenario == s & is.finite(d_all$PI2050_pct_ci_low) &
                 is.finite(d_all$PI2050_pct_ci_high), ]
    c(m = wmean(d$PI2050_pct, d$pop_est),
      lo = wmean(pmin(d$PI2050_pct_ci_low, d$PI2050_pct_ci_high), d$pop_est),
      hi = wmean(pmax(d$PI2050_pct_ci_low, d$PI2050_pct_ci_high), d$pop_est))
  })
  names(glo) <- SCEN_ORDER
  m2020 <- mean_pi[yrs == 2020]
  yli <- range(mean_pi, unlist(glo))
  yli <- yli + c(-0.06, 0.06) * diff(yli)
  style_panel(c(2000, 2052.5), yli, xat = c(2000, 2010, 2020, 2030, 2040, 2050),
              xlab = "Year", ylab = "Prevalence of physical inactivity (%)")
  abline(v = 2020, col = "#bfbfbf", lwd = 0.9, lty = "13")
  for (s in SCEN_ORDER) {
    g <- glo[[s]]
    polygon(c(2020, 2050, 2050), c(m2020, g["lo"], g["hi"]),
            col = alpha_col(FAN_STROKE[[s]], 0.13), border = NA)
  }
  lines(yrs, mean_pi, col = INK, lwd = 2.2)
  # stagger endpoint labels that would collide
  ords <- order(sapply(glo, `[[`, "m"))
  lab_y <- sapply(glo, `[[`, "m")
  min_gap <- 0.055 * diff(yli)
  for (k in seq(2, length(ords))) {
    if (lab_y[ords[k]] - lab_y[ords[k - 1]] < min_gap) {
      lab_y[ords[k]] <- lab_y[ords[k - 1]] + min_gap
    }
  }
  for (s in SCEN_ORDER) {
    g <- glo[[s]]
    lines(c(2020, 2050), c(m2020, g["m"]), col = FAN_STROKE[[s]], lwd = 2.1, lty = "42")
    points(2050, g["m"], pch = 21, bg = "white", col = FAN_STROKE[[s]], lwd = 1.5, cex = 0.85)
    text(2051.2, lab_y[[s]], sprintf("%s  %.0f", SCEN_LABS[[s]], g["m"]),
         cex = 0.64, col = FAN_STROKE[[s]], font = 2, adj = c(0, 0.5), xpd = NA)
  }
  text(2001, par("usr")[4], "Shading: 95% interval from IV-coefficient uncertainty",
       cex = 0.58, col = MUTED, adj = c(0, 1), xpd = FALSE)
  panel_letter("f", adj_x = -0.13)

  # --- g-i: PI prevalence fans, observed history + projected 2050 levels
  par(mar = c(2.9, 3.9, 1.8, 1.3))
  letters3_ <- c(RCP26 = "g", RCP45 = "h", RCP85 = "i")
  for (s in SCEN_ORDER) {
    d <- d_all[d_all$scenario == s, ]
    style_panel(c(2000, 2052.5), ylp, xat = c(2000, 2010, 2020, 2030, 2040, 2050),
                xlab = "Year", ylab = if (s == "RCP26") "Prevalence of physical inactivity (%)" else "")
    abline(v = 2020, col = "#bfbfbf", lwd = 0.9, lty = "13")
    for (dd in split(obs_pi, obs_pi$objectid)) {
      dd <- dd[order(dd$año), ]
      lines(dd$año, dd$PI0, col = alpha_col(OBS_LINE, 0.45), lwd = 0.55)
    }
    up <- d$PI2050_pct > d$PI2020_pct
    segments(rep(2020, nrow(d)), d$PI2020_pct, rep(2050, nrow(d)), d$PI2050_pct,
             col = ifelse(up, alpha_col(col_up, 0.45), alpha_col(col_dn, 0.30)), lwd = 0.55)
    lines(yrs, mean_pi, col = INK, lwd = 1.9)
    m1 <- wmean(d$PI2050_pct, d$pop_est)
    lines(c(2020, 2050), c(mean_pi[yrs == 2020], m1), col = INK, lwd = 1.9, lty = "42")
    points(2050, m1, pch = 16, col = INK, cex = 0.8)
    text(2050.9, m1, sprintf("%.0f", m1), cex = 0.72, col = INK, adj = c(0, 0.5), xpd = NA)
    mtext(SCEN_LABS[[s]], side = 3, line = 0.55, cex = 0.78, col = INK, font = 2)
    panel_letter(letters3_[[s]], adj_x = -0.13)
  }
})


# =============================================================================
# figM3 - projected reductions and burden by country: PI reduction, avoided
# deaths, and cost savings (3 map rows x 3 scenarios, merged manuscript figure)
# =============================================================================
save_figure("figM3_projection_burden_maps", 11.6, 7.5, function() {
  layout(rbind(c(1, 2, 3), c(4, 4, 4), c(5, 6, 7), c(8, 8, 8), c(9, 10, 11), c(12, 12, 12)),
         heights = c(1, 0.19, 1, 0.19, 1, 0.19))
  par(oma = c(0.2, 2.6, 1.8, 0.4), mar = c(0.1, 0.25, 0.6, 0.25))
  # NOTE: costs_share_gdp is already expressed in per cent of GDP (e.g. 0.013
  # means 0.013% of GDP) - do not rescale it.
  rows <- list(
    list(col = "pi_reduction_pp_PI0", mult = 1, lab = "Physical inactivity",
         cb = "Change in physical inactivity at mid-century (pp)", fmt = "%.0f",
         dirs = c("Increase", "Reduction")),
    list(col = "deaths_per_100k", mult = 1, lab = "Avoided deaths",
         cb = "Annual deaths per 100,000 population at mid-century", fmt = "%.0f",
         dirs = c("Added deaths", "Avoided deaths")),
    list(col = "costs_share_gdp", mult = 1, lab = "Productivity losses",
         cb = "Productivity losses at mid-century (% of GDP)", fmt = "%.3f",
         dirs = c("Added losses", "Avoided losses"))
  )
  row_letters <- list(c(RCP26 = "a", RCP45 = "b", RCP85 = "c"),
                      c(RCP26 = "d", RCP45 = "e", RCP85 = "f"),
                      c(RCP26 = "g", RCP45 = "h", RCP85 = "i"))
  first <- TRUE
  round_up2 <- function(z) {  # round up to two significant digits
    mag <- 10^floor(log10(z))
    ceiling(z / mag * 10) / 10 * mag
  }
  for (ri in seq_along(rows)) {
    r <- rows[[ri]]
    vals_all <- scen[[r$col]][is.finite(scen[[r$col]])] * r$mult
    z <- round_up2(as.numeric(stats::quantile(abs(vals_all), 0.98)))
    for (s in SCEN_ORDER) {
      d <- scen[scen$scenario == s & is.finite(scen[[r$col]]) & is.finite(scen$objectid), ]
      draw_world(map_fill(d$objectid, d[[r$col]] * r$mult, c(-z, z), pal_div_benefit),
                 border = "#C8CDD3", lwd = 0.16)
      if (first) mtext(SCEN_LABS[[s]], side = 3, line = 0.15, cex = 0.8,
                       col = FAN_STROKE[[s]], font = 2)
      if (s == "RCP26") mtext(r$lab, side = 2, line = 0.4, cex = 0.74, col = INK, font = 2)
      panel_letter(row_letters[[ri]][[s]], adj_x = 0.0, line = if (first) 0.15 else 0.05)
    }
    first <- FALSE
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
    colorbar_h(0.38, 0.62, 0.5, 0.86, c(-z, z), pal_div_benefit, diverging = TRUE,
               title = r$cb, cex = 0.68, fmt = r$fmt, truncated = TRUE,
               dir_labels = r$dirs, nodata = TRUE,
               title_offset = 1.85)
  }
})

# figT1_burden_by_region - graphical replacement for manuscript Table 1: three
# shaded heatmap panels (avoided deaths, avoided productivity losses, and % of
# GDP), each a
# region x scenario grid with cells shaded by magnitude (blue = larger avoided
# burden, red = added burden) and the value printed in every cell; the Global
# total is a bold, separated row at the top of each panel
# =============================================================================
save_figure("figT1_burden_by_region", 12.0, 6.0, function() {
  layout(matrix(1:3, 1, 3), widths = c(1.62, 1, 1))
  par(oma = c(0.9, 0.4, 0.4, 0.4))
  d <- scen[is.finite(scen$avoidable_deaths_2050) & is.finite(scen$saved_costs_2050) &
              is.finite(scen$gdp_total) & !is.na(scen$region_wb) & nzchar(scen$region_wb), ]
  agg <- aggregate(list(deaths = d$avoidable_deaths_2050 / 1e6,
                        costs = d$saved_costs_2050 / 1e9,
                        sc = d$saved_costs_2050, gdp = d$gdp_total),
                   by = list(region = d$region_wb, scenario = d$scenario), FUN = sum)
  agg$gdp_pct <- agg$sc / agg$gdp * 100
  glob <- aggregate(list(deaths = d$avoidable_deaths_2050 / 1e6,
                         costs = d$saved_costs_2050 / 1e9,
                         sc = d$saved_costs_2050, gdp = d$gdp_total),
                    by = list(scenario = d$scenario), FUN = sum)
  glob$gdp_pct <- glob$sc / glob$gdp * 100
  glob$region <- "Global"
  full <- rbind(agg[, c("region", "scenario", "deaths", "costs", "gdp_pct")],
                glob[, c("region", "scenario", "deaths", "costs", "gdp_pct")])

  ord <- agg[agg$scenario == "RCP26", c("region", "deaths")]
  regions <- ord$region[order(-ord$deaths)]        # descending -> largest on top
  reg_full <- function(x) gsub(" \\(.*\\)$", "", x) # keep full World Bank names
  R <- length(regions)
  yreg <- setNames(seq(R, 1), regions)             # first (largest) at top
  y_glob <- R + 1.4                                # sample total sits above, with a gap

  # Diverging palette (ColorBrewer RdBu, reversed): deep red = added burden,
  # white = none, deep blue = avoided burden. A two-slope mapping keeps white
  # fixed at zero, so the small added-burden cells read as red and the negative
  # range is shown in the colour key.
  pal_div <- grDevices::colorRampPalette(c(
    "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
    "#F7F7F7",
    "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#08519C"))(255)
  clampi <- function(i) pmax(1L, pmin(255L, as.integer(i)))

  heatpanel <- function(vcol, title, unit, letter, fmt, dig, left_labels) {
    rv_all <- round(full[[vcol]], dig)
    vmax <- max(rv_all[rv_all > 0], na.rm = TRUE)
    vmin <- if (any(rv_all < 0, na.rm = TRUE)) min(rv_all[rv_all < 0], na.rm = TRUE) else 0
    has_neg <- vmin < 0
    idx_of <- function(v) {                          # two-slope: white (128) at 0
      rv <- round(v, dig)
      if (rv == 0) 128L
      else if (rv > 0) clampi(128 + round(min(1, rv / vmax) * 127))
      else clampi(128 - round(min(1, rv / vmin) * 127))
    }
    cell_fill <- function(v) pal_div[idx_of(v)]
    is_dark   <- function(v) abs(idx_of(v) - 128L) > 95L
    fmt_lab   <- function(v) {                        # avoid a spurious "-0.00"
      lab <- sprintf(fmt, v)
      if (grepl("^-0[.]?0*$", lab)) lab <- sub("-", "", lab)
      lab
    }
    par(mar = c(3.4, if (left_labels) 13.0 else 0.6, 3.1, 0.6))
    plot.new(); plot.window(xlim = c(0, 3), ylim = c(0.3, y_glob + 0.55), yaxs = "i")

    for (si in seq_along(SCEN_ORDER)) {
      s <- SCEN_ORDER[si]
      text(si - 0.5, y_glob + 0.62, SCEN_LABS[[s]], cex = 0.74, col = FAN_STROKE[[s]],
           font = 2, xpd = NA)
    }
    g <- 0.045                                       # thin white gap between tiles
    draw_cells <- function(rg, y, h) {
      for (si in seq_along(SCEN_ORDER)) {
        s <- SCEN_ORDER[si]
        v <- full[[vcol]][full$region == rg & full$scenario == s]
        if (length(v) == 0 || !is.finite(v)) next
        rect(si - 1 + g, y - h, si - g, y + h, col = cell_fill(v), border = "white", lwd = 0.6)
        text(si - 0.5, y, fmt_lab(v), cex = 0.76, font = if (rg == "Global") 2 else 1,
             col = if (is_dark(v)) "white" else INK)
      }
    }
    for (rg in regions) draw_cells(rg, yreg[[rg]], 0.42)
    draw_cells("Global", y_glob, 0.46)
    mtext(sprintf("%s (%s)", title, unit), side = 3, line = 1.55, at = 1.5,
          cex = 0.74, col = INK, font = 2)
    if (left_labels) {
      xl <- par("usr")[1] - 0.04 * diff(par("usr")[1:2])
      text(xl, yreg, reg_full(regions), adj = c(1, 0.5), cex = 0.84, col = INK2, xpd = NA)
      text(xl, y_glob, "Global", adj = c(1, 0.5), cex = 0.84, col = INK, font = 2, xpd = NA)
    }
    # diverging colour key: red (added) -- white (0) -- blue (avoided)
    nb <- 128; cbx0 <- 0.35; cbx1 <- 2.65; cby <- -0.06; cbh <- 0.17
    xmid <- (cbx0 + cbx1) / 2
    xs <- seq(cbx0, cbx1, length.out = nb + 1)
    for (i in seq_len(nb)) {
      t <- (i - 0.5) / nb
      pidx <- if (has_neg) {
        if (t < 0.5) clampi(1 + round((t / 0.5) * 127)) else clampi(128 + round(((t - 0.5) / 0.5) * 127))
      } else clampi(128 + round(t * 127))
      rect(xs[i], cby - cbh, xs[i + 1], cby, col = pal_div[pidx], border = NA, xpd = NA)
    }
    rect(cbx0, cby - cbh, cbx1, cby, col = NA, border = SPINE, lwd = 0.4, xpd = NA)
    text(cbx0 - 0.05, cby - cbh / 2, if (has_neg) fmt_lab(vmin) else "0",
         cex = 0.6, col = INK2, adj = c(1, 0.5), xpd = NA)
    text(cbx1 + 0.05, cby - cbh / 2, fmt_lab(vmax), cex = 0.6, col = INK2, adj = c(0, 0.5), xpd = NA)
    if (has_neg) {
      text(xmid, cby - cbh - 0.12, "0", cex = 0.56, col = INK2, adj = c(0.5, 1), xpd = NA)
      text(cbx0, cby + 0.05, "added", cex = 0.52, col = "#B2182B", adj = c(0, 0), xpd = NA)
      text(cbx1, cby + 0.05, "avoided", cex = 0.52, col = "#2166AC", adj = c(1, 0), xpd = NA)
    }
    panel_letter(letter, adj_x = if (left_labels) -0.46 else -0.07, line = 1.7)
  }
  heatpanel("deaths",  "Avoided deaths",              "millions",    "a", "%.2f", 2, TRUE)
  heatpanel("costs",   "Avoided productivity losses", "Int$ billion", "b", "%.1f", 1, FALSE)
  heatpanel("gdp_pct", "Avoided losses",              "% of GDP",    "c", "%.3f", 3, FALSE)
})


write.csv(manifest, file.path(fig_dir, "manifest.csv"), row.names = FALSE)
cat("done:", nrow(manifest), "figures ->", fig_dir, "\n")
