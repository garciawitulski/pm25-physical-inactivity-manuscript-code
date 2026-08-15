# PM2.5 exposure and physical inactivity: manuscript code

This repository contains the code and processed data accompanying the manuscript, *Impacts of long-term PM2.5 exposure on physical inactivity: quasi-experimental evidence and projected cobenefits of cleaner air pathways*.

The minimum analytical and derived inputs needed to reproduce the five main-manuscript figures are included under `data/`. Generated artifacts are not tracked.

## Figure map

| Manuscript figure | Script | Output stem |
|---|---|---|
| Figure 1 | `R/figure_01.R` | `figure_1_regression_models` |
| Figure 2 | `R/figures_02_04.R` | `figM2_exposure_pi_projection` |
| Figure 3 | `R/figures_02_04.R` | `figM3_projection_burden_maps` |
| Figure 4 | `R/figures_02_04.R` | `figT1_burden_by_region` |
| Figure 5 | `R/figure_05.R` | `figure_5_cost_distribution_alluvial` |

All files are written to `outputs/` in PDF and PNG format. Figures 1 and 5 are also written as TIFF files.

## Software

The code was validated with R 4.6.1. Install the required CRAN packages with:

```r
Rscript requirements.R
```

## Included inputs

The repository includes the following processed inputs:

```text
Data_base.csv
coef_allmodels_pm25.csv
final_country_anio_panel.dta
PM25_mensual_pop_long.csv
outputs/derived/pm25_country_scenario_figure_data.csv
outputs/derived/msa_temperature_style_previews/world_coords.csv
outputs/derived/msa_temperature_style_previews/world_attributes.csv
outputs/alternative_burden_ccpi_style_histPM_PI2020/final/global_summary_histPM_PI2020.csv
```

See `data/README.md` for an inventory. The files retain the directory structure expected by the figure scripts.

## Run

PowerShell:

```powershell
Rscript requirements.R
Rscript run_all.R
```

Bash:

```bash
Rscript requirements.R
Rscript run_all.R
```

The runner uses `data/` by default and validates all required inputs before generating any figure. To use an alternative copy of the inputs, set the optional `PM25_DATA_ROOT` environment variable to its root directory.
