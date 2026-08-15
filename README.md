# PM2.5 exposure and physical inactivity: manuscript figure code

This code-only repository reproduces the five figures in the main manuscript, *Impacts of long-term PM2.5 exposure on physical inactivity: quasi-experimental evidence and projected cobenefits of cleaner air pathways*.

It intentionally excludes datasets, estimation files, the manuscript, supplementary material, and generated figures.

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

## Required inputs

Set `PM25_DATA_ROOT` to the root directory containing the analytical and derived inputs listed below. These files are not tracked by this repository.

```text
Data_base.csv
coef_allmodels_pm25.csv
final_country_a*_panel.dta
PM25_mensual_pop_long.csv
outputs/derived/pm25_country_scenario_figure_data.csv
outputs/derived/msa_temperature_style_previews/world_coords.csv
outputs/derived/msa_temperature_style_previews/world_attributes.csv
outputs/alternative_burden_ccpi_style_histPM_PI2020/final/global_summary_histPM_PI2020.csv
```

If more than one `final_country_a*_panel.dta` file is present, Figure 1 uses the most recently modified file, matching the manuscript build.

## Run

PowerShell:

```powershell
$env:PM25_DATA_ROOT = "C:\path\to\the\paper\data-root"
Rscript run_all.R
```

Bash:

```bash
export PM25_DATA_ROOT="/path/to/the/paper/data-root"
Rscript run_all.R
```

The runner validates all required inputs before generating any figure.

