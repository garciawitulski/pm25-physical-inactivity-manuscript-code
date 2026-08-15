# Figure reproduction data

This directory contains the minimum processed analytical and derived inputs consumed by the scripts that generate Figures 1–5 of the manuscript.

## Inventory

| File | Used for |
|---|---|
| `Data_base.csv` | Figures 1–4 |
| `coef_allmodels_pm25.csv` | Figure 1 |
| `final_country_anio_panel.dta` | Figure 1 |
| `PM25_mensual_pop_long.csv` | Figures 2–4 |
| `outputs/derived/pm25_country_scenario_figure_data.csv` | Figures 2–5 |
| `outputs/derived/msa_temperature_style_previews/world_coords.csv` | Figure 3 |
| `outputs/derived/msa_temperature_style_previews/world_attributes.csv` | Figure 3 |
| `outputs/alternative_burden_ccpi_style_histPM_PI2020/final/global_summary_histPM_PI2020.csv` | Figure 2 |

These are reproduction inputs rather than the unprocessed upstream source datasets. Run `Rscript run_all.R` from the repository root to generate the manuscript figures.
