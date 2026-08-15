packages <- c(
  "dplyr",
  "fixest",
  "ggalluvial",
  "ggplot2",
  "ggrepel",
  "haven",
  "patchwork",
  "readr",
  "rlang",
  "scales",
  "stringi",
  "tibble",
  "tidyr"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

