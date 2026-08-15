args <- commandArgs(trailingOnly = FALSE)
script_arg <- sub("--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(script_arg) > 0) {
  dirname(normalizePath(script_arg[1], winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

Sys.setenv(PM25_CODE_ROOT = repo_root)
source(file.path(repo_root, "R", "common.R"))

data_root <- get_data_root()
validate_required_inputs(data_root)
dir.create(file.path(repo_root, "outputs"), recursive = TRUE, showWarnings = FALSE)

scripts <- c("figure_01.R", "figures_02_04.R", "figure_05.R")
for (script in scripts) {
  message("Running ", script)
  sys.source(
    file.path(repo_root, "R", script),
    envir = new.env(parent = globalenv())
  )
}

message("Done. Main-manuscript figures are in: ", file.path(repo_root, "outputs"))

