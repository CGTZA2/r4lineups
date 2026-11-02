# Regenerate the stored vignette results
# ------------------------------------------------------------------
# CRAN asks that vignettes rebuild quickly. The slowest chunks (power
# simulation, bootstraps, the two-condition MAX fit) therefore show their code
# with `eval = precompute` and load their results from vignettes/precomputed/,
# together with the random-number state at that point, so every later chunk
# gives the same output as a full computation.
#
# Run this from the package root, with the current package installed, after
# any change that could alter those results:
#
#   Rscript data-raw/vignette_precompute.R
#
# It renders each affected vignette with R4LINEUPS_PRECOMPUTE=true, which runs
# the slow chunks and rewrites the .rds files. The HTML goes to a temporary
# directory.

vignettes <- c(
  "simulation_power_analysis.Rmd",
  "sdt_summary_comparisons.Rmd",
  "calibration_decision_analysis.Rmd",
  "pauc_statistical_comparison.Rmd"
)

Sys.setenv(R4LINEUPS_PRECOMPUTE = "true")
dir.create(file.path("vignettes", "precomputed"), showWarnings = FALSE)
out_dir <- tempfile("vignette_precompute_")

for (v in vignettes) {
  message("Rendering ", v)
  rmarkdown::render(file.path("vignettes", v), output_dir = out_dir,
                    intermediates_dir = out_dir,
                    knit_root_dir = normalizePath("vignettes"),
                    envir = new.env(), quiet = TRUE)
}

print(file.info(list.files(file.path("vignettes", "precomputed"),
                           full.names = TRUE))["size"])
