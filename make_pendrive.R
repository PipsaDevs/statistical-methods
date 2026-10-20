# ==============================================================================
# Script: make_pendrive.R
# Purpose: Automatically builds all exam deliverables (.zip, .tar.gz, docs, fallback)
#          and organizes them into a single 'pendrive_ready' folder.
# ==============================================================================

cat("\n========================================================\n")
cat(" Starting Automated USB Pendrive Build for statmethods\n")
cat("========================================================\n\n")

# --- Step 1: Ensure DESCRIPTION has a valid email (Required by R CMD build) ---
cat("[1/6] Checking DESCRIPTION file...\n")
desc_file <- "DESCRIPTION"
if (file.exists(desc_file)) {
  desc_lines <- readLines(desc_file)
  if (!any(grepl("email", desc_lines))) {
    cat("      Adding maintainer email to DESCRIPTION for R CMD build compatibility...\n")
    desc_lines <- gsub(
      'person\\("PipsaDevs", role = c\\("aut", "cre", "cph"\\)\\)',
      'person("PipsaDevs", email = "pipsadevs@uma.es", role = c("aut", "cre", "cph"))',
      desc_lines
    )
    writeLines(desc_lines, desc_file)
  }
}

# --- Step 2: Regenerate roxygen2 documentation & NAMESPACE ---
cat("[2/6] Updating documentation and NAMESPACE with roxygen2...\n")
roxygen2::roxygenise(".")

# --- Step 3: Create clean output directory ---
cat("[3/6] Setting up 'pendrive_ready' output folder...\n")
out_dir <- "pendrive_ready"
if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, showWarnings = FALSE)

# --- Step 4: Build universal source package (.tar.gz) ---
cat("[4/6] Building source package (.tar.gz)...\n")
tar_pkg <- devtools::build(path = out_dir, quiet = TRUE)
cat(paste0("      Created: ", basename(tar_pkg), "\n"))

# --- Step 5: Build Windows binary package (.zip) ---
cat("[5/6] Building Windows binary package (.zip)...\n")
# Because this package is 100% pure R code, a binary package is the 
# installed library structure zipped. We install into a temp folder and zip it.
temp_lib <- file.path(tempdir(), "pkg_build_temp")
if (dir.exists(temp_lib)) unlink(temp_lib, recursive = TRUE)
dir.create(temp_lib, showWarnings = FALSE)

utils::install.packages(tar_pkg, lib = temp_lib, repos = NULL, type = "source", quiet = TRUE)
zip_name <- sub("\\.tar\\.gz$", ".zip", basename(tar_pkg))
zip_path <- file.path(normalizePath(out_dir), zip_name)

zip::zip(zipfile = zip_path, files = "statmethods", root = temp_lib)
unlink(temp_lib, recursive = TRUE)
cat(paste0("      Created: ", zip_name, "\n"))

# --- Step 6: Copy raw source folder & emergency fallback script ---
cat("[6/6] Copying raw scripts and creating emergency fallback...\n")
source_copy <- file.path(out_dir, "statmethods_source")
dir.create(source_copy, showWarnings = FALSE)

file.copy("DESCRIPTION", source_copy)
file.copy("NAMESPACE", source_copy)
if (file.exists("README.md")) file.copy("README.md", source_copy)

dir.create(file.path(source_copy, "R"), showWarnings = FALSE)
file.copy(list.files("R", full.names = TRUE), file.path(source_copy, "R"))

dir.create(file.path(source_copy, "man"), showWarnings = FALSE)
file.copy(list.files("man", full.names = TRUE), file.path(source_copy, "man"))

# Create fallback loader script
cat(
'# ==============================================================================
# EMERGENCY FALLBACK SCRIPT: load_all_fallback.R
# Run this script in RStudio if the exam PC restricts installing packages!
# ==============================================================================

script_files <- list.files("statmethods_source/R", full.names = TRUE, pattern = "\\\\.R$")
for (f in script_files) {
  source(f)
}
message("[OK] All formulas and the bundle() tool have been loaded directly into memory!")
',
  file = file.path(out_dir, "load_all_fallback.R")
)

cat("\n========================================================\n")
cat(" BUILD COMPLETE! Your files are ready in 'pendrive_ready':\n")
cat("========================================================\n\n")

files <- list.files(out_dir, recursive = FALSE)
for (f in files) {
  cat(paste0("  -> ", f, "\n"))
}

cat("\nSimply copy everything inside 'pendrive_ready' directly to your USB stick!\n\n")
