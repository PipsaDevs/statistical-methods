# ==============================================================================
# Script: make_pendrive.R
# Purpose: Automatically builds all exam deliverables (.zip, .tar.gz, docs, fallback)
#          and organizes them into a single 'pendrive_ready' folder.
# ==============================================================================

cat("\n========================================================\n")
cat(" Starting Automated USB Pendrive Build for statmethods\n")
cat("========================================================\n\n")

# --- Step 1: Ensure DESCRIPTION has a valid email (Required by R CMD build) ---
cat("[1/7] Checking DESCRIPTION file...\n")
desc_file <- "DESCRIPTION"
if (file.exists(desc_file)) {
  desc_lines <- readLines(desc_file, warn = FALSE)
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
cat("[2/7] Updating documentation and NAMESPACE with roxygen2...\n")
roxygen2::roxygenise(".")

# --- Step 3: Create clean output directory ---
cat("[3/7] Setting up 'pendrive_ready' output folder...\n")
out_dir <- "pendrive_ready"
if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, showWarnings = FALSE)

# --- Step 4: Build universal source package (.tar.gz) using base R ---
cat("[4/7] Building source package (.tar.gz)...\n")
# Clear any existing tarballs in the working directory first
old_tars <- list.files(".", pattern = "^statmethods_.*\\.tar\\.gz$", full.names = TRUE)
if (length(old_tars) > 0) unlink(old_tars)

r_bin <- file.path(R.home("bin"), "R")
build_out <- system2(
  r_bin,
  c("CMD", "build", "--no-build-vignettes", "--no-manual", "."),
  stdout = TRUE,
  stderr = TRUE
)
tar_built <- list.files(".", pattern = "^statmethods_.*\\.tar\\.gz$", full.names = FALSE)
if (length(tar_built) == 0) {
  stop("Failed to build package tarball. R CMD build output:\n", paste(build_out, collapse = "\n"))
}
tar_pkg <- file.path(out_dir, tar_built[1])
file.copy(tar_built[1], tar_pkg, overwrite = TRUE)
unlink(tar_built[1])
cat(paste0("      Created: ", basename(tar_pkg), "\n"))

# --- Step 5: Build Windows binary package (.zip) ---
cat("[5/7] Building Windows binary package (.zip)...\n")
temp_lib <- file.path(tempdir(), "pkg_build_temp")
if (dir.exists(temp_lib)) unlink(temp_lib, recursive = TRUE)
dir.create(temp_lib, showWarnings = FALSE)

utils::install.packages(tar_pkg, lib = temp_lib, repos = NULL, type = "source", quiet = TRUE)
zip_name <- sub("\\.tar\\.gz$", ".zip", basename(tar_pkg))
zip_path <- file.path(normalizePath(out_dir), zip_name)

zip::zip(zipfile = zip_path, files = "statmethods", root = temp_lib)
unlink(temp_lib, recursive = TRUE)
cat(paste0("      Created: ", zip_name, "\n"))

# --- Step 6: Generate standalone offline HTML documentation ---
cat("[6/7] Generating offline HTML documentation & cheat sheet...\n")
rd_files <- sort(list.files("man", full.names = TRUE, pattern = "\\.Rd$"))
temp_html_dir <- file.path(tempdir(), "rd_html_temp")
if (dir.exists(temp_html_dir)) unlink(temp_html_dir, recursive = TRUE)
dir.create(temp_html_dir, showWarnings = FALSE)

fn_data <- list()
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

for (f in rd_files) {
  fn_name <- sub("\\.Rd$", "", basename(f))

  # Determine source file from roxygen comment or parse fallback
  rd_lines <- readLines(f, warn = FALSE)
  src_file <- NA
  m <- grep("^%\\s*Please edit documentation in R/", rd_lines, value = TRUE)
  if (length(m) > 0) {
    raw_src <- sub("^%\\s*Please edit documentation in ", "", m[1])
    first_src <- trimws(strsplit(raw_src, ",")[[1]][1])
    src_file <- basename(first_src)
  }
  if (is.na(src_file) || src_file == "") {
    for (rf in r_files) {
      rlines <- readLines(rf, warn = FALSE)
      pattern <- paste0("^\\s*(`?", fn_name, "`?)\\s*(<-|=)\\s*function")
      if (any(grepl(pattern, rlines))) {
        src_file <- basename(rf)
        break
      }
    }
  }
  if (is.na(src_file) || src_file == "") {
    src_file <- "other.R"
  }

  out_f <- file.path(temp_html_dir, paste0(fn_name, ".html"))
  tools::Rd2HTML(f, out = out_f, package = "statmethods", stylesheet = "")

  full_html <- paste(readLines(out_f, warn = FALSE), collapse = "\n")

  # Extract body content cleanly from Rd2HTML output
  body_content <- sub("(?s)^.*<body[^>]*>(.*?)</body>.*$", "\\1", full_html, perl = TRUE)

  # Remove Rd2HTML header table, HR lines, and footer navigation
  clean_content <- gsub("(?s)<table style=\"width: 100%;\">.*?</table>", "", body_content, perl = TRUE)
  clean_content <- gsub("(?s)<hr\\s*/?>", "", clean_content, perl = TRUE)
  clean_content <- gsub("(?s)<div style=\"text-align: center;\">.*?</div>", "", clean_content, perl = TRUE)

  fn_data[[fn_name]] <- list(
    name = fn_name,
    file = src_file,
    content = clean_content
  )
}
unlink(temp_html_dir, recursive = TRUE)

# Sort files logically: chapter1, chapter2, chapter3, chapter4, then others
all_files <- unique(sapply(fn_data, function(x) x$file))
file_order_key <- function(files) {
  nums <- suppressWarnings(as.numeric(sub("^chapter([0-9]+).*", "\\1", files)))
  is_ch <- grepl("^chapter[0-9]+", files)
  rank_val <- ifelse(is_ch, nums, 10000)
  order(rank_val, files)
}
sorted_files <- all_files[file_order_key(all_files)]

nav_sections_html <- c()
main_sections_html <- c()
pills_html <- c()

for (sf in sorted_files) {
  file_slug <- gsub("[^a-zA-Z0-9_-]", "_", sf)
  sec_id <- paste0("file-", file_slug)

  fns_in_file <- fn_data[sapply(fn_data, function(x) x$file == sf)]
  fns_in_file <- fns_in_file[order(names(fns_in_file))]
  fn_count <- length(fns_in_file)

  # Quick jump pill
  pills_html <- c(pills_html, paste0(
    '        <a href="#', sec_id, '" class="file-pill">',
    '<span class="pill-icon">📁</span> ', sf, ' ',
    '<span class="pill-badge">', fn_count, '</span></a>'
  ))

  # Sidebar navigation list items
  sublist_items <- sapply(fns_in_file, function(item) {
    paste0('          <li data-fn="', item$name, '"><a href="#fn-', item$name, '" class="nav-item">', item$name, '</a></li>')
  })

  nav_sec <- paste0(
    '      <div class="nav-file-section" data-file="', sf, '">\n',
    '        <div class="nav-file-header">\n',
    '          <a href="#', sec_id, '" class="nav-file-title" title="Jump to ', sf, '">\n',
    '            <span class="file-icon">📁</span>\n',
    '            <span class="nav-file-name">', sf, '</span>\n',
    '          </a>\n',
    '          <span class="nav-count">', fn_count, '</span>\n',
    '        </div>\n',
    '        <ul class="nav-sublist">\n',
    paste(sublist_items, collapse = "\n"), '\n',
    '        </ul>\n',
    '      </div>'
  )
  nav_sections_html <- c(nav_sections_html, nav_sec)

  # Main content cards
  cards_in_sec <- sapply(fns_in_file, function(item) {
    paste0(
      '      <article class="fn-card" id="fn-', item$name, '" data-fn="', item$name, '" data-file="', sf, '">\n',
      '        <div class="fn-header">\n',
      '          <div class="fn-header-left">\n',
      '            <span class="badge badge-fn">Function</span>\n',
      '            <span class="badge badge-file" title="Defined in ', sf, '">📁 ', sf, '</span>\n',
      '            <h2 class="fn-name">', item$name, '</h2>\n',
      '          </div>\n',
      '          <div class="fn-header-right">\n',
      '            <span class="fn-source-tag">Source: <code>R/', sf, '</code></span>\n',
      '          </div>\n',
      '        </div>\n',
      '        <div class="fn-body">\n',
      item$content, '\n',
      '        </div>\n',
      '      </article>'
    )
  })

  main_sec <- paste0(
    '    <section class="file-section" id="', sec_id, '" data-file="', sf, '">\n',
    '      <div class="file-section-header">\n',
    '        <div class="file-section-info">\n',
    '          <span class="file-section-icon">📁</span>\n',
    '          <div>\n',
    '            <h2 class="file-section-title">', sf, '</h2>\n',
    '            <p class="file-section-desc">', fn_count, ' functions defined in <code>R/', sf, '</code></p>\n',
    '          </div>\n',
    '        </div>\n',
    '        <span class="file-section-count">', fn_count, ' functions</span>\n',
    '      </div>\n',
    '      <div class="file-cards-list">\n',
    paste(cards_in_sec, collapse = "\n"), '\n',
    '      </div>\n',
    '    </section>'
  )
  main_sections_html <- c(main_sections_html, main_sec)
}

# Assemble final HTML using template files or inline fallback
if (file.exists("templates/template.html") && file.exists("templates/styles.css") && file.exists("templates/app.js")) {
  tpl <- paste(readLines("templates/template.html", warn = FALSE), collapse = "\n")
  css <- paste(readLines("templates/styles.css", warn = FALSE), collapse = "\n")
  js  <- paste(readLines("templates/app.js", warn = FALSE), collapse = "\n")

  html_out <- tpl
  html_out <- sub("/* {{CSS}} */", css, html_out, fixed = TRUE)
  html_out <- sub("/* {{JS}} */", js, html_out, fixed = TRUE)
  html_out <- sub("<!-- {{PILLS}} -->", paste(pills_html, collapse = "\n"), html_out, fixed = TRUE)
  html_out <- sub("<!-- {{NAV}} -->", paste(nav_sections_html, collapse = "\n"), html_out, fixed = TRUE)
  html_out <- sub("<!-- {{CONTENT}} -->", paste(main_sections_html, collapse = "\n"), html_out, fixed = TRUE)
} else {
  # Inline string fallback if templates folder is not created yet
  html_out <- paste0(
    '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><title>statmethods Manual</title></head><body>',
    '<h1>statmethods Documentation</h1>',
    paste(pills_html, collapse = "\n"),
    paste(main_sections_html, collapse = "\n"),
    '</body></html>'
  )
}

writeLines(html_out, file.path(out_dir, "documentation.html"))
cat("      Created: documentation.html (Self-contained offline manual)\n")

# --- Step 7: Copy raw source folder & create enhanced emergency fallback ---
cat("[7/7] Copying raw scripts and creating emergency fallback...\n")
source_copy <- file.path(out_dir, "statmethods_source")
dir.create(source_copy, showWarnings = FALSE)

file.copy("DESCRIPTION", source_copy)
file.copy("NAMESPACE", source_copy)
if (file.exists("README.md")) file.copy("README.md", source_copy)

dir.create(file.path(source_copy, "R"), showWarnings = FALSE)
file.copy(list.files("R", full.names = TRUE), file.path(source_copy, "R"))

dir.create(file.path(source_copy, "man"), showWarnings = FALSE)
file.copy(list.files("man", full.names = TRUE), file.path(source_copy, "man"))

# Create fallback loader script with doc() support
fallback_code <- '# ==============================================================================
# EMERGENCY FALLBACK SCRIPT: load_all_fallback.R
# Run this script in RStudio if the exam PC restricts installing packages!
# ==============================================================================

# 1. Source all R functions into global memory
script_files <- list.files("statmethods_source/R", full.names = TRUE, pattern = "\\.R$")
for (f in script_files) {
  source(f)
}

# 2. Define offline documentation helper doc()
doc <- function(topic) {
  name <- tryCatch(as.character(substitute(topic)), error = function(e) as.character(topic))
  name <- gsub(\'^"|"$\', \'\', name)

  man_candidates <- c(
    file.path("statmethods_source", "man"),
    file.path(".", "statmethods_source", "man"),
    "man"
  )
  man_dir <- NULL
  for (d in man_candidates) {
    if (dir.exists(d)) { man_dir <- d; break }
  }

  if (is.null(man_dir)) {
    message("[!] Could not locate statmethods_source/man. Make sure working directory is pendrive_ready.")
    return(invisible(NULL))
  }

  rd_file <- file.path(man_dir, paste0(name, ".Rd"))
  if (file.exists(rd_file)) {
    tools::Rd2txt(rd_file)
  } else {
    message("[!] No documentation found for \x27", name, "\x27. Available functions:")
    avail <- sort(sub("\\.Rd$", "", list.files(man_dir, pattern = "\\.Rd$")))
    print(avail)
  }
  invisible(NULL)
}

# 3. Print friendly status banner
cat("\n========================================================================\n")
cat(" [OK] statmethods (Fallback Mode) loaded into memory!\n")
cat("------------------------------------------------------------------------\n")
cat(" -> All formulas and bundle() are ready to use directly.\n")
cat(" -> For help on any function, run: doc(function_name)\n")
cat("    Example: doc(modelFunc)\n")
cat(" -> Or double-click \x27documentation.html\x27 in this folder for the browser guide!\n")
cat("========================================================================\n\n")
'

writeLines(fallback_code, file.path(out_dir, "load_all_fallback.R"))

cat("\n========================================================\n")
cat(" BUILD COMPLETE! Your files are ready in 'pendrive_ready':\n")
cat("========================================================\n\n")

files <- list.files(out_dir, recursive = FALSE)
for (f in files) {
  cat(paste0("  -> ", f, "\n"))
}

cat("\nSimply copy everything inside 'pendrive_ready' directly to your USB stick!\n\n")
