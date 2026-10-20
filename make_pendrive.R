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
file.rename(tar_built[1], tar_pkg)
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
  lines <- readLines(out_f, warn = FALSE)
  start_idx <- grep("<main>", lines)
  end_idx <- grep("</main>", lines)
  if (length(start_idx) > 0 && length(end_idx) > 0) {
    content <- paste(lines[(start_idx + 1):(end_idx - 1)], collapse = "\n")
  } else {
    content <- paste(lines, collapse = "\n")
  }
  content <- sub("<table style=\"width: 100%;\">.*?</table>", "", content)
  content <- sub("<hr><div style=\"text-align: center;\">.*?</div>", "", content)

  fn_data[[fn_name]] <- list(
    name = fn_name,
    file = src_file,
    content = content
  )
}
unlink(temp_html_dir, recursive = TRUE)

# Sort files logically: chapter1, chapter2, chapter3, chapter4, then others (e.g. bundle.R)
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

  # Nav items in sidebar
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

  # Cards in main content
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

html_page <- paste0(
'<!DOCTYPE html>
<html lang="en" data-theme="light">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>statmethods — Offline Exam Documentation</title>
  <script>
    (function() {
      try {
        var saved = localStorage.getItem("statmethods-theme");
        var prefersDark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
        var theme = saved || (prefersDark ? "dark" : "light");
        document.documentElement.setAttribute("data-theme", theme);
      } catch (e) {}
    })();
  </script>
  <style>
    :root, html[data-theme="light"] {
      --bg: #f8fafc;
      --surface: #ffffff;
      --surface-hover: #f1f5f9;
      --surface-header: #0f172a;
      --primary: #2563eb;
      --primary-hover: #1d4ed8;
      --primary-light: #eff6ff;
      --text: #0f172a;
      --text-muted: #64748b;
      --border: #e2e8f0;
      --border-subtle: #f1f5f9;
      --code-bg: #f1f5f9;
      --code-border: #e2e8f0;
      --code-text: #0f172a;
      --badge-fn-bg: #dbeafe;
      --badge-fn-text: #1d4ed8;
      --badge-file-bg: #ede9fe;
      --badge-file-text: #6d28d9;
      --file-header-bg: #ffffff;
      --file-header-border: #e2e8f0;
      --nav-group-bg: #f8fafc;
      --nav-group-border: #e2e8f0;
      --nav-item-color: #334155;
      --nav-item-hover-bg: #eff6ff;
      --nav-item-hover-color: #2563eb;
      --card-shadow: 0 1px 3px rgba(0,0,0,0.06), 0 1px 2px rgba(0,0,0,0.04);
      --input-bg: #ffffff;
      --input-border: #cbd5e1;
      --sidebar-w: 300px;
    }

    html[data-theme="dark"] {
      --bg: #090d16;
      --surface: #111827;
      --surface-hover: #1f2937;
      --surface-header: #030712;
      --primary: #38bdf8;
      --primary-hover: #60a5fa;
      --primary-light: rgba(56, 189, 248, 0.12);
      --text: #f3f4f6;
      --text-muted: #9ca3af;
      --border: #1f2937;
      --border-subtle: #162032;
      --code-bg: #030712;
      --code-border: #1f2937;
      --code-text: #f3f4f6;
      --badge-fn-bg: #1e3a5f;
      --badge-fn-text: #93c5fd;
      --badge-file-bg: #2e1d54;
      --badge-file-text: #c4b5fd;
      --file-header-bg: #151d30;
      --file-header-border: #243048;
      --nav-group-bg: #111827;
      --nav-group-border: #1f2937;
      --nav-item-color: #9ca3af;
      --nav-item-hover-bg: #1e293b;
      --nav-item-hover-color: #38bdf8;
      --card-shadow: 0 2px 5px rgba(0,0,0,0.4);
      --input-bg: #1f2937;
      --input-border: #374151;
      --sidebar-w: 300px;
    }

    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      background: var(--bg);
      color: var(--text);
      line-height: 1.6;
      display: flex;
      height: 100vh;
      overflow: hidden;
      transition: background-color 0.2s ease, color 0.2s ease;
    }
    aside {
      width: var(--sidebar-w);
      background: var(--surface);
      border-right: 1px solid var(--border);
      display: flex;
      flex-direction: column;
      flex-shrink: 0;
      height: 100vh;
    }
    .brand {
      padding: 14px 18px;
      border-bottom: 1px solid var(--border);
      background: var(--surface-header);
      color: white;
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      flex-shrink: 0;
    }
    .brand-info h1 {
      font-size: 1.15rem;
      font-weight: 700;
      letter-spacing: -0.02em;
      color: #ffffff;
      line-height: 1.2;
    }
    .brand-info p {
      font-size: 0.76rem;
      color: #94a3b8;
      margin-top: 2px;
      white-space: nowrap;
    }
    .theme-toggle-btn {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      background: rgba(255, 255, 255, 0.12);
      border: 1px solid rgba(255, 255, 255, 0.2);
      color: #f1f5f9;
      padding: 5px 10px;
      border-radius: 9999px;
      font-size: 0.78rem;
      font-weight: 600;
      cursor: pointer;
      outline: none;
      transition: all 0.2s ease;
      flex-shrink: 0;
      user-select: none;
    }
    .theme-toggle-btn:hover {
      background: rgba(255, 255, 255, 0.22);
      border-color: rgba(255, 255, 255, 0.35);
    }
    .theme-toggle-btn:active {
      transform: scale(0.96);
    }
    .theme-icon {
      width: 14px;
      height: 14px;
      display: block;
    }
    html[data-theme="light"] .theme-icon.sun-icon { display: none; }
    html[data-theme="light"] .theme-icon.moon-icon { display: block; }
    html[data-theme="dark"] .theme-icon.sun-icon { display: block; }
    html[data-theme="dark"] .theme-icon.moon-icon { display: none; }

    .search-box {
      padding: 12px 14px;
      border-bottom: 1px solid var(--border);
      flex-shrink: 0;
      background: var(--surface);
    }
    .search-wrapper {
      position: relative;
      display: flex;
      align-items: center;
    }
    .search-box input {
      width: 100%;
      padding: 8px 30px 8px 12px;
      font-size: 0.88rem;
      border: 1px solid var(--input-border);
      border-radius: 6px;
      outline: none;
      background: var(--input-bg);
      color: var(--text);
      transition: border-color 0.15s, box-shadow 0.15s;
    }
    .search-box input:focus {
      border-color: var(--primary);
      box-shadow: 0 0 0 2px var(--primary-light);
    }
    .search-clear {
      position: absolute;
      right: 8px;
      background: none;
      border: none;
      color: var(--text-muted);
      cursor: pointer;
      font-size: 0.9rem;
      display: none;
      padding: 2px 4px;
    }
    .nav-list {
      overflow-y: auto;
      flex: 1;
      padding: 10px 10px 40px;
    }
    .nav-file-section {
      margin-bottom: 12px;
      border: 1px solid var(--border);
      border-radius: 6px;
      background: var(--surface);
      overflow: hidden;
    }
    .nav-file-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 8px 10px;
      background: var(--nav-group-bg);
      border-bottom: 1px solid var(--border);
    }
    .nav-file-title {
      display: flex;
      align-items: center;
      gap: 6px;
      color: var(--text);
      font-weight: 600;
      font-size: 0.82rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      text-decoration: none;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .nav-file-title:hover {
      color: var(--primary);
    }
    .nav-file-title .file-icon {
      font-size: 0.85rem;
    }
    .nav-count {
      font-size: 0.72rem;
      font-weight: 700;
      color: var(--text-muted);
      background: var(--surface);
      border: 1px solid var(--border);
      padding: 1px 6px;
      border-radius: 999px;
      flex-shrink: 0;
    }
    .nav-sublist {
      list-style: none;
      padding: 4px 0;
    }
    .nav-item {
      display: block;
      padding: 6px 10px 6px 20px;
      font-size: 0.82rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: var(--nav-item-color);
      text-decoration: none;
      border-left: 2px solid transparent;
      transition: all 0.15s;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .nav-item:hover {
      background: var(--nav-item-hover-bg);
      color: var(--nav-item-hover-color);
    }
    .nav-item.active {
      background: var(--nav-item-hover-bg);
      color: var(--primary);
      font-weight: 600;
      border-left-color: var(--primary);
    }
    main {
      flex: 1;
      overflow-y: auto;
      padding: 32px 48px 120px;
      scroll-behavior: smooth;
    }
    .header-banner {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 22px 26px;
      margin-bottom: 24px;
      box-shadow: var(--card-shadow);
    }
    .header-banner h2 {
      font-size: 1.45rem;
      margin-bottom: 8px;
      color: var(--text);
    }
    .header-banner p {
      color: var(--text-muted);
      font-size: 0.95rem;
      line-height: 1.5;
    }
    .file-pills-bar {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      gap: 8px;
      margin-bottom: 32px;
      padding: 12px 18px;
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      box-shadow: var(--card-shadow);
    }
    .pills-label {
      font-size: 0.8rem;
      font-weight: 700;
      color: var(--text-muted);
      text-transform: uppercase;
      letter-spacing: 0.05em;
      margin-right: 4px;
    }
    .file-pill {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      padding: 4px 10px;
      background: var(--file-header-bg);
      border: 1px solid var(--border);
      border-radius: 6px;
      color: var(--text);
      text-decoration: none;
      font-size: 0.82rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      font-weight: 500;
      transition: all 0.15s;
    }
    .file-pill:hover {
      border-color: var(--primary);
      color: var(--primary);
      background: var(--primary-light);
    }
    .pill-badge {
      font-size: 0.7rem;
      background: var(--surface);
      border: 1px solid var(--border);
      color: var(--text-muted);
      padding: 1px 5px;
      border-radius: 999px;
      font-weight: 700;
    }
    .file-section {
      margin-bottom: 48px;
      scroll-margin-top: 20px;
    }
    .file-section-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      background: var(--file-header-bg);
      border: 1px solid var(--file-header-border);
      border-left: 5px solid var(--primary);
      border-radius: 8px;
      padding: 16px 20px;
      margin-bottom: 20px;
      box-shadow: var(--card-shadow);
    }
    .file-section-info {
      display: flex;
      align-items: center;
      gap: 12px;
    }
    .file-section-icon {
      font-size: 1.5rem;
    }
    .file-section-title {
      font-size: 1.25rem;
      font-weight: 700;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: var(--text);
    }
    .file-section-desc {
      font-size: 0.85rem;
      color: var(--text-muted);
    }
    .file-section-count {
      font-size: 0.8rem;
      font-weight: 600;
      padding: 4px 12px;
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 12px;
      color: var(--text-muted);
    }
    .fn-card {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 24px;
      margin-bottom: 24px;
      box-shadow: var(--card-shadow);
      scroll-margin-top: 20px;
      transition: border-color 0.2s, box-shadow 0.2s;
    }
    .fn-card:target {
      border-color: var(--primary);
      box-shadow: 0 0 0 3px var(--primary-light);
    }
    .fn-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-wrap: wrap;
      gap: 10px;
      border-bottom: 1px solid var(--border);
      padding-bottom: 12px;
      margin-bottom: 16px;
    }
    .fn-header-left {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      gap: 10px;
    }
    .badge {
      font-size: 0.74rem;
      font-weight: 700;
      padding: 3px 8px;
      border-radius: 4px;
      text-transform: uppercase;
      letter-spacing: 0.04em;
    }
    .badge-fn {
      background: var(--badge-fn-bg);
      color: var(--badge-fn-text);
    }
    .badge-file {
      background: var(--badge-file-bg);
      color: var(--badge-file-text);
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      text-transform: none;
    }
    .fn-name {
      font-size: 1.25rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: var(--text);
    }
    .fn-header-right {
      display: flex;
      align-items: center;
    }
    .fn-source-tag {
      font-size: 0.8rem;
      color: var(--text-muted);
    }
    .fn-source-tag code {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: var(--text);
      background: var(--code-bg);
      padding: 2px 6px;
      border-radius: 4px;
      border: 1px solid var(--code-border);
    }
    .fn-body h2 {
      font-size: 1.05rem;
      font-weight: 500;
      color: var(--text-muted);
      margin-top: 0;
      margin-bottom: 14px;
    }
    .fn-body h3 {
      font-size: 0.85rem;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--primary);
      margin: 16px 0 6px;
      font-weight: 700;
    }
    .fn-body p {
      margin-bottom: 10px;
      font-size: 0.95rem;
      color: var(--text);
    }
    .fn-body pre {
      background: var(--code-bg);
      border: 1px solid var(--code-border);
      border-radius: 6px;
      padding: 12px 16px;
      overflow-x: auto;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      font-size: 0.9rem;
      color: var(--code-text);
      margin-bottom: 14px;
    }
    .fn-body code {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      background: var(--code-bg);
      color: var(--code-text);
      border: 1px solid var(--code-border);
      padding: 2px 6px;
      border-radius: 4px;
      font-size: 0.88rem;
    }
    .fn-body pre code {
      background: none;
      border: none;
      padding: 0;
      color: inherit;
    }
    .fn-body table {
      width: 100%;
      border-collapse: collapse;
      margin: 10px 0 16px;
      font-size: 0.92rem;
      color: var(--text);
    }
    .fn-body table td {
      padding: 8px 12px;
      border-bottom: 1px solid var(--border);
      vertical-align: top;
      color: var(--text);
    }
    .fn-body table td:first-child {
      width: 25%;
      font-weight: 600;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    }
    .fn-body ul {
      margin-left: 20px;
      margin-bottom: 10px;
      font-size: 0.95rem;
      color: var(--text);
    }
    .fn-body a {
      color: var(--primary);
      text-decoration: none;
    }
    .fn-body a:hover {
      text-decoration: underline;
    }
    kbd {
      background: var(--code-bg);
      border: 1px solid var(--border);
      border-radius: 4px;
      padding: 1px 5px;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      font-size: 0.8em;
      color: var(--text-muted);
    }
    .no-results {
      display: none;
      background: var(--surface);
      border: 1px dashed var(--border);
      border-radius: 8px;
      padding: 48px 24px;
      text-align: center;
      color: var(--text-muted);
    }
    .no-results h3 {
      font-size: 1.15rem;
      color: var(--text);
      margin-bottom: 6px;
    }
    ::-webkit-scrollbar {
      width: 8px;
      height: 8px;
    }
    ::-webkit-scrollbar-track {
      background: transparent;
    }
    ::-webkit-scrollbar-thumb {
      background: var(--border);
      border-radius: 4px;
    }
    ::-webkit-scrollbar-thumb:hover {
      background: var(--text-muted);
    }
    @media (max-width: 860px) {
      body {
        flex-direction: column;
        height: auto;
        overflow-y: auto;
      }
      aside {
        width: 100%;
        height: auto;
        max-height: 45vh;
        border-right: none;
        border-bottom: 1px solid var(--border);
      }
      main {
        padding: 20px;
        overflow-y: visible;
      }
    }
  </style>
</head>
<body>
  <aside>
    <div class="brand">
      <div class="brand-info">
        <h1>statmethods</h1>
        <p>Offline Exam Reference</p>
      </div>
      <button id="themeToggle" class="theme-toggle-btn" type="button" aria-label="Toggle theme" title="Toggle dark mode">
        <svg class="theme-icon sun-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="5"></circle><line x1="12" y1="1" x2="12" y2="3"></line><line x1="12" y1="21" x2="12" y2="23"></line><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line><line x1="1" y1="12" x2="3" y2="12"></line><line x1="21" y1="12" x2="23" y2="12"></line><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line></svg>
        <svg class="theme-icon moon-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path></svg>
        <span id="themeToggleLabel">Dark</span>
      </button>
    </div>
    <div class="search-box">
      <div class="search-wrapper">
        <input type="text" id="filterInput" placeholder="Filter functions or files (Ctrl+K, /)..." oninput="filterList()">
        <button id="clearSearch" class="search-clear" onclick="clearSearch()" title="Clear search">✕</button>
      </div>
    </div>
    <div class="nav-list" id="navList">
',
  paste(nav_sections_html, collapse = "\n"),
'
    </div>
  </aside>
  <main id="mainContent">
    <div class="header-banner">
      <h2>statmethods Function Manual (Offline)</h2>
      <p>Complete offline reference organized by source file. Use the left sidebar, file quick-jump pills, or search to instantly look up any function, parameters, return values, and exam examples.</p>
    </div>
    <div class="file-pills-bar">
      <span class="pills-label">Jump to file:</span>
',
  paste(pills_html, collapse = "\n"),
'
    </div>
',
  paste(main_sections_html, collapse = "\n"),
'
    <div id="noResults" class="no-results">
      <h3>No matching functions found</h3>
      <p>Try searching for a different keyword or press <kbd>Esc</kbd> to clear filter.</p>
    </div>
  </main>

  <script>
    // Theme toggle management
    function getStoredTheme() {
      try {
        return localStorage.getItem("statmethods-theme");
      } catch (e) {
        return null;
      }
    }

    function setStoredTheme(theme) {
      try {
        localStorage.setItem("statmethods-theme", theme);
      } catch (e) {}
    }

    function updateThemeUI(theme) {
      document.documentElement.setAttribute("data-theme", theme);
      var label = document.getElementById("themeToggleLabel");
      var btn = document.getElementById("themeToggle");
      if (label) {
        label.textContent = (theme === "dark") ? "Light" : "Dark";
      }
      if (btn) {
        var nextMode = (theme === "dark") ? "light" : "dark";
        btn.setAttribute("aria-label", "Switch to " + nextMode + " mode");
        btn.setAttribute("title", "Switch to " + nextMode + " mode");
      }
    }

    function toggleTheme() {
      var current = document.documentElement.getAttribute("data-theme") || "light";
      var next = (current === "dark") ? "light" : "dark";
      setStoredTheme(next);
      updateThemeUI(next);
    }

    // Initialize theme button
    (function initTheme() {
      var activeTheme = document.documentElement.getAttribute("data-theme") || "light";
      updateThemeUI(activeTheme);
      var btn = document.getElementById("themeToggle");
      if (btn) {
        btn.addEventListener("click", toggleTheme);
      }
    })();

    // Search and filtering across functions and files
    function filterList() {
      var input = document.getElementById("filterInput");
      var clearBtn = document.getElementById("clearSearch");
      var q = input.value.trim().toLowerCase();

      if (clearBtn) {
        clearBtn.style.display = q ? "block" : "none";
      }

      var totalVisible = 0;
      var fileSections = document.querySelectorAll(".file-section");
      var navSections = document.querySelectorAll(".nav-file-section");

      fileSections.forEach(function(sec) {
        var fileName = (sec.getAttribute("data-file") || "").toLowerCase();
        var cards = sec.querySelectorAll(".fn-card");
        var secMatches = 0;

        cards.forEach(function(card) {
          var fnName = (card.getAttribute("data-fn") || "").toLowerCase();
          var cardText = card.textContent.toLowerCase();
          var match = !q || fnName.indexOf(q) !== -1 || fileName.indexOf(q) !== -1 || cardText.indexOf(q) !== -1;

          card.style.display = match ? "" : "none";
          if (match) {
            secMatches++;
            totalVisible++;
          }
        });

        sec.style.display = (secMatches > 0) ? "" : "none";
      });

      navSections.forEach(function(group) {
        var fileName = (group.getAttribute("data-file") || "").toLowerCase();
        var items = group.querySelectorAll(".nav-sublist li");
        var groupMatches = 0;

        items.forEach(function(li) {
          var fnName = (li.getAttribute("data-fn") || li.textContent).toLowerCase();
          var match = !q || fnName.indexOf(q) !== -1 || fileName.indexOf(q) !== -1;

          li.style.display = match ? "" : "none";
          if (match) groupMatches++;
        });

        group.style.display = (groupMatches > 0 || (q && fileName.indexOf(q) !== -1)) ? "" : "none";
      });

      var noRes = document.getElementById("noResults");
      if (noRes) {
        noRes.style.display = (totalVisible === 0 && q !== "") ? "block" : "none";
      }
    }

    function clearSearch() {
      var input = document.getElementById("filterInput");
      if (input) {
        input.value = "";
        filterList();
        input.focus();
      }
    }

    // Keyboard navigation shortcuts
    document.addEventListener("keydown", function(e) {
      if ((e.key === "/" || (e.ctrlKey && e.key.toLowerCase() === "k")) && document.activeElement !== document.getElementById("filterInput")) {
        e.preventDefault();
        var fi = document.getElementById("filterInput");
        if (fi) {
          fi.focus();
          fi.select();
        }
      } else if (e.key === "Escape" && document.activeElement === document.getElementById("filterInput")) {
        clearSearch();
      }
    });

    // Smooth navigation for file pills (clearing search filter if target is hidden)
    document.querySelectorAll(".file-pill").forEach(function(pill) {
      pill.addEventListener("click", function(e) {
        var targetId = this.getAttribute("href");
        if (targetId && targetId.indexOf("#") === 0) {
          var el = document.querySelector(targetId);
          if (el && el.style.display === "none") {
            clearSearch();
          }
          if (el) {
            e.preventDefault();
            el.scrollIntoView({ behavior: "smooth", block: "start" });
            try { history.pushState(null, null, targetId); } catch (err) {}
          }
        }
      });
    });

    // Smooth navigation for sidebar links
    document.querySelectorAll(".nav-item, .nav-file-title").forEach(function(link) {
      link.addEventListener("click", function(e) {
        var targetId = this.getAttribute("href");
        if (targetId && targetId.indexOf("#") === 0) {
          var el = document.querySelector(targetId);
          if (el) {
            e.preventDefault();
            el.scrollIntoView({ behavior: "smooth", block: "start" });
            try { history.pushState(null, null, targetId); } catch (err) {}
          }
        }
      });
    });

    // Active item highlight on scroll
    var mainEl = document.getElementById("mainContent");
    if (mainEl) {
      mainEl.addEventListener("scroll", function() {
        var cards = document.querySelectorAll(".fn-card");
        var scrollPos = mainEl.scrollTop + 120;
        var activeId = null;

        cards.forEach(function(card) {
          if (card.offsetTop <= scrollPos) {
            activeId = card.id;
          }
        });

        if (activeId) {
          document.querySelectorAll(".nav-item").forEach(function(item) {
            var href = item.getAttribute("href");
            if (href === "#" + activeId) {
              item.classList.add("active");
            } else {
              item.classList.remove("active");
            }
          });
        }
      });
    }
  </script>
</body>
</html>'
)

writeLines(html_page, file.path(out_dir, "documentation.html"))
cat(paste0("      Created: documentation.html (Self-contained offline manual)\n"))

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
script_files <- list.files("statmethods_source/R", full.names = TRUE, pattern = "\\\\.R$")
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
    avail <- sort(sub("\\\\.Rd$", "", list.files(man_dir, pattern = "\\\\.Rd$")))
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
