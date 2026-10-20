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
cards_html <- c()
nav_links <- c()
temp_html_dir <- file.path(tempdir(), "rd_html_temp")
if (dir.exists(temp_html_dir)) unlink(temp_html_dir, recursive = TRUE)
dir.create(temp_html_dir, showWarnings = FALSE)

for (f in rd_files) {
  fn_name <- sub("\\.Rd$", "", basename(f))
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
  
  card <- paste0(
    "    <article class=\"fn-card\" id=\"fn-", fn_name, "\">\n",
    "      <div class=\"fn-header\">\n",
    "        <span class=\"badge\">Function</span>\n",
    "        <h2>", fn_name, "</h2>\n",
    "      </div>\n",
    "      <div class=\"fn-body\">\n", content, "\n      </div>\n",
    "    </article>\n"
  )
  cards_html <- c(cards_html, card)
  nav_links <- c(nav_links, paste0("      <li><a href=\"#fn-", fn_name, "\" class=\"nav-item\">", fn_name, "</a></li>"))
}
unlink(temp_html_dir, recursive = TRUE)

html_page <- paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>statmethods — Offline Exam Documentation</title>
  <style>
    :root {
      --bg: #f8fafc;
      --surface: #ffffff;
      --primary: #2563eb;
      --primary-hover: #1d4ed8;
      --text: #0f172a;
      --text-muted: #64748b;
      --border: #e2e8f0;
      --code-bg: #f1f5f9;
      --sidebar-w: 280px;
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
    }
    aside {
      width: var(--sidebar-w);
      background: var(--surface);
      border-right: 1px solid var(--border);
      display: flex;
      flex-direction: column;
      flex-shrink: 0;
    }
    .brand {
      padding: 16px 20px;
      border-bottom: 1px solid var(--border);
      background: #0f172a;
      color: white;
    }
    .brand h1 { font-size: 1.15rem; font-weight: 700; letter-spacing: -0.02em; }
    .brand p { font-size: 0.78rem; color: #94a3b8; margin-top: 2px; }
    .search-box { padding: 12px 16px; border-bottom: 1px solid var(--border); }
    .search-box input {
      width: 100%;
      padding: 8px 12px;
      font-size: 0.9rem;
      border: 1px solid var(--border);
      border-radius: 6px;
      outline: none;
    }
    .search-box input:focus { border-color: var(--primary); ring: 2px var(--primary); }
    .nav-list {
      list-style: none;
      overflow-y: auto;
      flex: 1;
      padding: 8px;
    }
    .nav-item {
      display: block;
      padding: 7px 12px;
      font-size: 0.88rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: #334155;
      text-decoration: none;
      border-radius: 4px;
      transition: background 0.15s;
    }
    .nav-item:hover { background: #eff6ff; color: var(--primary); }
    main {
      flex: 1;
      overflow-y: auto;
      padding: 32px 48px 100px;
      scroll-behavior: smooth;
    }
    .header-banner {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 20px 24px;
      margin-bottom: 32px;
    }
    .header-banner h2 { font-size: 1.4rem; margin-bottom: 8px; }
    .header-banner p { color: var(--text-muted); font-size: 0.95rem; }
    .fn-card {
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 24px;
      margin-bottom: 24px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.03);
    }
    .fn-header {
      display: flex;
      align-items: center;
      gap: 12px;
      border-bottom: 1px solid var(--border);
      padding-bottom: 12px;
      margin-bottom: 16px;
    }
    .badge {
      background: #dbeafe;
      color: #1e40af;
      font-size: 0.75rem;
      font-weight: 700;
      padding: 2px 8px;
      border-radius: 4px;
      text-transform: uppercase;
    }
    .fn-header h2 {
      font-size: 1.25rem;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      color: #1e293b;
    }
    .fn-body h2 { font-size: 1.1rem; color: #1e293b; margin: 16px 0 8px; }
    .fn-body h3 { font-size: 0.95rem; text-transform: uppercase; letter-spacing: 0.05em; color: var(--text-muted); margin: 16px 0 6px; }
    .fn-body p { margin-bottom: 10px; font-size: 0.95rem; }
    .fn-body pre {
      background: var(--code-bg);
      border: 1px solid var(--border);
      border-radius: 6px;
      padding: 12px 16px;
      overflow-x: auto;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      font-size: 0.9rem;
      margin-bottom: 14px;
    }
    .fn-body code {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      background: var(--code-bg);
      padding: 2px 6px;
      border-radius: 4px;
      font-size: 0.88rem;
    }
    .fn-body pre code { background: none; padding: 0; }
    .fn-body table {
      width: 100%;
      border-collapse: collapse;
      margin: 10px 0 16px;
      font-size: 0.92rem;
    }
    .fn-body table td {
      padding: 8px 12px;
      border-bottom: 1px solid var(--border);
      vertical-align: top;
    }
    .fn-body table td:first-child { width: 25%; font-weight: 600; font-family: ui-monospace, monospace; }
    .fn-body ul { margin-left: 20px; margin-bottom: 10px; font-size: 0.95rem; }
  </style>
</head>
<body>
  <aside>
    <div class="brand">
      <h1>statmethods</h1>
      <p>Offline Exam Reference</p>
    </div>
    <div class="search-box">
      <input type="text" id="filterInput" placeholder="Filter functions (Ctrl+F)..." oninput="filterList()">
    </div>
    <ul class="nav-list" id="navList">
',
  paste(nav_links, collapse = "\n"),
'
    </ul>
  </aside>
  <main id="mainContent">
    <div class="header-banner">
      <h2>statmethods Function Manual (Offline)</h2>
      <p>Use the left sidebar or the search bar above to instantly find any function, formula arguments, return values, and code examples.</p>
    </div>
',
  paste(cards_html, collapse = "\n"),
'
  </main>
  <script>
    function filterList() {
      const q = document.getElementById("filterInput").value.toLowerCase();
      const items = document.querySelectorAll("#navList li");
      const cards = document.querySelectorAll(".fn-card");
      items.forEach(li => {
        const text = li.textContent.toLowerCase();
        li.style.display = text.includes(q) ? "" : "none";
      });
      cards.forEach(card => {
        const text = card.textContent.toLowerCase();
        card.style.display = text.includes(q) ? "" : "none";
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
