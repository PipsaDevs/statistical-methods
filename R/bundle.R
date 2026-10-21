#' Bundle a Function and All Its Custom Dependencies
#'
#' Scans a function, recursively extracts all custom prerequisite functions
#' it depends on, formats them in proper topological order, and optionally
#' copies the full standalone code block to the system clipboard.
#'
#' @param fn Function name (as a bare symbol or a character string).
#' @param copy Logical, whether to automatically copy to the clipboard.
#' @return Invisibly returns the concatenated code string.
#' @examples
#' \dontrun{
#' bundle(p_skewness)
#' bundle(linear_correlation)
#' }
#' @export
bundle <- function(fn, copy = TRUE, env = parent.frame()) {
  fn_expr <- substitute(fn)
  fn_name <- if (is.character(fn_expr)) fn_expr else deparse(fn_expr)

  if (!exists(fn_name, envir = env, mode = "function")) {
    stop(paste("Function", fn_name, "not found in environment!"))
  }

  # Identify all custom functions defined in the global environment
  all_funcs <- ls(envir = env)
  all_funcs <- all_funcs[
    sapply(all_funcs, function(x) is.function(get(x, envir = env)))
  ]
  all_funcs <- setdiff(all_funcs, c("bundle"))

  # Recursive dependency collector
  get_deps <- function(name, visited = character()) {
    if (name %in% visited) {
      return(character())
    }
    visited <- c(visited, name)
    f <- get(name, envir = env)
    symbols <- all.names(body(f))
    deps <- intersect(symbols, all_funcs)
    deps <- setdiff(deps, name)

    res <- character()
    for (d in deps) {
      res <- c(res, get_deps(d, visited), d)
      visited <- unique(c(visited, res))
    }
    unique(res)
  }

  # Topological order: prerequisites first, target function last
  ordered_deps <- c(get_deps(fn_name), fn_name)

  # Format each function definition
  code_blocks <- character()
  for (f in ordered_deps) {
    f_obj <- get(f, envir = env)
    code_lines <- deparse(f_obj)
    code_lines[1] <- paste0(f, " <- ", code_lines[1])
    code_blocks <- c(code_blocks, paste(code_lines, collapse = "\n"))
  }

  full_code <- paste(code_blocks, collapse = "\n\n")

  # Output to console
  cat("\n# ==================================================\n")
  cat(paste0("# STANDALONE BUNDLE: ", fn_name, " (", length(ordered_deps), " functions included)\n"))
  cat("# ==================================================\n\n")
  cat(full_code)
  cat("\n\n# ==================================================\n\n")

  # Copy to clipboard (works natively on Windows lab PCs)
  if (copy) {
    copied <- FALSE
    if (.Platform$OS.type == "windows") {
      try(
        {
          utils::writeClipboard(full_code)
          copied <- TRUE
        },
        silent = TRUE
      )
    } else {
      # Linux fallback
      try(
        {
          con <- pipe("xclip -selection clipboard", "w")
          cat(full_code, file = con)
          close(con)
          copied <- TRUE
        },
        silent = TRUE
      )
    }
    if (copied) {
      cat("[i] Copied to clipboard! Just press Ctrl + V in your exam file.\n\n")
    }
  }

  invisible(full_code)
}
