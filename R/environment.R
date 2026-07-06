# ===========================================================================
# cliomicplot: Environment Management
# Inspired by tinyplot's environment.R
# ===========================================================================

# Package-level state environment (similar to tinyplot's .tinyplot_env)
.cliomicplot_env = new.env()

# Query environment variable
get_environment_variable = function(name = NULL) {
  if (is.null(name)) return(as.list(.cliomicplot_env))
  name = as.character(name)[1L]
  return(.cliomicplot_env[[name]])
}

# Set environment variable(s)
set_environment_variable = function(...) {
  dots = list(...)
  if (is.null(names(dots))) {
    stop("arguments must be named")
  } else if (any(names(dots) == "")) {
    warning("ignoring unnamed arguments")
    dots = dots[names(dots) != ""]
  }

  if (length(dots) > 0L) {
    for (i in names(dots)) {
      .cliomicplot_env[[i]] = dots[[i]]
    }
  }
  invisible(dots)
}

# Initialize all environment variables with NULL
set_environment_variable(
  .saved_par_before = NULL,
  .saved_par_after  = NULL,
  .last_call        = NULL,
  .clipar_hooks     = NULL,
  .registered_themes = NULL,
  .active_theme     = NULL
)
