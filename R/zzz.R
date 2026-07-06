# ===========================================================================
# cliomicplot: Package startup (zzz.R)
# ===========================================================================

# Non-standard-evaluation symbols used inside aes()/ggalluvial data-masks that
# R CMD check cannot see are bound at compile time. Declaring them here keeps
# the "no visible binding for global variable" NOTE clear.
utils::globalVariables(c(":=", "level", "stratum"))

.onLoad = function(libname, pkgname) {
  # Set default ggplot2 theme to a clean base
  ggplot2::theme_set(theme_cli_base())

  # Initialize environment
  set_environment_variable(
    .saved_par_before = NULL,
    .saved_par_after  = NULL,
    .last_call        = NULL,
    .clipar_hooks     = NULL,
    .registered_themes = list(),
    .active_theme     = "cli_bw"
  )
}

.onAttach = function(libname, pkgname) {
  packageStartupMessage(
    "cliomicplot ", utils::packageVersion("cliomicplot"),
    " - Publication-ready clinical & omics plots\n",
    "Main function: cliplot() | Themes: clitheme() | Params: clipar()"
  )
}
