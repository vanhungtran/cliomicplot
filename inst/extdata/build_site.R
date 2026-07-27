# Build pkgdown site — fresh process to avoid stale package cache
pkgdown::build_site("C:/Users/tranh/OneDrive/Statistics/cliomicplot",
                     new_process = TRUE,
                     preview = FALSE)
cat("\n===== Site built =====\n")
