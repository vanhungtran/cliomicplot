# Build vignettes using devtools::load_all for proper function resolution
devtools::load_all("C:/Users/tranh/OneDrive/Statistics/cliomicplot", quiet = TRUE)

# Verify functions exist
stopifnot(exists("type_infobar"))
stopifnot(exists("type_rankabundance"))
stopifnot(exists("type_pattern"))
stopifnot(exists("type_deg_compare"))
cat("All new functions verified\n")

library(rmarkdown)

cat("\n===== Building multiomics.Rmd =====\n")
render("C:/Users/tranh/OneDrive/Statistics/cliomicplot/vignettes/multiomics.Rmd",
       output_format = "html_vignette", quiet = FALSE)

cat("\n===== Building oncology.Rmd =====\n")
render("C:/Users/tranh/OneDrive/Statistics/cliomicplot/vignettes/oncology.Rmd",
       output_format = "html_vignette", quiet = FALSE)

cat("\n===== Done =====\n")
