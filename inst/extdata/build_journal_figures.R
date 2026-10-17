# ===========================================================================
# Regenerate the Nature/Cell/Science journal-style gallery figures
# used in README.md and the `journal-plots` vignette.
#
# Run from the package root:
#   Rscript inst/extdata/build_journal_figures.R
#
# Produces man/figures/{roc,gsea,upset,venn,slope,confusion,donut}.png
# at 300 dpi with Nature-style typography (no in-figure titles, bold
# lowercase panel labels, black text on white, thin black axes).
# ===========================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages(library(ggplot2))
clipar(stat.test = NULL)

fig_dir  = "man/figures"
data_dir = "inst/extdata"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

resolve_data = function(filename) {
  pkg_path = system.file("extdata", filename, package = "cliomicplot")
  if (pkg_path != "" && file.exists(pkg_path)) return(pkg_path)
  file.path(data_dir, filename)
}

save_fig = function(p, file, width_mm, height_mm, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, file),
    plot     = p,
    width    = width_mm,
    height   = height_mm,
    units    = "mm",
    dpi      = dpi
  )
  cat("saved", file, sprintf("(%dx%d mm, %d dpi)\n", width_mm, height_mm, dpi))
}

# ---------------------------------------------------------------------------
# Data: AD/PD cortex proteomics
# ---------------------------------------------------------------------------
load(resolve_data("ADPD_cortex_Maxquant.RData"))   # data_long, data_wide, MetaData, results_long

# --- Fig a: ROC (AD vs CTL) -------------------------------------------------
meta2   = MetaData[MetaData$group %in% c("AD", "CTL"), ]
samples = as.character(meta2$sampleid)
markers = c("SMOC1_A0A024R6E0", "APP_A0A218KGR2", "MDK_E9PPJ5")
marker_short = c("SMOC1", "APP", "MDK")

roc_df = data.frame(
  disease = factor(meta2$group, levels = c("CTL", "AD")),
  t(data_wide[markers, samples])
)
names(roc_df)[-1] = marker_short

p_roc = cliplot(roc_df,
                type = type_roc(predictors = marker_short, response = "disease"),
                theme = "nature", palette = "jco") +
  cli_panel_label("a")
save_fig(p_roc, "roc.png", 105, 105)

# --- Fig b: GSEA-style dot plot (top 25 DE proteins) ------------------------
r_adctl = results_long[results_long$test == "ADvsCTL", ]
r_adctl = r_adctl[order(r_adctl$Adj.P.Value), ]

top_de = data.frame(
  Term = sub("_.*$", "", r_adctl$UniqueID[1:25]),
  NES  = r_adctl$logFC[1:25],
  FDR  = r_adctl$Adj.P.Value[1:25]
)

p_gsea = cliplot(top_de, type = "gsea", top_term = 25, theme = "nature") +
  cli_panel_label("b")
save_fig(p_gsea, "gsea.png", 115, 150)

# --- Fig c: UpSet (DEG overlap across six comparisons) ----------------------
deg_sets = lapply(unique(results_long$test), function(tt) {
  sub = results_long[results_long$test == tt, ]
  sub$UniqueID[sub$Adj.P.Value < 0.05 & abs(sub$logFC) > 0.5]
})
names(deg_sets) = unique(results_long$test)
deg_sets = deg_sets[vapply(deg_sets, length, integer(1)) > 0]

p_upset = cliplot(deg_sets, type = "upset", theme = "nature") +
  cli_panel_label("c")
save_fig(p_upset, "upset.png", 160, 110)

# --- Fig d: Venn (three contrasts) ------------------------------------------
p_venn = cliplot(deg_sets[c("ADvsCTL", "PDvsCTL", "ADPDvsCTL")],
                 type = "venn", theme = "nature") +
  cli_panel_label("d")
save_fig(p_venn, "venn.png", 105, 105)

# --- Fig e: Slope (top 12 DE proteins, AD vs CTL) ---------------------------
top12 = r_adctl$UniqueID[1:12]
sub = data_long[data_long$UniqueID %in% top12 & data_long$group %in% c("AD", "CTL"), ]
means = stats::aggregate(expr ~ UniqueID + group, data = sub, FUN = mean)
wide  = stats::reshape(means, idvar = "UniqueID", timevar = "group", direction = "wide")
names(wide) = c("protein", "AD", "CTL")

slope_df = data.frame(
  subject   = rep(wide$protein, 2),
  condition = rep(c("AD", "CTL"), each = nrow(wide)),
  value     = c(wide$AD, wide$CTL)
)

p_slope = cliplot(value ~ condition, data = slope_df,
                  type = type_slope(pair = "subject"),
                  theme = "nature", palette = "nejm") +
  cli_panel_label("e") +
  labs(y = "Mean abundance (log2)")
save_fig(p_slope, "slope.png", 125, 95)

# --- Fig f: Confusion matrix (stroke vs sham classifier) --------------------
load(resolve_data("StrokeBrain_TimeCourse.RData"))   # data_long, results_stat, MetaData

sub = data_long[data_long$UniqueID == "Lxn_P70202" &
                data_long$treatment %in% c("StrkIp", "ShamIp"), ]
sub$stroke = factor(ifelse(sub$treatment == "StrkIp", "Stroke", "Sham"),
                    levels = c("Sham", "Stroke"))
thr = mean(c(median(sub$expr[sub$stroke == "Stroke"]),
             median(sub$expr[sub$stroke == "Sham"])))
sub$pred = factor(ifelse(sub$expr > thr, "Stroke", "Sham"),
                  levels = levels(sub$stroke))

p_conf = cliplot(stroke ~ pred, data = sub,
                 type = type_confusion(add_pvalue = TRUE),
                 theme = "nature") +
  cli_panel_label("f")
save_fig(p_conf, "confusion.png", 105, 105)

# --- Fig g: Donut (study composition) ---------------------------------------
load(resolve_data("AgingHFCD_Metabolomics.RData"))   # MetaData, results_long

comp = as.data.frame(table(MetaData$group))
names(comp) = c("group", "n")

p_donut = cliplot(n ~ group, data = comp,
                  type = type_donut(center_label = "samples",
                                    center_value = as.character(nrow(MetaData))),
                  theme = "nature") +
  cli_panel_label("g")
save_fig(p_donut, "donut.png", 105, 105)

cat("All 7 journal-style figures regenerated in", fig_dir, "\n")