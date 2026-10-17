# ===========================================================================
# Recreate key figures of the SpaMTP manuscript (Nature Methods, 2026,
# DOI: 10.1038/s41592-026-03140-8; GenomicsMachineLearning/SpaMTP) using
# cliomicplot plot types and the paper's public datasets.
#
# Data availability (per the paper):
#   - Mouse urinary bladder MALDI-MSI (PRIDE PXD001283) -> Zenodo SpaMTP
#     community, "SpaMTP: Mouse Bladder dataset" (CC-BY-4.0)
#   - Parkinson mouse brain Visium + MALDI (SciLifeLab
#     10.17044/scilifelab.22770161.v2) -> "SpaMTP: Multi-Omics Mouse Brain
#     dataset"
#   - Human brain tumour single-cell multi-omics -> "SpaMTP: Human Brain
#     Tissue Single Cell dataset" (pathway results)
#   - Spotted mouse liver MALDI-MSI (METASPACE 2020-12-07_03h16m14s) ->
#     "SpaMTP: Mouse Liver with Spotted Chemical Standards dataset"
#
# Run from the package root:
#   Rscript inst/extdata/build_spamtp_figures.R
#
# The first run downloads the processed Seurat objects from Zenodo into
# inst/extdata/spamtp_paper/raw/ (one-time, ~90 MB). Derived analysis tables
# are cached as .rda bundles in inst/extdata/spamtp_paper/ so later runs are
# offline. Figures are written to man/figures/spamtp_fig{1..4}.png at 300 dpi
# (Nature style: no in-figure titles, bold lowercase panel labels, black on
# white, thin black axes).
# ===========================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages({library(ggplot2); library(patchwork); library(Matrix)})
clipar(stat.test = NULL)

fig_dir  = "man/figures"
data_dir = "inst/extdata/spamtp_paper"
raw_dir  = file.path(data_dir, "raw")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

ZENODO_URLS = c(
  bladder_annotated = "https://zenodo.org/records/17246684/files/bladder_annotated.RDS?download=1",
  msi_transformed   = "https://zenodo.org/records/17246900/files/SpaMTP_msi_transformed.RDS?download=1",
  visium            = "https://zenodo.org/records/17246900/files/visium.RDS?download=1",
  tumour_pathways   = "https://zenodo.org/records/18282678/files/brain_tumour_combined_pathways.rds?download=1",
  spotted_liver     = "https://zenodo.org/records/17289187/files/spotted_Annotated_H_Cl_Adducts.RDS?download=1"
)
resolve_data = function(filename) {
  pkg_path = system.file("extdata", "spamtp_paper", filename, package = "cliomicplot")
  if (pkg_path != "" && file.exists(pkg_path)) return(pkg_path)
  file.path(data_dir, filename)
}

load_data = function(filename) {
  e = new.env(parent = emptyenv())
  load(resolve_data(filename), envir = e)
  as.list(e)
}

save_fig = function(p, file, width_mm, height_mm, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, file), plot = p,
    width = width_mm, height = height_mm, units = "mm", dpi = dpi
  )
  cat("saved", file, sprintf("(%dx%d mm, %d dpi)\n", width_mm, height_mm, dpi))
}

save_preview = function(png_file, jpg_file, width_px = 1100) {
  img = png::readPNG(file.path(fig_dir, png_file))
  h = round(nrow(img) * width_px / ncol(img))
  jpeg::writeJPEG(
    img[seq(1, nrow(img), length.out = h), seq(1, ncol(img), length.out = width_px), ],
    quality = 0.88, target = file.path(fig_dir, jpg_file)
  )
  cat("saved", jpg_file, "\n")
}

hm_label = function(p, lab) {
  p = p + cli_panel_label(lab)
  suppressMessages(p +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1),
                             expand = FALSE, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 6, 6, 6)))
}

# ---- Nature-style spatial map base -----------------------------------------
spatial_theme = function() {
  theme_void(base_size = 8) +
    theme(
      plot.margin = margin(4, 4, 4, 4),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.4),
      legend.position = "right",
      legend.key.height = unit(3.2, "mm"),
      legend.key.width  = unit(1.6, "mm"),
      legend.title = element_text(size = 6.5, face = "bold"),
      legend.text  = element_text(size = 5.5)
    )
}

sp_label = function(p, lab, dat, x = "x", y = "y", size = 3) {
  xr = range(dat[[x]], na.rm = TRUE)
  yr = range(dat[[y]], na.rm = TRUE)
  p + ggplot2::annotate("text",
        x = xr[1] + 0.035 * diff(xr), y = yr[2] - 0.035 * diff(yr),
        label = lab, hjust = 0, vjust = 1,
        size = size, fontface = "bold", colour = "black")
}

mz_spatial = function(pix, x, y, mzcol, lab, fill_name = "Intensity\n(log10)") {
  p = ggplot(pix, aes(.data[[x]], .data[[y]], colour = .data[[mzcol]])) +
    geom_point(size = 0.45, stroke = 0) +
    scale_colour_gradientn(colours = viridisLite::viridis(100), name = fill_name) +
    coord_equal() + spatial_theme() +
    labs(x = NULL, y = NULL)
  if (!is.null(lab)) p = sp_label(p, lab, pix, x, y)
  p
}
# ===========================================================================
# DATA EXTRACTION (one-time; cached as .rda bundles in inst/extdata/spamtp_paper)
# ===========================================================================

ensure_raw = function(name, local) {
  path = file.path(raw_dir, local)
  if (!file.exists(path)) {
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Downloading", name, "from Zenodo ...\n")
    utils::download.file(ZENODO_URLS[[name]], path, mode = "wb", quiet = TRUE)
  }
  path
}

bundle_path = function(file) file.path(data_dir, file)

extract_bladder = function() {
  out = bundle_path("spa_bladder.rda")
  if (file.exists(out)) return(invisible())
  raw = ensure_raw("bladder_annotated", "bladder_annotated.RDS")
  obj = readRDS(raw)
  md  = attr(obj, "meta.data")
  a   = attr(obj, "assays")$Spatial
  lay = SeuratObject::GetAssayData(obj, assay = "Spatial", layer = "counts")
  adm = attr(a, "meta.data")
  cells = colnames(lay)

  region_map = c("2" = "Adventitia", "5" = "Muscle", "6" = "Urothelium")
  short = vapply(strsplit(adm$Species.Name.Simple, "; "), `[`, "", 1)

  mz_sm  = adm$mz_names[grep("SM\\(34:1\\)", adm$Species.Name.Simple)[1]]
  mz_pc  = adm$mz_names[grep("PC\\(34:1\\)", adm$Species.Name.Simple)[1]]
  mz_sm0 = adm$mz_names[grep("SM\\(d18:0/16:0\\)", adm$all_IsomerNames)[1]]
  mz_lac = adm$mz_names[grep("LacCer\\(d18:1/12:0\\)", adm$all_IsomerNames)[1]]
  key_mzs = c("SM(34:1)" = mz_sm, "PC(34:1)" = mz_pc,
              "SM(d18:0/16:0)" = mz_sm0, "LacCer(d18:1/12:0)" = mz_lac)
  stopifnot(!anyNA(key_mzs))
  pix = data.frame(pixel = cells, x = md$x_coord, y = md$y_coord,
                   ssc = md$ssc, stringsAsFactors = FALSE)
  pix$region = region_map[pix$ssc]
  pix$region[is.na(pix$region)] = "Other"
  ints = as.matrix(t(lay[key_mzs, , drop = FALSE]))
  colnames(ints) = names(key_mzs)
  pix = cbind(pix, ints)
  pix$region = factor(pix$region, levels = c("Adventitia", "Muscle", "Urothelium", "Other"))

  # sampled per-pixel intensities for boxplots (Nature: keep panels light)
  set.seed(42)
  keep = pix[pix$region != "Other", ]
  s_idx = unlist(lapply(split(seq_len(nrow(keep)), keep$region), function(idx) {
    if (length(idx) > 8000) sample(idx, 8000) else idx
  }))
  samp  = keep[sort(s_idx), c("pixel", "region", names(key_mzs))]
  mzbox = reshape2::melt(samp, id.vars = c("pixel", "region"),
                         variable.name = "metabolite", value.name = "intensity")
  mzbox$intensity = log2(mzbox$intensity + 1)
  mzbox$metabolite = factor(mzbox$metabolite, levels = names(key_mzs))

  # pseudo-bulk limma DE (as in SpaMTP FindAllDEMs): 5 random pools / region
  pool_counts = function(counts, idx, k = 5) {
    idx = sample(idx)
    grp = rep(seq_len(k), length.out = length(idx))
    sapply(seq_len(k), function(g) Matrix::rowSums(counts[, idx[grp == g], drop = FALSE]))
  }
  set.seed(1)
  regs = c("2", "5", "6")
  pooled = do.call(cbind, lapply(regs, function(r) pool_counts(lay, which(pix$ssc == r))))
  colnames(pooled) = paste0(rep(regs, each = 5), "_", rep(1:5, times = 3))
  cpm  = sweep(pooled, 2, colSums(pooled), "/") * 1e6
  expr = log2(cpm + 1)
  design = model.matrix(~ 0 + factor(rep(regs, each = 5)))
  colnames(design) = paste0("r", regs)
  fit = limma::lmFit(expr, design)
  dem = do.call(rbind, lapply(regs, function(r) {
    others = setdiff(regs, r)
    ct = limma::makeContrasts(
      contrasts = sprintf("r%s - (r%s)/2", r, paste(others, collapse = " + r")),
      levels = design)
    e2 = limma::eBayes(limma::contrasts.fit(fit, ct))
    tt = limma::topTable(e2, number = Inf, sort.by = "none")
    tt$feature = rownames(tt); tt$cluster = r
    tt
  }))
  dem$Species    = short[match(dem$feature, adm$mz_names)]
  dem$Isomers    = adm$all_IsomerNames[match(dem$feature, adm$mz_names)]
  dem$Category   = adm$Lipid.Maps.Category[match(dem$feature, adm$mz_names)]
  dem$MainClass  = adm$Lipid.Maps.Main.Class[match(dem$feature, adm$mz_names)]
  dem$Regulation = ifelse(dem$logFC > 0, "Up", "Down")

  # muscle (5) vs urothelium (6) contrast for the volcano
  ct56 = limma::makeContrasts(r5 - r6, levels = design)
  e56  = limma::eBayes(limma::contrasts.fit(fit, ct56))
  vol  = limma::topTable(e56, number = Inf, sort.by = "none")
  vol$feature  = rownames(vol)
  vol$Species  = short[match(vol$feature, adm$mz_names)]
  vol$Isomers  = adm$all_IsomerNames[match(vol$feature, adm$mz_names)]
  vol$raw_mz   = adm$raw_mz[match(vol$feature, adm$mz_names)]
  dup = duplicated(vol$Species) | duplicated(vol$Species, fromLast = TRUE)
  vol$Species_Label = ifelse(dup, paste0(vol$Species, " (", round(vol$raw_mz, 2), ")"),
                             vol$Species)
  rownames(vol) = vol$Species_Label

  # UP lipid categories per cluster (for the circular "lipid shift" panel)
  up = dem[dem$Regulation == "Up" & dem$adj.P.Val < 0.05, ]
  up$category = ifelse(is.na(up$Category), "Non-Lipids", up$Category)
  lipid_counts = aggregate(list(count = up$feature),
                           by = list(cluster = up$cluster, category = up$category),
                           FUN = length)

  save(pix, mzbox, adm, dem, vol, lipid_counts, key_mzs,
       file = out, compress = "xz")
  cat("saved", out, "\n")
}
extract_brain_msi = function() {
  out = bundle_path("spa_brain_msi.rda")
  if (file.exists(out)) return(invisible())
  raw = ensure_raw("msi_transformed", "msi_transformed.RDS")
  obj = readRDS(raw)
  md  = attr(obj, "meta.data")
  a   = attr(obj, "assays")$Spatial
  lay = SeuratObject::GetAssayData(obj, assay = "Spatial", layer = "counts")
  adm = attr(a, "meta.data")
  mz_dop2 = adm$mz_names[which.min(abs(as.numeric(gsub("mz-", "", adm$mz_names)) - 674.28))]
  mz_dop1 = adm$mz_names[grep("Dopamine \\(single", adm$all_IsomerNames)[1]]
  pix = data.frame(x = md$x_coord, y = md$y_coord)
  pix$dopamine2 = log10(as.numeric(lay[mz_dop2, ]) + 1)
  pix$dopamine1 = log10(as.numeric(lay[mz_dop1, ]) + 1)
  save(pix, adm, mz_dop1, mz_dop2, file = out, compress = "xz")
  cat("saved", out, "\n")
}

extract_visium = function() {
  out = bundle_path("spa_brain_visium.rda")
  if (file.exists(out)) return(invisible())
  raw = ensure_raw("visium", "visium.RDS")
  obj = readRDS(raw)
  md = attr(obj, "meta.data")
  fov = attr(obj, "images")$slice1
  coords = as.data.frame(attr(attr(fov, "boundaries")$centroids, "coords"))
  coords$cell = rownames(md)   # centroids are stored in cell order (no dimnames)
  attr(obj, "images") = list()   # strip legacy VisiumV2 FOV (breaks Seurat 5 validation)
  sp = data.frame(spot = rownames(md),
                  x = coords$x[match(rownames(md), coords$cell)],
                  y = coords$y[match(rownames(md), coords$cell)],
                  region = md$region, lesion = md$lesion,
                  annotations = md$annotations, stringsAsFactors = FALSE)
  sp$annotations[sp$annotations == ""] = NA
  obj = Seurat::NormalizeData(obj, verbose = FALSE)
  obj = Seurat::FindVariableFeatures(obj, nfeatures = 2000, verbose = FALSE)
  obj = Seurat::ScaleData(obj, verbose = FALSE)
  obj = Seurat::RunPCA(obj, npcs = 10, verbose = FALSE)
  emb = as.data.frame(Seurat::Embeddings(obj, "pca")[, 1:2])
  names(emb) = c("PC1", "PC2")
  sp = cbind(sp, emb[rownames(md), ])

  # striatum differential expression: lesioned vs intact
  stri = subset(obj, cells = rownames(md)[md$region == "striatum"])
  Seurat::Idents(stri) = "lesion"
  deg = Seurat::FindMarkers(stri, ident.1 = "lesioned", ident.2 = "intact",
                            logfc.threshold = 0, min.pct = 0.1, verbose = FALSE)
  deg$gene = rownames(deg)
  nd  = Seurat::GetAssayData(stri, assay = "Spatial", layer = "data")
  grp = stri$lesion
  top_genes = deg$gene[order(deg$p_val_adj, -abs(deg$avg_log2FC))][seq_len(min(20, nrow(deg)))]
  top_genes = top_genes[!is.na(top_genes)]
  means = sapply(c("lesioned", "intact"), function(g)
    Matrix::rowMeans(nd[top_genes, grp == g, drop = FALSE]))
  colnames(means) = c("Lesioned", "Intact")
  save(sp, deg, means, top_genes, file = out, compress = "xz")
  cat("saved", out, "\n")
}

extract_tumour = function() {
  out = bundle_path("spa_tumour_pathways.rda")
  if (file.exists(out)) return(invisible())
  raw = ensure_raw("tumour_pathways", "brain_tumour_combined_pathways.rds")
  dt = as.data.frame(readRDS(raw))
  save(dt, file = out, compress = "xz")
  cat("saved", out, "\n")
}
extract_liver = function() {
  out = bundle_path("spa_liver.rda")
  if (file.exists(out)) return(invisible())
  raw = ensure_raw("spotted_liver", "spotted_liver.RDS")
  obj = readRDS(raw)
  md  = attr(obj, "meta.data")
  a   = attr(obj, "assays")$Spatial
  lay = SeuratObject::GetAssayData(obj, assay = "Spatial", layer = "counts")
  adm = attr(a, "meta.data")

  pix = data.frame(x = md$x, y = md$y, cluster = md$clusters, stringsAsFactors = FALSE)
  gi = grep("Glutathione", adm$all_IsomerNames)[1]
  gz = adm$mz_names[gi]
  pix$glutathione = log10(as.numeric(lay[gz, ]) + 1)

  qc = data.frame(
    mz        = adm$observed_mz,
    annotated = adm$all_IsomerNames != "No Annotation",
    n_isomers = lengths(strsplit(adm$all_IsomerNames, "; ")),
    adducts   = adm$all_Adducts,
    formula   = !(adm$all_Formulas %in% c("No Annotation", NA)),
    err_min   = vapply(strsplit(adm$all_Errors, "; "),
                       function(x) suppressWarnings(min(as.numeric(x))), numeric(1)),
    stringsAsFactors = FALSE)
  qc$ppm = qc$err_min / qc$mz * 1e6
  save(pix, qc, gz, file = out, compress = "xz")
  cat("saved", out, "\n")
}

extract_all = function() {
  extract_bladder()
  extract_brain_msi()
  extract_visium()
  extract_tumour()
  extract_liver()
}
# ===========================================================================
# FIGURES
# ===========================================================================

region_cols = c(Adventitia = "#D55E00", Muscle = "#0072B2",
                Urothelium = "#009E73", Other = "#C9C9C9")
annot_cols = c(
  "CP_lesioned" = "#348BC2", "CP_intact" = "#B3DFF2", "ACB_lesioned" = "#D16D08",
  "ACB_intact" = "#FAA957", "CTX" = "#AEDEA0", "LSX" = "#F37985",
  "MSC" = "#E34045", "OT" = "#AD9EA0", "VL" = "#FF59EE", "cc" = "#C29FCB",
  "aco" = "#592E8B")

# ---- FIGURE 1: mouse urinary bladder (paper Fig 2C-G, S2) ------------------
build_fig1 = function() {
  d = load_data("spa_bladder.rda")

  # a) tissue segmentation (spatial)
  p1a_pix = d$pix[d$pix$region != "Other", ]
  p1a_oth = d$pix[d$pix$region == "Other", ]
  p1a = ggplot() +
    geom_point(data = p1a_oth, aes(x, y), colour = "#C9C9C9", size = 0.35, stroke = 0) +
    geom_point(data = p1a_pix, aes(x, y, colour = region), size = 0.45, stroke = 0) +
    scale_colour_manual(values = region_cols, name = "Tissue region",
                        breaks = c("Adventitia", "Muscle", "Urothelium")) +
    coord_equal() + spatial_theme() +
    labs(x = NULL, y = NULL)
  p1a = sp_label(p1a, "a", d$pix)

  # b) SM(34:1) + PC(34:1) spatial intensity
  p1b1 = mz_spatial(d$pix, "x", "y", "SM(34:1)", "b", fill_name = "SM(34:1)\n(log10)")
  p1b2 = mz_spatial(d$pix, "x", "y", "PC(34:1)", NULL, fill_name = "PC(34:1)\n(log10)")
  p1b  = p1b1 / p1b2 + plot_layout(heights = c(1, 1))
  # c) MZBoxPlot: key metabolite intensities by tissue region
  p1c = cliplot(intensity ~ region | region, data = d$mzbox,
                type = type_boxplot(add_jitter = TRUE, jitter_size = 0.1,
                                    jitter_alpha = 0.15, width = 0.6),
                theme = "nature",
                palette = region_cols[c("Adventitia", "Muscle", "Urothelium")],
                facet = ~ metabolite) +
    labs(x = NULL, y = "Intensity (log2)") +
    cli_panel_label("c") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 6),
          strip.text = element_text(size = 6.5, face = "bold"),
          legend.position = "none")

  # d) DEMs heatmap (top DE metabolites across the three regions)
  regs   = c("2", "5", "6")
  reglab = c(Adventitia = "Adventitia", Muscle = "Muscle", Urothelium = "Urothelium")
  top = do.call(rbind, lapply(regs, function(r) {
    dd = d$dem[d$dem$cluster == r, ]
    head(dd[order(-abs(dd$logFC)), ], 12)
  }))
  top = unique(top$feature)
  mat = reshape2::acast(d$dem[d$dem$feature %in% top, ], feature ~ cluster,
                        value.var = "logFC")
  mat = mat[, regs, drop = FALSE]
  mat = mat[top, , drop = FALSE]
  colnames(mat) = reglab[colnames(mat)]
  p1d = cliplot(mat,
                type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                    cluster_cols = FALSE, show_rownames = TRUE,
                                    show_colnames = TRUE, legend_title = "logFC",
                                    color_low = "#2166AC", color_mid = "#F7F7F7",
                                    color_high = "#B2182B"),
                theme = "nature")
  p1d = hm_label(p1d, "d")
  # e) volcano: muscle (5) vs urothelium (6)
  vol = d$vol
  lab = c("SM(34:1)", "PC(34:1)")
  lab = lab[lab %in% rownames(vol)]
  p1e = cliplot(-log10(adj.P.Val) ~ logFC, data = vol,
                type = type_volcano(pval_cutoff = 0.05, fc_cutoff = 1,
                                    label_genes = lab, point_size = 1.9,
                                    max_overlaps = 15, seed = 7),
                theme = "nature") +
    labs(x = "logFC (muscle vs urothelium)", y = "-log10 (adj. P)") +
    cli_panel_label("e") +
    theme(legend.position = "none", axis.title = element_text(size = 7))

  # f) circular "lipid shift" between muscle (5) and urothelium (6)
  cc = d$lipid_counts[d$lipid_counts$cluster %in% c("5", "6"), ]
  ccw = reshape2::dcast(cc, category ~ cluster, value.var = "count", fill = 0)
  ccw$diff = ccw[["5"]] - ccw[["6"]]
  ccw = ccw[ccw$diff != 0 & ccw$category != "Non-Lipids", ]
  ccw$cluster = ifelse(ccw$diff < 0, "Urothelium", "Muscle")
  ccw$count   = abs(ccw$diff)
  ccw$label   = gsub("GL; GP", "GL & GP", gsub("SP; GP", "SP & GP", ccw$category))
  ccw$label   = factor(ccw$label, levels = ccw$label[order(ccw$count)])
  ccw$id = seq_len(nrow(ccw))
  n_b = nrow(ccw)
  angle = 90 - 360 * (ccw$id - 0.5) / n_b
  ccw$hjust = ifelse(angle < -90, 1, 0)
  ccw$angle = ifelse(angle < -90, angle + 180, angle)
  maxv = max(ccw$count)
  p1f = ggplot(ccw, aes(id, count, fill = cluster)) +
    geom_col(width = 0.8, colour = "white", linewidth = 0.2) +
    geom_text(aes(y = count + maxv * 0.06, label = label,
                  angle = angle, hjust = hjust),
              size = 2.2, colour = "grey20") +
    scale_fill_manual(values = region_cols[c("Muscle", "Urothelium")], name = NULL) +
    coord_polar() + ylim(-maxv * 0.35, maxv * 1.15) +
    theme_void(base_size = 8) +
    theme(legend.position = "top", plot.margin = margin(12, 6, 6, 6)) +
    cli_panel_label("f")

  fig1 = (p1a | p1b | p1c) / (p1d | p1e | p1f) +
    plot_layout(widths = c(1, 1, 1), heights = c(1, 1.08))
  save_fig(fig1, "spamtp_fig1.png", 190, 208)
}
# ---- FIGURE 2: multi-omic mouse brain (paper Fig 2H-N, S3) -----------------
build_fig2 = function() {
  m  = load_data("spa_brain_msi.rda")
  v  = load_data("spa_brain_visium.rda")

  # a) MSI: dopamine (double derivatised, m/z 674.28)
  p2a = mz_spatial(m$pix, "x", "y", "dopamine2", "a",
                   fill_name = "Dopamine\n(log10)")

  # b) Visium: anatomical annotations
  p2b_sp = v$sp[!is.na(v$sp$annotations), ]
  p2b = ggplot(p2b_sp, aes(x, y, colour = annotations)) +
    geom_point(size = 1.05, stroke = 0) +
    scale_colour_manual(values = annot_cols, name = NULL) +
    coord_equal() + spatial_theme() +
    theme(legend.text = element_text(size = 5)) +
    labs(x = NULL, y = NULL)
  p2b = sp_label(p2b, "b", p2b_sp)

  # c) PCA of Visium transcriptomes (DimPlot analog)
  p2c = ggplot(v$sp, aes(PC1, PC2, colour = region)) +
    geom_point(size = 1.1, alpha = 0.85, stroke = 0) +
    scale_colour_manual(values = c(striatum = "#0072B2",
                                   not_striatum = "#BBBBBB"),
                        name = NULL) +
    cliomicplot:::theme_cli_nature(base_size = 9) +
    labs(x = "PC1", y = "PC2") +
    cli_panel_label("c") +
    theme(legend.position = "top")

  # d) striatum DEG volcano: lesioned vs intact
  deg = v$deg
  deg$gene_label = ifelse(duplicated(deg$gene) | duplicated(deg$gene, fromLast = TRUE),
                          paste0(deg$gene, "_2"), deg$gene)
  rownames(deg) = deg$gene_label
  sig_genes = deg$gene[order(deg$p_val_adj)][1:8]
  lab_d = deg$gene_label[match(sig_genes, deg$gene)]
  lab_d = lab_d[!is.na(lab_d)]
  p2d = cliplot(-log10(p_val_adj) ~ avg_log2FC, data = deg,
                type = type_volcano(pval_cutoff = 0.05, fc_cutoff = 0.5,
                                    label_genes = lab_d, point_size = 1.7,
                                    max_overlaps = 15, seed = 11),
                theme = "nature") +
    labs(x = "log2FC (lesioned vs intact)", y = "-log10 (adj. P)") +
    cli_panel_label("d") +
    theme(legend.position = "none", axis.title = element_text(size = 7))
  # e) top DEG heatmap (lesioned vs intact)
  mat = v$means[v$top_genes, c("Lesioned", "Intact"), drop = FALSE]
  mat = mat[order(-abs(mat[, 1] - mat[, 2])), , drop = FALSE]
  mat = mat[seq_len(min(18, nrow(mat))), , drop = FALSE]
  p2e = cliplot(mat,
                type = type_heatmap(scale = "row", cluster_rows = FALSE,
                                    cluster_cols = FALSE, show_rownames = TRUE,
                                    show_colnames = TRUE, legend_title = "row z",
                                    color_low = "#2166AC", color_mid = "#F7F7F7",
                                    color_high = "#B2182B"),
                theme = "nature")
  p2e = hm_label(p2e, "e")

  # f) MSI: dopamine (single derivatised, m/z 421.19)
  p2f = mz_spatial(m$pix, "x", "y", "dopamine1", "f",
                   fill_name = "Dopamine\n(log10)")

  fig2 = (p2a | p2b | p2c) / (p2d | p2e | p2f) +
    plot_layout(widths = c(1, 1, 1), heights = c(1, 1.08))
  save_fig(fig2, "spamtp_fig2.png", 190, 208)
}
# ---- FIGURE 3: human brain tumour pathway analysis (paper Fig S5) ----------
build_fig3 = function() {
  t = load_data("spa_tumour_pathways.rda")
  dt = t$dt
  dt = dt[!is.na(dt$padj) & dt$padj < 0.25, ]
  dt$short = vapply(strsplit(dt$pathwayName, "\\[|,"), `[`, "", 1)
  dt$short = gsub("\\s+", " ", trimws(dt$short))
  dt$short = ifelse(nchar(dt$short) > 52, paste0(substr(dt$short, 1, 49), "..."), dt$short)
  dt$short = make.unique(dt$short)
  dt$Cluster_id = factor(dt$Cluster_id,
                         levels = c("tumour", "leading edge"),
                         labels = c("Tumour", "Leading edge"))

  # a) NES heatmap: top pathways per cluster
  top = do.call(rbind, lapply(levels(dt$Cluster_id), function(cl) {
    dd = dt[dt$Cluster_id == cl, ]
    head(dd[order(-abs(dd$NES)), ], 12)
  }))
  top = unique(top$pathwayName)
  mat = reshape2::acast(dt[dt$pathwayName %in% top, ], pathwayName ~ Cluster_id,
                        value.var = "NES", fun.aggregate = mean)
  mat = mat[top, , drop = FALSE]
  rownames(mat) = vapply(strsplit(rownames(mat), "\\[|,"), `[`, "", 1)
  rownames(mat) = gsub("\\s+", " ", trimws(rownames(mat)))
  rownames(mat) = ifelse(nchar(rownames(mat)) > 42,
                         paste0(substr(rownames(mat), 1, 39), "..."), rownames(mat))
  p3a = cliplot(mat,
                type = type_heatmap(scale = "none", cluster_rows = FALSE,
                                    cluster_cols = FALSE, show_rownames = TRUE,
                                    show_colnames = TRUE, legend_title = "NES",
                                    color_low = "#2166AC", color_mid = "#F7F7F7",
                                    color_high = "#B2182B"),
                theme = "nature")
  p3a = hm_label(p3a, "a")
  # b) dot plot: NES vs significance (tumour cluster)
  dd = dt[dt$Cluster_id == "Tumour", ]
  dd = head(dd[order(-abs(dd$NES)), ], min(20, nrow(dd)))
  p3b = ggplot(dd, aes(NES, reorder(short, NES),
                       size = -log10(padj), colour = NES)) +
    geom_point(alpha = 0.85) +
    scale_colour_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                           midpoint = 0, name = "NES") +
    scale_size_continuous(range = c(1.2, 4.2), name = "-log10 (padj)") +
    cliomicplot:::theme_cli_nature(base_size = 9) +
    labs(x = "Normalised enrichment score (NES)", y = NULL) +
    cli_panel_label("b") +
    theme(legend.position = "right", legend.key.size = unit(3, "mm"),
          legend.title = element_text(size = 6.5, face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.y = element_text(size = 6))

  # c) top tumour pathways with leading-edge metabolite counts
  dd = dt[dt$Cluster_id == "Tumour", ]
  dd = head(dd[order(-dd$NES), ], min(12, nrow(dd)))
  dd$met_count = lengths(strsplit(dd$leadingEdge_metabolites, ";"))
  dd$short = factor(dd$short, levels = rev(dd$short))
  p3c = ggplot(dd, aes(NES, short, fill = NES > 0)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = met_count, hjust = ifelse(NES > 0, -0.25, 1.25)),
              size = 2.2, colour = "grey25") +
    scale_fill_manual(values = c("TRUE" = "#B2182B", "FALSE" = "#2166AC"),
                      name = NULL, labels = c("Down", "Up")) +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    cliomicplot:::theme_cli_nature(base_size = 9) +
    labs(x = "NES (tumour cluster)", y = NULL) +
    cli_panel_label("c") +
    theme(legend.position = "top", axis.text.y = element_text(size = 6))

  fig3 = (p3a | p3b) / p3c + plot_layout(heights = c(1.25, 1), widths = c(0.78, 1.22))
  save_fig(fig3, "spamtp_fig3.png", 190, 175)
}
# ---- FIGURE 4: spotted mouse liver / METASPACE annotation (paper Fig S6) ---
build_fig4 = function() {
  l = load_data("spa_liver.rda")
  liver_cols = c(liver = "#D9D9D9", Adenosine = "#E64B35", ADP = "#4DBBD5",
                 ATP = "#00A087", `Glutamic Acid` = "#F39B7F", Glutamine = "#8491B4",
                 Glutathione = "#F0B429", Guanine = "#7E6148")

  # a) spatial distribution of tissue + spotted chemical standards
  p4a = ggplot(l$pix, aes(x, y, colour = cluster)) +
    geom_point(size = 0.28, stroke = 0) +
    scale_colour_manual(values = liver_cols, name = NULL) +
    coord_equal() + spatial_theme() +
    theme(legend.text = element_text(size = 5)) +
    labs(x = NULL, y = NULL)
  p4a = sp_label(p4a, "a", l$pix)

  # b) spatial intensity of glutathione (spotted standard)
  p4b = mz_spatial(l$pix, "x", "y", "glutathione", "b",
                   fill_name = "Glutathione\n(log10)")

  # c) annotation mass error across the m/z range
  qc = l$qc
  p4c = ggplot(qc, aes(mz, ppm, colour = annotated)) +
    geom_point(size = 1.4, alpha = 0.85) +
    scale_colour_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#BBBBBB"),
                        name = "Annotated") +
    cliomicplot:::theme_cli_nature(base_size = 9) +
    labs(x = "m/z", y = "Mass error (ppm)") +
    cli_panel_label("c") +
    theme(legend.position = "top", legend.key.size = unit(3, "mm"))
  # d) top features by number of putative isomer annotations
  ann = qc[qc$annotated, ]
  ann = head(ann[order(-ann$n_isomers), ], min(15, nrow(ann)))
  ann$mz_lab = sprintf("m/z %.2f", ann$mz)
  ann$mz_lab = make.unique(ann$mz_lab)
  ann$mz_lab = factor(ann$mz_lab, levels = rev(ann$mz_lab))
  p4d = ggplot(ann, aes(n_isomers, mz_lab)) +
    geom_col(fill = "#0072B2", width = 0.7) +
    geom_text(aes(label = n_isomers), hjust = -0.25, size = 2.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    cliomicplot:::theme_cli_nature(base_size = 9) +
    labs(x = "Putative isomers (HMDB)", y = NULL) +
    cli_panel_label("d") +
    theme(axis.text.y = element_text(size = 6))

  fig4 = (p4a | p4b) / (p4c | p4d) + plot_layout(widths = c(1, 1), heights = c(1.05, 1))
  save_fig(fig4, "spamtp_fig4.png", 190, 180)
}

# ===========================================================================
# RUN
# ===========================================================================
extract_all()
build_fig1()
build_fig2()
build_fig3()
build_fig4()
for (i in 1:4) save_preview(sprintf("spamtp_fig%d.png", i),
                            sprintf("spamtp_fig%d_preview.jpg", i))
cat("All 4 SpaMTP paper figures regenerated in", fig_dir, "\n")