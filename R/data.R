# ===========================================================================
# cliomicplot: Package Datasets
# Clinical & Multi-Omics Research Cohorts
# ===========================================================================

#' Cis-pQTL Mendelian Randomization and Colocalization Results
#'
#' @description
#' Summary statistics from a two-sample cis-pQTL Mendelian Randomization (MR)
#' and Bayesian colocalization (\code{coloc.abf}) study linking UK Biobank
#' Pharma Proteomics Project (UKB-PPP) plasma protein levels to 17 FinnGen R12
#' clinical disease endpoints across cardiovascular, allergic skin, and
#' autoimmune/metabolic categories (Terekhova et al., Immunity 2026).
#'
#' @format A data frame with 323 rows and 15 variables:
#' \describe{
#'   \item{symbol}{Gene/protein symbol (e.g. \code{"IL7R"}, \code{"CD27"}, \code{"GZMB"}, \code{"LTB"}).}
#'   \item{set}{Biological functional class of exposure protein (\code{"GZMB_effector"}, \code{"early_memory"}, or \code{"lineage"}).}
#'   \item{phenocode}{FinnGen R12 endpoint identifier code.}
#'   \item{label}{Full descriptive disease endpoint label.}
#'   \item{label_short}{Shortened disease endpoint label for publication figures.}
#'   \item{family}{Disease family category (\code{"cardiovascular"}, \code{"skin_atopy"}, or \code{"immune_metabolic"}).}
#'   \item{family_label}{Formatted factor for disease family facets.}
#'   \item{set_label}{Formatted factor for exposure protein class facets.}
#'   \item{b_mr}{Causal effect estimate (Wald ratio \eqn{\beta}) per SD genetically higher plasma protein.}
#'   \item{se_mr}{Second-order delta-method standard error of the Wald ratio.}
#'   \item{z}{\eqn{Z}-statistic (\eqn{b_{\text{MR}} / \text{SE}_{\text{MR}}}).}
#'   \item{p_mr}{Association p-value of the Wald ratio.}
#'   \item{or_per_sd}{Odds ratio per SD genetically higher plasma protein.}
#'   \item{PP_H3}{Posterior probability of colocalization hypothesis 3 (two distinct causal variants in locus / LD artifact).}
#'   \item{PP_H4}{Posterior probability of colocalization hypothesis 4 (one shared causal variant).}
#'   \item{q_mr}{FDR-adjusted q-value across the 323 tested pairs.}
#'   \item{lead_is_top_cis}{Logical. Whether the instrument variant is the primary cis-pQTL lead in UKB-PPP.}
#' }
#'
#' @source
#' UK Biobank Pharma Proteomics Project (UKB-PPP) discovery plasma pQTL summary
#' statistics and FinnGen Release 12 GWAS summary statistics.
#'
#' @references
#' Terekhova et al. (2026). Pan-atlas of immune aging reveals single-cell
#' trajectories and plasma biomarker signatures. \emph{Immunity}.
#'
#' @examples
#' data(pqtl_mr_coloc)
#' head(pqtl_mr_coloc)
"pqtl_mr_coloc"

#' Single-Cell Immune Lineage Composition Across Blood Libraries
#'
#' @description
#' Relative cellular lineage fractions (CD4+ T, CD8+ T, NK, \eqn{\gamma\delta} T,
#' B, myeloid, other) derived from single-cell RNA-seq libraries of peripheral
#' blood mononuclear cells (PBMC) across diverse inflammatory and allergic
#' diseases (atopic dermatitis, atherosclerosis with/without type 2 diabetes,
#' mild/severe asthma, and healthy controls) from NCBI GEO series.
#'
#' @format A data frame with 630 rows and 5 variables:
#' \describe{
#'   \item{gsm}{NCBI GEO sample accession identifier.}
#'   \item{dataset}{NCBI GEO series accession (e.g. \code{"GSE183953"}, \code{"GSE272294"}, \code{"GSE288147"}).}
#'   \item{group}{Clinical phenotype, disease subgroup, or treatment arm.}
#'   \item{lineage}{Cellular lineage factor (\code{"CD4+ T"}, \code{"CD8+ T"}, \code{"NK"}, \code{"gd T"}, \code{"B"}, \code{"Myeloid"}, \code{"Other"}).}
#'   \item{share}{Fraction of total library leukocytes (0.0 to 1.0).}
#' }
#'
#' @source
#' NCBI Gene Expression Omnibus (GEO) single-cell series GSE183953, GSE272294,
#' GSE288147, GSE256088, GSE268436, GSE295457, GSE193096, GSE224144.
#'
#' @examples
#' data(immune_cell_composition)
#' head(immune_cell_composition)
"immune_cell_composition"
