# Single-Cell Immune Lineage Composition Across Blood Libraries

Relative cellular lineage fractions (CD4+ T, CD8+ T, NK,
\\\gamma\delta\\ T, B, myeloid, other) derived from single-cell RNA-seq
libraries of peripheral blood mononuclear cells (PBMC) across diverse
inflammatory and allergic diseases (atopic dermatitis, atherosclerosis
with/without type 2 diabetes, mild/severe asthma, and healthy controls)
from NCBI GEO series.

## Usage

``` r
immune_cell_composition
```

## Format

A data frame with 630 rows and 5 variables:

- gsm:

  NCBI GEO sample accession identifier.

- dataset:

  NCBI GEO series accession (e.g. `"GSE183953"`, `"GSE272294"`,
  `"GSE288147"`).

- group:

  Clinical phenotype, disease subgroup, or treatment arm.

- lineage:

  Cellular lineage factor (`"CD4+ T"`, `"CD8+ T"`, `"NK"`, `"gd T"`,
  `"B"`, `"Myeloid"`, `"Other"`).

- share:

  Fraction of total library leukocytes (0.0 to 1.0).

## Source

NCBI Gene Expression Omnibus (GEO) single-cell series GSE183953,
GSE272294, GSE288147, GSE256088, GSE268436, GSE295457, GSE193096,
GSE224144.

## Examples

``` r
data(immune_cell_composition)
head(immune_cell_composition)
#>                     gsm   dataset             group lineage    share
#> share_CD4+.1 GSM5574840 GSE183953 atopic_dermatitis  CD4+ T 0.420486
#> share_CD4+.2 GSM5574841 GSE183953 atopic_dermatitis  CD4+ T 0.361311
#> share_CD4+.3 GSM5574842 GSE183953 atopic_dermatitis  CD4+ T 0.672016
#> share_CD4+.4 GSM5574843 GSE183953 atopic_dermatitis  CD4+ T 0.559277
#> share_CD4+.5 GSM5574844 GSE183953 atopic_dermatitis  CD4+ T 0.184995
#> share_CD4+.6 GSM5574845 GSE183953 atopic_dermatitis  CD4+ T 0.192308
```
