# Cis-pQTL Mendelian Randomization and Colocalization Results

Summary statistics from a two-sample cis-pQTL Mendelian Randomization
(MR) and Bayesian colocalization (`coloc.abf`) study linking UK Biobank
Pharma Proteomics Project (UKB-PPP) plasma protein levels to 17 FinnGen
R12 clinical disease endpoints across cardiovascular, allergic skin, and
autoimmune/metabolic categories (Terekhova et al., Immunity 2026).

## Usage

``` r
pqtl_mr_coloc
```

## Format

A data frame with 323 rows and 15 variables:

- symbol:

  Gene/protein symbol (e.g. `"IL7R"`, `"CD27"`, `"GZMB"`, `"LTB"`).

- set:

  Biological functional class of exposure protein (`"GZMB_effector"`,
  `"early_memory"`, or `"lineage"`).

- phenocode:

  FinnGen R12 endpoint identifier code.

- label:

  Full descriptive disease endpoint label.

- label_short:

  Shortened disease endpoint label for publication figures.

- family:

  Disease family category (`"cardiovascular"`, `"skin_atopy"`, or
  `"immune_metabolic"`).

- family_label:

  Formatted factor for disease family facets.

- set_label:

  Formatted factor for exposure protein class facets.

- b_mr:

  Causal effect estimate (Wald ratio \\\beta\\) per SD genetically
  higher plasma protein.

- se_mr:

  Second-order delta-method standard error of the Wald ratio.

- z:

  \\Z\\-statistic (\\b\_{\text{MR}} / \text{SE}\_{\text{MR}}\\).

- p_mr:

  Association p-value of the Wald ratio.

- or_per_sd:

  Odds ratio per SD genetically higher plasma protein.

- PP_H3:

  Posterior probability of colocalization hypothesis 3 (two distinct
  causal variants in locus / LD artifact).

- PP_H4:

  Posterior probability of colocalization hypothesis 4 (one shared
  causal variant).

- q_mr:

  FDR-adjusted q-value across the 323 tested pairs.

- lead_is_top_cis:

  Logical. Whether the instrument variant is the primary cis-pQTL lead
  in UKB-PPP.

## Source

UK Biobank Pharma Proteomics Project (UKB-PPP) discovery plasma pQTL
summary statistics and FinnGen Release 12 GWAS summary statistics.

## References

Terekhova et al. (2026). Pan-atlas of immune aging reveals single-cell
trajectories and plasma biomarker signatures. *Immunity*.

## Examples

``` r
data(pqtl_mr_coloc)
head(pqtl_mr_coloc)
#>   symbol          set         phenocode                            label
#> 1    LTB early_memory        M13_RHEUMA             Rheumatoid arthritis
#> 2    LTB early_memory               T1D                  Type 1 diabetes
#> 3    LTB early_memory             G6_MS               Multiple sclerosis
#> 4   IL7R early_memory        L12_ATOPIC                Atopic dermatitis
#> 5    LTB early_memory J10_ASTHMA_EXMORE Asthma (more control exclusions)
#> 6   IL7R early_memory J10_ASTHMA_EXMORE Asthma (more control exclusions)
#>             family        b_mr       se_mr         p_mr or_per_sd      PP_H3
#> 1 immune_metabolic -0.94981860 0.084019614 1.243436e-29 0.3868112 1.00000000
#> 2 immune_metabolic  1.62710011 0.153683809 3.412108e-26 5.0890955 1.00000000
#> 3 immune_metabolic  1.42235212 0.177743149 1.221269e-15 4.1468629 1.00000000
#> 4       skin_atopy  0.07271676 0.010184223 9.323733e-13 1.0754259 0.01809004
#> 5       skin_atopy  0.29713471 0.042920587 4.425060e-12 1.3459966 1.00000000
#> 6       skin_atopy  0.04402621 0.007854229 2.077572e-08 1.0450097 0.37239552
#>          PP_H4         q_mr lead_is_top_cis          label_short          z
#> 1 1.040931e-90 4.016297e-27            TRUE Rheumatoid arthritis -11.304725
#> 2 1.238782e-90 5.510554e-24            TRUE      Type 1 diabetes  10.587323
#> 3 1.793156e-31 1.314900e-13            TRUE   Multiple sclerosis   8.002289
#> 4 9.819100e-01 7.528914e-11            TRUE    Atopic dermatitis   7.140138
#> 5 2.145330e-14 2.858589e-10            TRUE               Asthma   6.922895
#> 6 6.275803e-01 1.118426e-06            TRUE               Asthma   5.605414
#>                 family_label            set_label
#> 1 Other immune and metabolic Early-memory markers
#> 2 Other immune and metabolic Early-memory markers
#> 3 Other immune and metabolic Early-memory markers
#> 4             Skin and atopy Early-memory markers
#> 5             Skin and atopy Early-memory markers
#> 6             Skin and atopy Early-memory markers
```
