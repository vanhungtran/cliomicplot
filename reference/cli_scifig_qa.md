# SciFig Quality Assurance and Journal Compliance Audit

Audits a ggplot2 object, grob, or patchwork composite against official
scientific publisher presets (Nature, Science, Cell, NEJM, Lancet, etc.)
using the AI4SciFig quality gates. Evaluates:

- Physical dimensions (width and height vs journal column limits)

- Font sizes (enforcing publisher minimum font floors, e.g. 5.0 pt for
  Nature)

- Stroke widths (enforcing hairline floors, e.g. \>= 0.25 pt)

- Zero sign suppression (verifying no "-0.00" or "-.00" negative zeros)

- Color accessibility and contrast against background

Produces exact numeric metrics and booleans that can be recomputed by
hand.

## Usage

``` r
cli_scifig_qa(
  plot = ggplot2::last_plot(),
  journal = c("nature", "natcomms", "science", "cell", "jama", "nejm", "lancet", "plos",
    "ieee"),
  column = c("double", "single", "one_half"),
  width_mm = NULL,
  height_mm = NULL
)
```

## Arguments

- plot:

  A ggplot object, grob, or patchwork object.

- journal:

  Target journal name: `"nature"` (default), `"natcomms"`, `"science"`,
  `"cell"`, `"jama"`, `"nejm"`, `"lancet"`, `"plos"`, or `"ieee"`.

- column:

  Column class: `"double"` (default), `"single"`, or `"one_half"`.

- width_mm:

  Measured or intended physical width in millimeters. If NULL, derived
  from preset.

- height_mm:

  Measured or intended physical height in millimeters. If NULL, derived
  from preset.

## Value

An object of class `c("cli_scifig_qa", "data.frame")` containing the
audit results.
