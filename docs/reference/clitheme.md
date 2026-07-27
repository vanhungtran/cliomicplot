# Set or Reset Plot Themes for cliomicplot

Sets or resets the theme for cliomicplot plots. Themes control the
overall appearance including fonts, grid lines, axis styles, and colors.
Inspired by tinyplot's \`tinytheme()\`, themes can be persistent
(applied globally) or ephemeral (for a single plot call).

## Usage

``` r
clitheme(theme = NULL, ..., register = NULL)
```

## Arguments

- theme:

  Character string specifying the theme name. Available themes:

  - `"cli_bw"` - Clean black-and-white theme (default)

  - `"cli_classic"` - Classic theme with axis lines, no panel border

  - `"cli_minimal"` - Minimal theme, clean and modern

  - `"nature"` - Nature Publishing Group style

  - `"science"` - Science/AAAS journal style

  - `"nejm"` - NEJM (New England Journal of Medicine) style

  - `"lancet"` - The Lancet style

  - `"cell"` - Cell Press style

  - `"broadsheet"` - Publication print style with subtle grid

  - `"showcase"` - Polished presentation style with stronger hierarchy

  - `"dark"` - Dark background theme

  Use `clitheme()` without arguments to reset to the default theme.

- ...:

  Named arguments to override specific theme settings (passed to
  ggplot2's
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)).

- register:

  Optional character string to register this theme under a custom name
  via
  [`clitheme_register`](https://vanhungtran.github.io/cliomicplot/reference/clitheme_register.md).

## Details

\*\*Persistent vs. ephemeral themes.\*\* Calling `clitheme("nature")`
triggers a persistent theme applied to all subsequent ggplot2 plots.
Reset by calling `clitheme()` without arguments. For ephemeral
(single-plot) themes, pass `theme = "nature"` directly to
[`cliplot`](https://vanhungtran.github.io/cliomicplot/reference/cliplot.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Set a persistent theme
clitheme("nature")
cliplot(mpg ~ wt, data = mtcars)

# Reset to default
clitheme()

# Ephemeral theme for one plot
cliplot(mpg ~ wt, data = mtcars, theme = "dark")
} # }
```
