# Enable Markdown Rendering in Plot Text Elements

Selectively enables markdown rendering for specific ggplot2 text
elements using \`ggtext::element_markdown()\`. Unlike a blanket theme
converter, this lets you target exactly which labels should interpret
markdown (e.g., only the title and subtitle, leaving axis labels as
plain text).

## Usage

``` r
cli_markdown(..., base_family = "", base_size = NULL)
```

## Arguments

- ...:

  Character strings naming which text elements to enable markdown for.
  Supported names: \`"title"\`, \`"subtitle"\`, \`"caption"\`,
  \`"xlab"\`, \`"ylab"\`, \`"xtext"\`, \`"ytext"\`, \`"axis_title"\`,
  \`"axis_text"\`, \`"legend_title"\`, \`"legend_text"\`, \`"strip"\`,
  \`"all"\`. If empty, enables markdown for \`"title"\`, \`"subtitle"\`,
  and \`"caption"\` (the most common use case). Use \`"all"\` to enable
  everywhere.

- base_family:

  Base font family for markdown text. Default \`""\` uses the current
  theme default.

- base_size:

  Base font size. \`NULL\` preserves the current theme's size.

## Value

A ggplot2 theme object that can be added to any plot.

## Details

Supported markdown syntax in labels:

- \`\*\*bold\*\*\` — bold text

- \`\*italics\*\` — italic text

- \`\*\*\*bold italic\*\*\*\` — combined

- \`\<span style='color:red'\>text\</span\>\` — colored spans

- \`\<br\>\` — line break

- \`\<sub\>subscript\</sub\>\` / \`\<sup\>superscript\</sup\>\`

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

p = ggplot(mtcars, aes(hp, mpg)) +
  geom_point() +
  labs(
    title = "**MPG** vs *Horsepower*",
    subtitle = "n = 32 vehicles",
    caption = "Source: *Motor Trend* (1974)"
  )

# Enable markdown only on title + subtitle + caption
p + cli_markdown()

# Enable on everything
p + cli_markdown("all")

# Only on title
p + cli_markdown("title")
} # }
```
