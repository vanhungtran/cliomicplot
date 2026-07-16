# Set or Query Cliomicplot Graphical Parameters

Sets or queries graphical parameters for cliomicplot, extending
ggplot2's theme system with cliomicplot-specific options. Similar to
tinyplot's \`tpar()\`, parameters are set by passing \`key = value\`
pairs.

## Usage

``` r
clipar(...)
```

## Arguments

- ...:

  Arguments of the form \`key = value\`, or character strings to query
  specific parameter values. If empty, returns all current parameters.

## Value

When setting, invisibly returns the previous values. When querying,
returns the current value(s).

## Examples

``` r
# Query all parameters
clipar()
#> $facet.font
#> [1] 1
#> 
#> $font.cap
#> [1] 1
#> 
#> $stat.test
#> NULL
#> 
#> $facet.border
#> [1] NA
#> 
#> $facet.cex
#> [1] 1
#> 
#> $palette.diverging
#> [1] "heatmap_rdbu"
#> 
#> $file.width
#> [1] 8
#> 
#> $theme.default
#> [1] "cli_bw"
#> 
#> $legend.direction
#> [1] "vertical"
#> 
#> $cex.sub
#> [1] 0.9
#> 
#> $cex.lab
#> [1] 1
#> 
#> $grid.lwd
#> [1] 0.3
#> 
#> $file.res
#> [1] 300
#> 
#> $col.default
#> [1] "#2B6CB0"
#> 
#> $facet.bg
#> [1] "grey95"
#> 
#> $grid.lty
#> [1] "dotted"
#> 
#> $cex.cap
#> [1] 0.8
#> 
#> $facet.col
#> [1] "black"
#> 
#> $font.main
#> [1] 2
#> 
#> $legend.position
#> [1] "right"
#> 
#> $palette.qualitative
#> [1] "npg"
#> 
#> $cex.main
#> [1] 1.1
#> 
#> $grid
#> [1] FALSE
#> 
#> $font.sub
#> [1] 1
#> 
#> $grid.col
#> [1] "grey90"
#> 
#> $stat.pcutoff
#> [1] 0.05
#> 
#> $stat.label
#> [1] "p.format"
#> 
#> $fmar
#> [1] 1 1 1 1
#> 
#> $ribbon.alpha
#> [1] 0.2
#> 
#> $file.height
#> [1] 6
#> 
#> $palette.sequential
#> [1] "blues"
#> 
#> $cex.axis
#> [1] 0.9
#> 
#> $lmar
#> [1] 1.0 0.1
#> 

# Set palette
clipar(palette.qualitative = "nejm")

# Query a specific parameter
clipar("palette.qualitative")
#> [1] "nejm"
```
