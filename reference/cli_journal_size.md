# Get Exact Figure Dimensions for Specific Scientific Journals

Returns the exact column widths in millimeters and inches for 9 major
scientific publishers, derived from verified publisher presets. Useful
together with
[`cli_save`](https://vanhungtran.github.io/cliomicplot/reference/cli_save.md).

## Usage

``` r
cli_journal_size(
  journal = c("nature", "natcomms", "science", "cell", "jama", "nejm", "lancet", "plos",
    "ieee"),
  column = c("single", "one_half", "double", "full")
)
```

## Arguments

- journal:

  One of `"nature"`, `"natcomms"`, `"science"`, `"cell"`, `"jama"`,
  `"nejm"`, `"lancet"`, `"plos"`, `"ieee"`.

- column:

  One of `"single"`, `"one_half"`, `"double"`, `"full"`.

## Value

A list with elements `width_mm`, `width_in`, `height_in` (default 3:4
aspect), `max_height_mm`, `journal`, and `column`.

## Examples

``` r
cli_journal_size("nature", "single")
#> $journal
#> [1] "nature"
#> 
#> $column
#> [1] "single"
#> 
#> $width_mm
#> [1] 89
#> 
#> $height_mm
#> [1] 71.2
#> 
#> $width_in
#> [1] 3.503937
#> 
#> $height_in
#> [1] 2.80315
#> 
#> $max_height_mm
#> [1] 170
#> 
cli_journal_size("jama", "double")
#> $journal
#> [1] "jama"
#> 
#> $column
#> [1] "double"
#> 
#> $width_mm
#> [1] 177.8
#> 
#> $height_mm
#> [1] 142.24
#> 
#> $width_in
#> [1] 7
#> 
#> $height_in
#> [1] 5.6
#> 
#> $max_height_mm
#> [1] 220
#> 
```
