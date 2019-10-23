
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marcr

<!-- badges: start -->

<!-- badges: end -->

The goal of marcr is to collect various functions that may be useful in
other projects. It’s a personal “misc” package.

## Installation

You can install the released version of marcr from
[Github](https://github.com/marcmtk/) with:

``` r
remotes::install_github("marcmtk/marcr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(marcr)
read_cobas_rst("data-raw/invalid.rst")
#> # A tibble: 1 x 27
#>   AnalysisDateTime    `Approved By` Assay `Ctrl Exp` Details `Device S/N`
#>   <dttm>              <chr>         <chr> <chr>      <chr>   <chr>       
#> 1 2019-01-01 23:44:00 <NA>          Liat~ <NA>       ""      M1-E-17163  
#> # ... with 21 more variables: `Failure Code` <chr>, MacAddress <chr>,
#> #   Operator <chr>, OrderingPhysician <chr>, Patient <chr>, `Report
#> #   Results` <chr>, `Run No.` <chr>, `Run Status` <chr>, `Sample
#> #   ID` <chr>, SampleType <chr>, `SW Ver` <chr>, `Time/Date` <chr>, `Tube
#> #   Exp` <date>, `Tube Lot` <chr>, `Tube S/N` <chr>, Use <chr>,
#> #   infl_a <lgl>, infl_b <lgl>, rsv <lgl>, invalid <lgl>, warning <lgl>
read_biotyper_out("data-raw/biotyper_1.astm")
#> # A tibble: 350 x 11
#>    organism organism_id score spot_number guess_number clinical_name sample
#>    <chr>    <chr>       <dbl> <chr>       <chr>        <chr>         <chr> 
#>  1 Meyeroz~ 4929         1.45 1           1            Candida guil~ 10041~
#>  2 Lactoba~ 1603         1.39 1           2            Lactobacillu~ 10041~
#>  3 Prevote~ 28127        1.38 1           3            Prevotella b~ 10041~
#>  4 Pseudom~ 75612        1.36 1           4            Pseudomonas ~ 10041~
#>  5 Lactoba~ 33962        1.30 1           5            Lactobacillu~ 10041~
#>  6 Strepto~ 1923         1.30 1           6            Streptomyces~ 10041~
#>  7 Candida~ 49331        1.26 1           7            Candida para~ 10041~
#>  8 Burkhol~ 292          1.25 1           8            Burkholderia~ 10041~
#>  9 Lactoba~ 259059       1.25 1           9            Lactobacillu~ 10041~
#> 10 Lactoba~ 47770        1.23 1           10           Lactobacillu~ 10041~
#> # ... with 340 more rows, and 4 more variables: isolate <int>,
#> #   spot_location <chr>, time <dttm>, instrument_id <chr>
random_timestamp(1)
#> [1] "2016-05-01T16:19:42.543"
```
