
<!-- README.md is generated from README.Rmd. Please edit that file -->

# templateRpackage

<!-- badges: start -->

[![R-CMD-check](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EcologyR/templateRpackage/branch/master/graph/badge.svg)](https://app.codecov.io/gh/EcologyR/templateRpackage?branch=master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/ecologyr/templaterpackage/badge)](https://www.codefactor.io/repository/github/ecologyr/templaterpackage) -->
<!-- badges: end -->

The goal of templateRpackage is to …

## Installation

``` r
# install.packages("devtools")
devtools::install_github("EcologyR/templateRpackage")
```

The code to create this package is available
[here](https://gist.github.com/Pakillo/999e34301c56011138ef164363502465).

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(templateRpackage)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

``` r
plot(pressure)
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Citation

If using this package, please cite it:

``` r
citation("templateRpackage")

To cite templateRpackage in publications use:

  Rodriguez-Sanchez F. 2023. templateRpackage.
  https://ecologyr.github.io/templateRpackage/

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {templateRpackage},
    author = {Francisco Rodriguez-Sanchez},
    year = {2023},
    url = {https://ecologyr.github.io/templateRpackage/},
  }
```

## Funding

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388 led by Francisco Rodríguez Sánchez, Universidad de Sevilla).

![](https://ecologyr.github.io/workshop/images/logos.png)
