
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![](http://www.r-pkg.org/badges/version/mixtur)](https://CRAN.R-project.org/package=mixtur)
[![](http://cranlogs.r-pkg.org/badges/grand-total/mixtur)](https://CRAN.R-project.org/package=mixtur)

# mixtur <a ><img src='images/logo/mixtur_logo.png' align="right" height="250" /></a>

**mixtur** is an R package for designing, analysing, and modelling
continuous report visual short-term memory studies. The package allows
users to implement the 2-component (Zhang & Luck, 2008) and 3-component
(Bays, Catalao, & Husain, 2009) mixture models of continuous-report
visual short-term memory data. The package can also fit & simulate the
slots and slots-plus averaging models of Zhang & Luck.

The package allows users to:

- Obtain summary statistics of response error and response precision of
  behavioural data
- Produce publication-ready plots of behavioural data
- Fit both the 2- and 3-component models to user data
- Plot the goodness of model fit to user data
- Simulate artificial data from both models
- Conduct formal model competition analysis

## Installation

You can install the released version of mixtur (v1.2.0) from
[CRAN](https://CRAN.R-project.org/package=mixtur) with:

``` r
install.packages("mixtur")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JimGrange/mixtur")
```

## Publication

We have an academic publication showing users how to use the package.
Here we provide the link to the final publication, as well as a link to
the pre-print of the paper:

- Grange, J.A. & Moore, S.B. (2022). mixtur: An R package for designing,
  analysing, and modelling continuous report visual short-term memory
  studies. *Behavior Research Methods, 54*, 2071–2100.
  - [Final
    Publication](https://link.springer.com/article/10.3758/s13428-021-01688-1)  
  - [Preprint](https://psyarxiv.com/n6gqx/)

The paper also includes several simulation studies exploring some
properties of the models (including parameter recovery simulations,
model recovery simulations) and provides concrete recommendations to
researchers wishing to use mixture modelling in their own research.

## Acknowledgements & References

- We are grateful to Ed. D.J. Berry who contributed to the package
  development.

- Portions of the package code have been adapted from code written by
  Paul Bays in Matlab, with permission. We are extremely grateful to
  Paul Bays for this permission. See <https://paulbays.com>.

- Bays, P. M., Catalao, R. F. G., & Husain, M. (2009). The precision of
  visual working memory is set by allocation of a shared resource.
  *Journal of Vision, 9*(10): 7, 1–11.

- Zhang, W., & Luck, S. J. (2008). Discrete fixed-resolution
  representations in visual working memory. *Nature, 453,* 233–235.
