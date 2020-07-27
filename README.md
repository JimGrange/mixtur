
# mixtur: An R package for designing, analysing, and modelling continuous report visual short-term memory studies

**mixtur** is a package to assist researchers in designing, analysing,
and modelling data from continuous report visual short-term memory
studies.

Overview of README:

  - [How to install the package](#installation)
  - [An overview of continuous report tasks](#continuous-report-tasks)
  - [Example data shipped with mixtur](#example-data)
  - [Plotting response error](#plotting-response-error)
  - [Plotting response precision](#plotting-precision-of-responses)

## Installation

To install the package from GitHub, you need the devtools package:

``` r
install.packages("devtools")
library(devtools)
```

Then **mixtur** can be directly installed:

``` r
devtools::install_github("JimGrange/mixtur")
```

## Continuous Report Tasks

The package is designed to assist researchers using continuous report
measures of visual short term memory. In continuous report tasks,
participants are presented with an encoding screen with items to
remember (in the example below, the participants need to recall the
colours of the squares). After a retention interval, one of the items is
probed (highlighted here by the darkened square), and the participants’
task is to recall the feature value (e.g., colour) of the probed item.

![](images/overview.png)

The participant makes their response on a colour wheel, and the
dependent variable is the angular deviation between the participant’s
response (R) and the true target value (T). After many trials, a
probability density distribution can be formed of the response errors
across participants.

## Example Data

**mixtur** includes several example data sets to allow the user to
familiarise themselves with the package:

  - **bays2009\_full**: The full data set from Bays et al. (2009)
    including data from 12 participants. The data includes set size
    manipulations (1, 2, 4, and 6 items) plus a manipulation of *delay*
    of the retention interval (100ms, 500ms, and 2,000ms). The response,
    target, and non-target values are in radians (-pi to pi).
  - **bays2009\_sample**: A sample of data taken from the full data from
    Bays et al. (2009). This data just consists of set sizes of 4 and
    delay of 500ms. This sample data is provided if the user wishes to
    interact with a simpler data structure. The response, target, and
    non-target values are in radians (-pi to pi).

Data can be loaded in the following way:

``` r
library(mixtur)
data(bays2009_sample)
head(bays2009_sample)
#>   id response target non_target_1 non_target_2 non_target_3
#> 1  1   -2.186 -0.002       -2.989        2.648        2.262
#> 2  1   -1.980 -2.498       -1.861       -1.340       -0.309
#> 3  1   -0.177 -2.088       -2.845       -3.102       -0.371
#> 4  1    1.342  1.334        2.844        1.007       -0.599
#> 5  1   -1.644 -2.224        3.129        2.936        1.295
#> 6  1    1.219  1.253        2.886       -0.924       -1.035
```

## Plotting Response Error

``` r
library(mixtur)

# example data
data(example_data)

# plot overall error
plot_error(data = example_data, 
           unit = "degrees", 
           condition_var = "NULL", 
           set_size_var = "NULL")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

``` r
# plot error by condition (condition b is easier than condition a)
plot_error(data = example_data, 
           unit = "degrees", 
           condition_var = "condition", 
           set_size = "NULL")
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
# sample of bays (2009) data
data("bays2009_sample")
plot_error(data = bays2009_sample, 
           unit = "radians", 
           condition_var = "NULL", 
           set_size_var = "NULL")
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

``` r
# bays (2009) full data set
data(bays2009_full)

plot_error(data = bays2009_full,
           unit = "radians",
           set_size_var = "set_size", 
           condition_var = "NULL")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

``` r
# by set size & condition

plot_error(data = bays2009_full,
           unit = "radians",
           set_size_var = "set_size", 
           condition_var = "delay")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Plotting Precision of Reponses

``` r
data <- bays2009_full
# plot just mean
plot_precision(data, unit = "radians",
               condition_var = "NULL",
               set_size = "NULL")
#> [1] "NULL"
```

``` r
# plot just set size
plot_precision(data, unit = "radians",
               condition_var = "NULL",
               set_size_var = "set_size")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

``` r
# plot just condition (delay)
plot_precision(data, unit = "radians",
               condition_var = "delay",
               set_size_var = "NULL")
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

``` r
# plot set size AND condition
plot_precision(data, unit = "radians",
               condition_var = "delay",
               set_size_var = "set_size")
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

``` r
data <- example_data
plot_precision(data, unit = "degrees", 
               condition_var = "condition")
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

## References
