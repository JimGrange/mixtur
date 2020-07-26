
# mixtur: An R package for designing, analysing, and modelling continuous report visual short-term memory studies

**mixtur** is a package to assist researchers in designing, analysing,
and modelling data from continuous report visual short-term memory
studies.

## Installation

To install the package from GitHub, you need the devools package:

``` r
install.packages("devtools")
library(devtools)
```

Then **mixtur** can be directly installed:

``` r
devtools::install_github("JimGrange/mixtur")
```

## Continuous Report Tasks

![](images/overview.png)

In continuous report tasks, participants are presented with an encoding
screen with items to remember (in this example, the participants need to
recall the colours of the squares). After a retention interval, one of
the squares is probed (highlighted here by the darkened square), and the
participants’ task is to recall the colour of the probed item.

The participant makes their response on a colour wheel, and the
dependent variable is the angular deviation between the participant’s
response (R) and the true target value (T) in radians.

After many trials, a probability density distribution can be formed of
the response errors across participants.

## Example Data

**mixtur** includes several example data sets to allow the user to
familiarise themselves with the package:

  - **bays2009\_full**: The full data set from Bays et al. (2009)
    including set size manipulations (1, 2, 4, and 6 items) plus a
    manipulation of *delay* of the retention interval (100ms, 500ms, and
    2,000ms).
  - **bays2009\_sample**: A sample from Bays et al. (2009).

Data can be loaded in the following way:

``` r
data(bays2009_full)
head(bays2009_full)
```

## References
