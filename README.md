
# mixtur: An R package for designing, analysing, and modelling continuous report visual short-term memory studies

**mixtur** is a package to assist researchers in designing, analysing,
and modelling data from continuous report visual short-term memory
studies.

![](images/overview.png)

In these studies, participants are presented with an encoding screen
with items to remember (in this example, the participants need to recall
the colours of the squares). After a retention interval, one of the
squares is probed (highlighted here by the darkened square), and the
participants’ task is to recall the colour of the probed item. The
participant makes their response on a colour wheel, and the dependent
variable is the angular deviation between the participant’s response (R)
and the true target value (T) in radians. After many trials, a
probability density distribution can be formed of the response errors
across participants.

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

## Example Data

**mixtur** includes several example data sets to allow the user to
familiarise themselves with the package:

  - **bays2009\_sample**: A sample from Bays et al. (2009).
  - **example\_data**: test.

## References
