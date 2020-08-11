
# mixtur: An R package for designing, analysing, and modelling continuous report visual short-term memory studies

**mixtur** is a package to assist researchers in designing, analysing,
and modelling data from continuous report visual short-term memory
studies.

## ToDo List

  - Plotting
      - ~~plot response error~~
      - ~~plot precision of responses~~
      - ~~need to test degress\_180 data works~~
  - Analysing
      - return response error by id for inferential analysis (is this
        even worthwhile??)
      - ~~return precision by id for inferential analysis~~
  - Modelling
      - ~~fit the 3-component model to data~~
      - ~~fit the 2-component model to data~~
      - plot model parameters
      - ~~plot model fit against participant error data~~
      - ~~need to test degress\_180 data works~~
      - formal model comparison tests?
  - Simulating
      - ~~base simulate\_mixtur function~~
      - ~~check parameter recovery 3-component~~
      - ~~check parameter recovery 2-component~~
  - General
      - update documentation of all functions
      - add more example data sets
      - vignettes for all functions & use cases
  - Designing
      - ~~generate trial-level data for a user’s experiment~~
      - ensure this works for multiple participants
      - cie-lab calculation
      - cie2rgb converter
      - rgb2cie converter

## Contents

  - [Package Overview](#package-overview)
      - [How to install the package](#installation)
      - [An overview of continuous report
        tasks](#continuous-report-tasks)
      - [Example data shipped with mixtur](#example-data)
      - [An overview of data structures in
        mixtur](#data-structures-in-mixtur)
  - [Plotting: Visualising behavioural data](#plotting)
      - [Plotting response error](#plotting-response-error)
      - [Plotting response precision](#plotting-precision-of-responses)
  - [Analysing: Preparing data for inferential analysis](#analysing)
  - [Modelling: Fitting mixture models to data](#modelling)
      - [2-component mixture models](#two-component-mixture-models)
      - [3-component mixture models](#three-component-mixture-models)
  - [Plotting 2: Visualising model fit](#plotting-2-plotting-model-fit)
  - [Simulating: Generating data from mixture
    models](#simulating-generating-data-from-mixture-models)
  - [Designing: Experiment design tools](#designing)
  - [References](#references)

## Package Overview

### Installation

To install the package from GitHub, you need the devtools package:

``` r
install.packages("devtools")
library(devtools)
```

Then **mixtur** can be directly installed:

``` r
devtools::install_github("JimGrange/mixtur")
```

### Continuous report tasks

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

### Example data

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
    non-target values are in radians (-PI to PI).
  - **berrt\_2009**: TODO - Write blurb about studt. Data set has
    respone, target, and non-target values in degrees in the range -180
    to 180.

Data can be loaded in the following way:

``` r
library(mixtur)

# load the full data
data(bays2009_full)

# look at the top of the data
head(bays2009_full)
#>   id set_size delay response target non_target_1 non_target_2 non_target_3
#> 1  1        1   100   -0.651 -0.638           NA           NA           NA
#> 2  1        1   100    2.042  2.108           NA           NA           NA
#> 3  1        1   500   -2.953  3.141           NA           NA           NA
#> 4  1        1   500    1.918  2.368           NA           NA           NA
#> 5  1        1   500   -0.176 -0.155           NA           NA           NA
#> 6  1        1   100   -2.683 -2.609           NA           NA           NA
#>   non_target_4 non_target_5
#> 1           NA           NA
#> 2           NA           NA
#> 3           NA           NA
#> 4           NA           NA
#> 5           NA           NA
#> 6           NA           NA
```

### Data structures in mixtur

**mixtur** has some flexibility in the data structures it can deal with,
but you need to tell **mixtur** some things about your data so it can
work. The data should be in long format (where rows indicate separate
trials, and columns indicate trial information). Typically the data
should have the following columns (note your columns do not necessarily
need these names as you can set this within function calls; see examples
later).

  - **id:** A column indicating the participant numbers / identifiers.
  - **response:** A column providing the participant response value.
    This can either be in degrees (i.e., 1-360), degrees limited to the
    range 1-180 (i.e., if your experiment uses bar orientations), or
    radians (typically in the range 0-2PI, but could also be -PI to PI).
  - **target:** A column providing the target value. This will be used
    to calculate response error (i.e., the deviation between the
    participant’s response and the target value).
  - **non-target:** If the experiment had set sizes greater than one,
    the data should include columns that provide the non-target values
    (one column for each non-target). If there is more than one
    non-target, each column name should begin with a common term (e.g.,
    the “non-target” term is common to the non-target columns
    “non\_target\_1”, “non\_target\_2” etc.).
  - **set-size:** A column indicating the current trial’s set size
    (i.e., number of items to remember during the encoding phase), if
    your experiment manipulated this.
  - **other condition:** A column indicating the current trial’s
    condition if your experiment had an additional manipulation (for
    example the *delay* of the retention interval, as in the Bays
    \[2009\] data).

## Plotting

### Plotting response error

Response error refers to the angular deviation between the participant’s
response and the location of the true target value (i.e., where
participants *should* have responded). The plot functions returns the
probability density of response error averaged across participants
together with standard errors. (Unless told otherwise, **mixtur** will
assume you have multiple participants in your data, and will therefore
average across these participants.)

The function—*plot\_error*—takes the following arguments:

  - **data:** A data frame containing the data that is to be plotted.
    See the [data structure section](#data-structures-in-mixtur) for how
    this should be formatted.
  - **unit:** A character variable indicating the unit of measurement in
    the data. **mixtur** accepts units in degrees (1-360), degrees\_180
    capped at 180 (1-180), and radians (either 0-2PI or -PI to PI).
    Defaults to “degrees”.
  - **id\_var:** A character variable indicating the column name that
    codes for participant identification. Defaults to “id”.
  - **response\_var:** A character variable indicating the column name
    that codes for participants’ responses. Defaults to “response”.
  - **target\_var:** A character variable indicating the column name
    that codes for the target value. Defaults to “target”.
  - **set\_size\_var:** If set size was manipulated, a character
    variable indicating the column name that codes for the set size.
    Defaults to NULL.
  - **conditon\_var:** If an additional condition was manipulated, a
    character indicating the column name that codes for this condition.
    Defaults to NULL.
  - **return\_data:** A logical variable (“TRUE” or “FALSE”) indicating
    whether the plot data should be returned by the function. By
    default, the function will just return the plot itself (i.e.,
    return\_data = “FALSE”), but having the data returned allows the
    user to customise the plots further.

#### Examples

In the following we use the example data sets shipped with **mixtur**.

In a first example, we will use just the sample data from Bays et
al. (2009). Recall that the units in this data are in radians, so we
need to inform **mixtur** of this in the function call. As there was no
set size manipulation in this sample, and no other condition
manipulation, we do not need to set any other variables.

``` r
# load the sample of bays (2009) data
data(bays2009_sample)

# do the plotting
plot_error(data = bays2009_sample, 
           unit = "radians")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

If you wish to receive the data underpinning the plot, save the
plot\_error call to a new variable and set *return\_data* to TRUE.

``` r
my_data <- plot_error(data = bays2009_sample, 
                      unit = "radians",
                      return_data = TRUE)
```

The data can then be accessed in the following way:

``` r
my_data$data
#> # A tibble: 17 x 3
#>         x mean_error se_error
#>     <dbl>      <dbl>    <dbl>
#>  1 -2.96      0.0392   0.0148
#>  2 -2.59      0.0267   0.0106
#>  3 -2.22      0.0386   0.0175
#>  4 -1.85      0.0504   0.0222
#>  5 -1.48      0.0599   0.0182
#>  6 -1.11      0.0514   0.0165
#>  7 -0.739     0.144    0.0261
#>  8 -0.370     0.449    0.0480
#>  9  0         0.902    0.0655
#> 10  0.370     0.464    0.0394
#> 11  0.739     0.171    0.0225
#> 12  1.11      0.0880   0.0240
#> 13  1.48      0.0611   0.0171
#> 14  1.85      0.0323   0.0110
#> 15  2.22      0.0675   0.0159
#> 16  2.59      0.0277   0.0128
#> 17  2.96      0.0331   0.0137
```

In the full data set, Bays et al. (2009) also had a set-size
manipulation. Participants were presented with either 1, 2, 4, or 6
items to remember in the encoding phase. To plot how response error
varies by set size, we need to tell the plot\_error function the column
name that codes for our set size variable. Note that in the example the
column name is usefully set as *set\_size*, but it needn’t be this
useful (it could—for example—be as ridiculous as *womble*); just tell
**mixtur** what it is called and it will do the rest\!

``` r
# load bays (2009) full data set
data(bays2009_full)

# do the plotting
plot_error(data = bays2009_full,
           unit = "radians",
           set_size_var = "set_size", 
           condition_var = NULL)
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

Maybe your experiment didn’t manipulate set size, but it did manipulate
something else. In the full data set of Bays et al. (2009) they
manipulated the *delay* of the retention interval. To plot how response
error varies by a factor other than set size, we need to tell the
plot\_error function the column name that codes for it (using the
*condition\_var* variable):

``` r
# plot response error by condition
plot_error(data = bays2009_full,
           unit = "radians",
           condition_var = "delay")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

Maybe your experiment manipulated both set size AND an additional
condition. No worries; **mixtur** can handle this. By default the
plot\_error function will return a plot where the *set\_size* variable
is represented by different panels, and the *condition* variable is
represented by different colours within the plots. At the moment this is
the default and only behaviour; if you wish to customise the plots
please use the *return\_data* variable.

``` r
# plot response error by set size & condition
plot_error(data = bays2009_full,
           unit = "radians",
           set_size_var = "set_size", 
           condition_var = "delay")
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

### Plotting precision of reponses

Once response error is known, a formal estimate of the **precision** of
a participant’s response is possible. Following Bays et al. (2009),
precision is calculated as the reciprocol of the standard deviation of
the response error distribution (calculated as SD for circular space).
Below are two example conditions that differ in their precision:

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

Condition A has lower precision (i.e., higher SD) than Condition B. The
arguments to pass to the function that calcualtes & plots
precision—*plot\_precision*—are the same as for the *plot\_error*
function.

#### Examples

In the first example, we plot mean precision across all participants as
a function of set size.

``` r

# load the data
data(bays2009_full)

# plot response precision by set size
plot_precision(data = bays2009_full, 
               unit = "radians",
               set_size_var = "set_size")
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

Again, if we wanted the data underlying the plot, we can ask **mixtur**
for this:

``` r

# plot response precision by set size
plot_data <- plot_precision(data = bays2009_full, 
                            unit = "radians",
                            set_size_var = "set_size", 
                            return_data = TRUE)

# show the data
plot_data$data
#> # A tibble: 4 x 5
#>   set_size mean_precision se_precision mean_bias se_bias
#>      <dbl>          <dbl>        <dbl>     <dbl>   <dbl>
#> 1        1          3.40        0.244    0.00679  0.0108
#> 2        2          1.72        0.153    0.0107   0.0114
#> 3        4          0.819       0.0917   0.0172   0.0144
#> 4        6          0.508       0.0555  -0.0120   0.0441
```

Note that when you return the data from a plot there is data concerning
*bias*. This is a measure of response bias (i.e., responses with no bias
should be near zero), and is not used further in this package.

Note that if you have no conditions and no set-size manipulation, the
plotting function doesn’t return anything. This is because in effect you
are asking to just plot one data point, which doesn’t seem worthwhile.

Instead, just ask for the plot data.

``` r
plot_data <- plot_precision(data = bays2009_full, 
                            unit = "radians",
                            condition_var = NULL,
                            set_size_var = NULL, 
                            return_data = TRUE)
plot_data$data
#> # A tibble: 1 x 4
#>   mean_precision se_precision mean_bias se_bias
#>            <dbl>        <dbl>     <dbl>   <dbl>
#> 1           1.06       0.0826   0.00968 0.00863
```

In this example, we plot precision as a function of the *delay*
condition.

``` r
# plot just condition (delay)
plot_precision(data = bays2009_full, 
               unit = "radians",
               condition_var = "delay",
               set_size_var = NULL)
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

As before, if we want to plot both set size and an additional condition,
we can do so:

``` r
# plot set size AND condition
plot_precision(data = bays2009_full, 
               unit = "radians",
               condition_var = "delay",
               set_size_var = "set_size")
```

![](man/figures/README-unnamed-chunk-14-1.png)<!-- -->

## Analysing

Want to analyse the precision data that you’ve just plotted using your
favourite inferential method? No problem\! You just need to use the
*analyse\_precision* function, which behaves in almost the same way as
the *plot\_precision* function, except it just returns a data frame
grouped by participant id.

``` r

# save the new data frame to a variable
precision_data <- analyse_precision(data = bays2009_full, 
                                    unit = "radians",
                                    condition_var = "delay",
                                    set_size_var = "set_size")

# show the top of this new data frame
head(precision_data)
#> # A tibble: 6 x 5
#> # Groups:   id, condition [2]
#>      id condition set_size precision     bias
#>   <dbl> <fct>        <dbl>     <dbl>    <dbl>
#> 1     1 100              1     3.60  -0.0522 
#> 2     1 100              2     1.89  -0.0589 
#> 3     1 100              4     0.485 -0.00112
#> 4     1 100              6     0.428 -0.115  
#> 5     1 500              1     3.66   0.0154 
#> 6     1 500              2     2.16  -0.0595
```

Note that this function **does not** conduct the analysis for you. We
wish to stay out of the fierce “frequentist vs. Bayes” wars that occupy
academic social media\! This way, **mixtur** can play nicely with all.

## Modelling

The main reason for using **mixtur** for most users will likely be to
fit mixture models to data already collected. The **mixtur** package can
currently fit both the 2-component mixture model (i.e., that of Zhang &
Luck, 2008) and the 3-component mixture model (i.e., that of Bays et
al., 2009).

### Two-component mixture models

The two-component mixture model of Zhang and Luck (2008) assumes that
participant responses are a probabilistic mixture of two types of
response:

  - A (noisy) response to the correct target value, which is modelled as
    a Gaussian distribution for circular data (i.e., a [von
    Mises](https://en.wikipedia.org/wiki/Von_Mises_distribution)
    distribution) centered on the true target value.
  - Guessing, which is modelled as a uniform distribution with equal
    probability across all possible responses.

The two-component model has two main parameters that are estimated via
model fitting:

  - *K:* The concentration parameter of the von Mises distrubition,
    estimating the variability in response error. Larger values of K
    reflect

### Three-component mixture models

The three-component model takes into account

#### Bays et al. (2009 Figure 1d & 1e)

``` r

data <- bays2009_full

fit <- fit_mixtur(data = data,
                  components = 3,
                  unit = "radians",
                  id_var = "id",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = "set_size",
                  condition_var = NULL)
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

fit %>% 
  group_by(set_size) %>% 
  summarise(mean_parm = mean(p_n), 
            se_parm = sd(p_n) / sqrt(length(p_n))) %>% 
  ggplot(aes(x = set_size, 
             y = mean_parm)) + 
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymax = mean_parm + se_parm,
                    ymin = mean_parm - se_parm),
                width = 0.05) + 
  scale_y_continuous(limits = c(0, 0.4)) + 
  theme_bw()
#> `summarise()` ungrouping output (override with `.groups` argument)
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->

``` r
data <- bays2009_full

fit <- fit_mixtur(data = data,
                  components = 3,
                  id_var = "id",
                  unit = "radians",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = "set_size",
                  condition_var = NULL)
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

fit %>% 
  group_by(set_size) %>% 
  summarise(mean_parm = mean(p_u), 
            se_parm = sd(p_u) / sqrt(length(p_u))) %>% 
  ggplot(aes(x = set_size, 
             y = mean_parm)) + 
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymax = mean_parm + se_parm,
                    ymin = mean_parm - se_parm),
                width = 0.05) + 
  scale_y_continuous(limits = c(0, 0.4)) + 
  theme_bw()
#> `summarise()` ungrouping output (override with `.groups` argument)
```

![](man/figures/README-unnamed-chunk-17-1.png)<!-- -->

#### Bayes et al. (2009 Figure 1h & 1i)

``` r
data <- bays2009_full

fit <- fit_mixtur(data = data,
                  components = 3,
                  unit = "radians",
                  id_var = "id",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = "set_size",
                  condition_var = "delay")
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

# add position jitter to avoid over-plotting
pd <- position_dodge(0.0)

fit %>% 
  group_by(set_size, condition) %>% 
  summarise(mean_parm = mean(p_n), 
            se_parm = sd(p_n) / sqrt(length(p_n))) %>% 
  mutate(condition = as.factor(condition)) %>% 
  ggplot(aes(x = set_size, 
             y = mean_parm, 
             group = condition)) + 
  geom_point(aes(colour = condition), 
             position = pd, 
             size = 2.5) + 
  geom_line(aes(colour = condition, 
                linetype = condition), 
            position = pd) +
  geom_errorbar(aes(ymax = mean_parm + se_parm,
                    ymin = mean_parm - se_parm, 
                    colour = condition),
                width = 0.05, 
                position = pd) + 
  scale_y_continuous(limits = c(0, 0.4)) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()
#> `summarise()` regrouping output by 'set_size' (override with `.groups` argument)
```

![](man/figures/README-unnamed-chunk-18-1.png)<!-- -->

``` r
fit %>% 
  group_by(set_size, condition) %>% 
  summarise(mean_parm = mean(p_u), 
            se_parm = sd(p_u) / sqrt(length(p_u))) %>% 
  mutate(condition = as.factor(condition)) %>% 
  ggplot(aes(x = set_size, 
             y = mean_parm, 
             group = condition)) + 
  geom_point(aes(colour = condition), 
             position = pd, 
             size = 2.5) + 
  geom_line(aes(colour = condition, 
                linetype = condition), 
            position = pd) +
  geom_errorbar(aes(ymax = mean_parm + se_parm,
                    ymin = mean_parm - se_parm, 
                    colour = condition),
                width = 0.05, 
                position = pd) + 
  scale_y_continuous(limits = c(0, 0.4)) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()
#> `summarise()` regrouping output by 'set_size' (override with `.groups` argument)
```

![](man/figures/README-unnamed-chunk-19-1.png)<!-- -->

#### Plotting by condition

``` r
data <- bays2009_full

# get just the set size = 4 data
sample_data <- data %>% 
  filter(set_size == 4) %>% 
  select(-non_target_4, -non_target_5)

# fit the model
fit <- fit_mixtur(data = sample_data,
                  components = 3,
                  unit = "radians",
                  id_var = "id",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = NULL,
                  condition_var = "delay")
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

# add position jitter to avoid over-plotting
pd <- position_dodge(0.8)

# plot the fit
fit %>% 
  group_by(delay) %>% 
  summarise(mean_parm = mean(p_t), 
            se_parm = sd(p_t) / sqrt(length(p_t))) %>% 
  mutate(condition = as.factor(delay)) %>% 
  ggplot(aes(x = delay, 
             y = mean_parm)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymax = mean_parm + se_parm,
                    ymin = mean_parm - se_parm),
                width = 0.05) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()
#> `summarise()` ungrouping output (override with `.groups` argument)
```

![](man/figures/README-unnamed-chunk-20-1.png)<!-- -->

## Plotting 2: Plotting Model Fit

### Rough recreation of Figure 2 (Bays et al., 2009)

``` r

data <- bays2009_full

model_data <- fit_mixtur(data,
                         components=  3,
                         unit = "radians",
                         id_var = "id",
                         response_var = "response",
                         target_var = "target",
                         non_target_var = "non_target",
                         set_size_var = "set_size",
                         condition_var = NULL)
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

plot_model_fit(human_data = data,
               model_data = model_data,
               unit = "radians",
               id_var = "id",
               response_var = "response",
               target_var = "target",
               set_size_var = "set_size",
               condition_var = NULL)
```

![](man/figures/README-unnamed-chunk-21-1.png)<!-- -->

``` r

data <- bays2009_full

model_data <- fit_mixtur(data,
                         components=  3,
                         unit = "radians",
                         id_var = "id",
                         response_var = "response",
                         target_var = "target",
                         non_target_var = "non_target",
                         set_size_var = "set_size",
                         condition_var = "delay")
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

plot_model_fit(human_data = data,
               model_data = model_data,
               unit = "radians",
               id_var = "id",
               response_var = "response",
               target_var = "target",
               set_size_var = "set_size",
               condition_var = "delay")
```

![](man/figures/README-unnamed-chunk-22-1.png)<!-- -->

## Simulating: Generating Data From Mixture Models

## Two-component model

``` r
simulated_data <- simulate_mixtur(n_trials = 5000, 
                                  K = 15, 
                                  p_t = 0.75, 
                                  p_n = NULL, 
                                  p_u = 0.25, 
                                  set_size = 4)
#> [1] "Simulating data. Please wait..."
#> [1] "Simulating data finished."

head(simulated_data)
#>   id target response non_target_1 non_target_2 non_target_3
#> 1  1 -1.012    0.582       -2.548        1.414        2.793
#> 2  1  2.147    2.119       -2.251        0.925       -0.908
#> 3  1 -0.262   -0.100       -2.094        2.182        2.897
#> 4  1 -1.833   -1.701        0.454       -0.454       -2.897
#> 5  1  1.152    1.175        2.653       -2.007        0.192
#> 6  1 -0.733   -0.719       -2.374        1.885        0.698

fit <- fit_mixtur(data = simulated_data,
                  components = 2,
                  unit = "radians",
                  id_var = "id",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = NULL,
                  condition_var = NULL)
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

head(fit)
#>   id      K   p_t p_n   p_u
#> 1  1 15.194 0.748   0 0.252
```

### Three-component model

``` r
simulated_data <- simulate_mixtur(n_trials = 5000, 
                                  K = 15, 
                                  p_t = 0.75, 
                                  p_n = 0.15, 
                                  p_u = 0.10, 
                                  set_size = 4)
#> [1] "Simulating data. Please wait..."
#> [1] "Simulating data finished."

head(simulated_data)
#>   id target response non_target_1 non_target_2 non_target_3
#> 1  1  0.663    0.929        1.937       -1.065        2.985
#> 2  1 -3.142   -3.267       -0.140        0.960        1.868
#> 3  1  1.553    1.520       -2.793       -1.676       -0.977
#> 4  1 -2.478    1.130        0.454        2.688        1.606
#> 5  1 -0.209   -0.073       -2.304        3.072        1.187
#> 6  1  1.204    1.486       -1.222        2.862       -2.601

fit <- fit_mixtur(data = simulated_data,
                  components = 3,
                  unit = "radians",
                  id_var = "id",
                  response_var = "response",
                  target_var = "target",
                  non_target_var = "non_target",
                  set_size_var = NULL,
                  condition_var = NULL)
#> [1] "Model fit running. Please wait..."
#> [1] "Model fit finished."

head(fit)
#>   id     K   p_t   p_n   p_u
#> 1  1 15.32 0.752 0.153 0.095
```

## Designing

Coming soon\!

## References
