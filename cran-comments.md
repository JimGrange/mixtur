## Resubmission
This is the a revised submission of the package responding to CRAN comments on 
our original (and initial) submission.

In this resubmission we have:

* Removed the redundant "An R Package..." from the package's title.

* Shortened the name to less than 65 characters.

* Added relevant references to the description field of our DESCRIPTION file.

* Added \value to .Rd files regarding exported methods and explained the
functions' results in the documentation (including the structure of the 
output and what the output means).

* Examples which previously were wrapped in \dontrun{} are now wrapped in 
\donttest{} as advised.

* Messages written to the console now use message() rather than print().

* Relevant copyright declarations relating to code developed by Paul Bays is 
now contained in a Copyright field in our DESCRIPTION file.

## Test environments
* local OS Cataline v.10.15.7, R 4.0.2
* local Windows 10 Home, R 4.0.5
* win-builder (devel and release)
* R-hub builder:
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit; 
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC; 
    * Fedora Linux, R-devel, clang, gfortran.

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Win-builder test (devel and release)
There were no ERRORs or WARNINGS. 

There was 1 NOTE:

1. New submission

  This is the initial submission of the package to CRAN.


## R-hub builder test (Windows Server)
There was 1 ERROR:

1. Bioconductor does not yet build and check packages for R version 4.2

  This issue seems to be with setting up the container for testing on r-hub
  rather than an issue with the package as I cannot replicate it locally. This
  has been discussed under Issue #471 on the r-hub github page (https://github.com/r-hub/rhub/issues/471).  

There were 3 NOTEs:

1. New submission

  This is the initial submission of the package to CRAN.

2. Found the following (possibly) invalid DOIs:
    DOI: 10.1167/9.10.7
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

  I've checked the offending doi (10.1167/9.10.7) and it is correct and 
  working in the browser. Following the advice stated on R-hub (https://blog.r-hub.io/2020/12/01/url-checks/) I have proceeded with submission
  with this comment added to explain. 
  
3. Possibly mis-spelled words in DESCRIPTION:
  al (27:42)
  et (27:39)
  Zhang (26:22)
  
  These have been checked and are all correct.
  
  
## R-hub builder test (Ubuntu Linux)
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

1. New submission

  This is the initial submission of the package to CRAN.

2. Found the following (possibly) invalid DOIs:
    DOI: 10.1167/9.10.7
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

  I've checked the offending doi (10.1167/9.10.7) and it is correct and 
  working in the browser. Following the advice stated on R-hub (https://blog.r-hub.io/2020/12/01/url-checks/) I have proceeded with submission
  with this comment added to explain. 
  
3. Possibly mis-spelled words in DESCRIPTION:
  al (27:42)
  et (27:39)
  Zhang (26:22)
  
  These have been checked and are all correct.
  
## R-hub builder test (Fedora Linux)
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

1. New submission

  This is the initial submission of the package to CRAN.

2. Found the following (possibly) invalid DOIs:
    DOI: 10.1167/9.10.7
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

  I've checked the offending doi (10.1167/9.10.7) and it is correct and 
  working in the browser. Following the advice stated on R-hub (https://blog.r-hub.io/2020/12/01/url-checks/) I have proceeded with submission
  with this comment added to explain. 
  
3. Possibly mis-spelled words in DESCRIPTION:
  al (27:42)
  et (27:39)
  Zhang (26:22)
  
  These have been checked and are all correct.
