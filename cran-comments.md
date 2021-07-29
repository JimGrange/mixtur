## Resubmission
This is the a revised submission of the package responding to CRAN comments on 
our original (and initial) submission.

In this resubmission we have:

* Removed the redundant "An R Package..." from the package's title.
* Shortened the name to less than 65 characters.

* Added relevant references to the description field of our DESCRIPTION file.

* Examples which previously were wrapped in \dontrun{} are now wrapped in 
\donttest{} as advised.

* Messages written to the console now use message() rather than print().

* Relevant copyright declarations relating to code developed by Paul Bays is 
now contained in a Copyright field of our DESCRIPTION file.

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

## Win-builder test
There were no ERRORs or WARNINGS. 

There was 1 NOTE:

* New submission

  This is the initial submission of the package to CRAN.


## R-hub builder tests
There were no ERRORs or WARNINGS on any of the 3 platforms.

There was 1 NOTE on each of the 3 platforms:

* New submission

  This is the initial submission of the package to CRAN.
