## Initial Submission
This is the initial submission of the package to CRAN.

## Test environments
* local OS Cataline v.10.15.7, R 4.0.2
* win-builder (devel and release)
* R-hub builder (Windows Server 2008 R2 SP1, R-devel, 32/64 bit; 
Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Win-builder test
There were no ERRORs or WARNINGS. 

There were 2 NOTEs:

* New submission

  This is the initial submission of the package to CRAN.

* Examples with CPU (user + system) or elapsed time > 10s

  The two functions with elapsed time > 10s ("fit_mixtur" and "simulate_mixtur")
  are necessarily long as they are conducting model fit routines.

## R-hub builder tests
There were no ERRORs or WARNINGS on any of the 3 platforms.

There were 2 NOTEs on each of the 3 platforms:

* New submission

  This is the initial submission of the package to CRAN.

* Examples with CPU (user + system) or elapsed time > 10s

  The two functions with elapsed time > 10s ("fit_mixtur" and "simulate_mixtur")
  are necessarily long as they are conducting model fit routines.
