## Test environments
* local OS Ventura v.13.2.1, R 4.2.1
* local Windows 10 Home, R 4.2.2
* win-builder (devel and release)
* R-hub builder:
    * Debian Linux, R-devel, GCC ASAN/UBSAN; 
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC; 
    * Fedora Linux, R-devel, clang, gfortran.

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 


## Win-builder test (devel and release)
There were no ERRORs or WARNINGS. 

There was 1 NOTE:

### Checking CRAN incoming feasibility

* New submission
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed in this submission.

* Package was archived on CRAN
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed. 

* CRAN repository db overrides: X-CRAN-Comment: Archived on 2023-04-03 as issues were not corrected
  in time.
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed.

* Possibly mis-spelled words in DESCRIPTION: al (27:42), et (27:39), Zhang (26:22) 
  * These have been checked and are all correct.


## R-hub builder test (Debian Linux)
There were no ERRORs, WARNINGs, or NOTEs.

  
## R-hub builder test (Ubuntu Linux)
There were no ERRORs or WARNINGs.

There was 1 NOTE:

### Checking CRAN incoming feasibility

* New submission
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed in this submission.
    
* Package was archived on CRAN
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed. 

* Possibly mis-spelled words in DESCRIPTION: al (27:42), et (27:39), Zhang (26:22) 
  * These have been checked and are all correct.

* CRAN repository db overrides: X-CRAN-Comment: Archived on 2023-04-03 as issues were not corrected
  in time.
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed.

* Found the following (possibly) invalid DOIs: DOI: 10.1167/9.10.7, From: DESCRIPTION, Status: Forbidden, Message: 403
  * I've checked the offending doi (10.1167/9.10.7) and it is correct and working in the browser. Following the advice stated on R-hub (https://blog.r-hub.io/2020/12/01/url-checks/) I have proceeded with submission with this comment added to explain. 
  
## R-hub builder test (Fedora Linux)
There were no ERRORs or WARNINGs.

There were 2 NOTEs

### Checking CRAN incoming feasibility
* New submission
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed.
    
* Package was archived on CRAN
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed. 

* Possibly mis-spelled words in DESCRIPTION: al (27:42), et (27:39), Zhang (26:22) 
  * These have been checked and are all correct.

* CRAN repository db overrides: X-CRAN-Comment: Archived on 2023-04-03 as issues were not corrected
  in time.
  * mixtur v1.2.0 was removed from CRAN as I didn't deal with an issue in time. These issues have now been successfully addressed.
  
### Checking HTML version of manual
* Skipping checking HTML validation: no command 'tidy' found
  * I have ignored this note.

