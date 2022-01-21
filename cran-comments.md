## Resubmission
This is a resubmission. In this version I have:

* Added the dontrun{} environment to those examples with CPU or elapsed time > 5s.

## Test environments
* local OS (windows) install, R 3.6.1
* ubuntu-latest (on GitHub Actions), R release, devel, oldrel-1
* windows-latest (on GitHub Actions), R release
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-devel (r-devel)

## R CMD check results
> On ubuntu-latest (release, devel, oldrel-1)

0 errors | 0 warnings | 0 notes

> On windows-latest (release)

0 errors | 0 warnings | 0 notes

> On windows-x86_64-devel (r-devel)

checking CRAN incoming feasibility ... NOTE
  Maintainer: 'C. Alfaro <c.alfarog@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Geospatial (3:10, 21:51)
    Grizzetti (23:5, 24:5, 25:5, 26:5)
    al (23:18, 24:18, 25:18, 26:18)
    et (23:15, 24:15, 25:15, 26:15)

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors | 0 warnings | 2 notes

> On ubuntu-gcc-devel (r-devel)

0 errors | 0 warnings | 0 notes

  
## Previous cran-comments

## Test environments
* local OS (windows) install, R 3.6.1
* ubuntu-latest (on GitHub Actions), R release, devel, oldrel-1
* windows-latest (on GitHub Actions), R release
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-devel (r-devel)

## R CMD check results
> On ubuntu-latest (release, devel, oldrel-1)

0 errors | 0 warnings | 0 notes

> On windows-latest (release)

0 errors | 0 warnings | 0 notes

> On windows-x86_64-devel (r-devel)

checking CRAN incoming feasibility ... NOTE
  Maintainer: 'C. Alfaro <c.alfarog@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Geospatial (3:10, 21:51)
    Grizzetti (23:5, 24:5, 25:5, 26:5)
    al (23:18, 24:18, 25:18, 26:18)
    et (23:15, 24:15, 25:15, 26:15)

checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                      user system elapsed
  input_maps         37.30   3.78   41.16
  compare_calib      16.23   0.51   17.15
  nut_balance         9.66   0.28   13.93
  simobs_annual_plot  8.31   0.17   10.39
  N4_sankey           9.41   0.23    9.75
  input_Tserie        5.16   0.37    5.54

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors | 0 warnings | 3 notes

> On ubuntu-gcc-devel (r-devel)

0 errors | 0 warnings | 0 notes

* This is a new release.
