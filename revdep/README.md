# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.2 (2017-09-28) |
|system   |x86_64, mingw32              |
|ui       |RStudio (1.1.383)            |
|language |(EN)                         |
|collate  |German_Germany.1252          |
|tz       |Europe/Berlin                |
|date     |2017-11-03                   |

## Packages

|package        |*  |version  |date       |source                         |
|:--------------|:--|:--------|:----------|:------------------------------|
|abind          |   |1.4-5    |2016-07-21 |CRAN (R 3.4.1)                 |
|berryFunctions |*  |1.15.43  |2017-11-03 |local (brry/berryFunctions@NA) |
|colorspace     |   |1.3-2    |2016-12-14 |CRAN (R 3.4.2)                 |
|gstat          |   |1.1-5    |2017-03-12 |CRAN (R 3.4.2)                 |
|knitr          |   |1.17     |2017-08-10 |CRAN (R 3.4.2)                 |
|pbapply        |*  |1.3-3    |2017-07-04 |CRAN (R 3.4.1)                 |
|RColorBrewer   |   |1.1-2    |2014-12-07 |CRAN (R 3.4.1)                 |
|RCurl          |   |1.95-4.8 |2016-03-01 |CRAN (R 3.4.1)                 |
|rmarkdown      |   |1.6      |2017-06-15 |CRAN (R 3.4.2)                 |

# Check results

3 packages

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|extremeStat |1.3.0   |      1|        0|     0|
|OSMscale    |0.5.1   |      1|        0|     0|
|rdwd        |0.8.0   |      0|        1|     0|

## extremeStat (1.3.0)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

1 error  | 0 warnings | 0 notes

```
checking whether package 'extremeStat' can be installed ... ERROR
Installation failed.
See 'S:/Dropbox/Rpack/berryFunctions/revdep/checks/extremeStat.Rcheck/00install.out' for details.
```

## OSMscale (0.5.1)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

1 error  | 0 warnings | 0 notes

```
checking whether package 'OSMscale' can be installed ... ERROR
Installation failed.
See 'S:/Dropbox/Rpack/berryFunctions/revdep/checks/OSMscale.Rcheck/00install.out' for details.
```

## rdwd (0.8.0)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 89-94 (rdwd.Rmd) 
Error: processing vignette 'rdwd.Rmd' failed with diagnostics:
in buildVignettes -> engine$ readDWD -> checkFile :  The file 'C:\Users\berry\AppData\Local\Temp\RtmpOez1N9/C:/Users/berry/AppData/Local/Temp/RtmpOez1N9/daily_kl_recent_tageswerte_KL_03987_akt.zip'
  does not exist. Current getwd: S:/Dropbox/Rpack/berryFunctions/revdep/checks/rdwd.Rcheck/vign_test/rdwd/vignettes
Execution halted

```

