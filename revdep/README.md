# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, mingw32              |
|ui       |RStudio (1.0.136)            |
|language |(EN)                         |
|collate  |German_Germany.1252          |
|tz       |Europe/Berlin                |
|date     |2017-04-07                   |

## Packages

|package        |*  |version  |date       |source                         |
|:--------------|:--|:--------|:----------|:------------------------------|
|abind          |   |1.4-5    |2016-07-21 |CRAN (R 3.3.2)                 |
|berryFunctions |*  |1.14.35  |2017-04-07 |local (brry/berryFunctions@NA) |
|knitr          |   |1.15.1   |2016-11-22 |CRAN (R 3.3.3)                 |
|RColorBrewer   |   |1.1-2    |2014-12-07 |CRAN (R 3.3.2)                 |
|RCurl          |   |1.95-4.8 |2016-03-01 |CRAN (R 3.3.3)                 |
|rmarkdown      |   |1.4      |2017-03-24 |CRAN (R 3.3.3)                 |

# Check results
3 packages

## extremeStat (1.3.0)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in 'extremeStat-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: distLexBoot
> ### Title: Bootstrapping uncertainty intervals for return periods
> ### Aliases: distLexBoot
> ### Keywords: bootstrap distribution dplot hplot montecarlo ts
> 
> ### ** Examples
> 
> 
> data(annMax)
> dlf <- distLextreme(annMax, selection=c("wak","gum","gev","nor"))
distLfit execution took 0.03 seconds.
> dlfB <- distLexBoot(dlf, nbest=4, conf.lev=0.5, n=10) # n low for quick example tests
Error in loadNamespace(name) : there is no package called 'pbapply'
Calls: distLexBoot ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
Execution halted
```

## OSMscale (0.4.1)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

0 errors | 0 warnings | 0 notes

## rdwd (0.7.0)
Maintainer: Berry Boessenkool <berry-b@gmx.de>

0 errors | 0 warnings | 0 notes

