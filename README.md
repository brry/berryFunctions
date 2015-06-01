berryFunctions
==============

My misc functions package. Notable: colPoints, horizHist, logAxis, pointZoom, smallPlot, lsc.

Code to install:

```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("brry/berryFunctions")
library(berryFunctions)
?berryFunctions # run the examples to get an idea of what's possible.
```

If this doesnt work, presumably, your R version is too old. In that case, an update is really recommendable: [r-project.org](http://www.r-project.org/)

If you can't update R, here's a workaround:
click on **Download ZIP** (to the right), unzip the file to some place, then

```R
getwd("that/path")
dd <- dir("berryFunctions-master/R", full=T)
dummy <- sapply(dd, source)
```
