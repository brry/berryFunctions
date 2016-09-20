Tests on win-builder show:

There were no ERRORs. 

One WARNING: Files in the 'vignettes' directory newer than all files in 'inst/doc'. 
- I believe this to be spurious as per https://stat.ethz.ch/pipermail/r-package-devel/2016q3/001089.html
  (Duncan Murdoch, Message to R-pkg-devel, Sept 15)

Two NOTEs:
Possibly mis-spelled words in DESCRIPTION: hydrograph 
- This is a valid technical term in hydrology describing the time series of flow in a river

Found the following (possibly) invalid URLs: https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html (CRAN URL not in canonical form)
- This is the best way I know to link directly to the web version of a vignette

