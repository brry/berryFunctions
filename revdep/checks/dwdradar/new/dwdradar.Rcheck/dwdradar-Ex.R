pkgname <- "dwdradar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('dwdradar')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("readRadarFile")
### * readRadarFile

flush(stderr()); flush(stdout())

### Name: readRadarFile
### Title: read binary radolan radar file
### Aliases: readRadarFile
### Keywords: binary file

### ** Examples


f <- system.file("extdata/raa01_sf_2019-10-14_1950", package="dwdradar")
out <- readRadarFile(f)
out$meta

if(requireNamespace("raster", quietly=TRUE))
  raster::plot(raster::raster(out$dat))

# for more files, see the tests.
# for real-world usage, readDWD.binary / readDWD.radar in the rdwd package




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
