pkgname <- "rdwd"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rdwd')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addBorders")
### * addBorders

flush(stderr()); flush(stdout())

### Name: addBorders
### Title: add country and Bundesland borders to a map
### Aliases: addBorders
### Keywords: aplot

### ** Examples

if(requireNamespace("raster", quietly=TRUE)){
plot(1, xlim=c(2,16), ylim=c(47,55)) 
addBorders()
plot(1, xlim=c(2,16), ylim=c(47,55))
addBorders(de="orange", eu=NA)
}




cleanEx()
nameEx("checkIndex")
### * checkIndex

flush(stderr()); flush(stdout())

### Name: checkIndex
### Title: check indexes
### Aliases: checkIndex

### ** Examples

data(fileIndex) ; data(metaIndex) ; data(geoIndex)
# ci <- checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex)
# cat(ci)



cleanEx()
nameEx("createIndex")
### * createIndex

flush(stderr()); flush(stdout())

### Name: createIndex
### Title: Create file and meta index of the DWD CDC FTP Server
### Aliases: createIndex
### Keywords: manip

### ** Examples

## Not run: 
##D  # Not tested with R CMD check because of file writing
##D link <- "daily/kl/historical/tageswerte_00699_19490101_19580630_hist.zip"
##D ind <- createIndex(link, dir=tempdir())
##D ind
##D link2 <- "daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"
##D link3 <- "daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt"
##D ind2 <- createIndex(c(link,link2,link3), dir=tempdir(), meta=TRUE)
##D lapply(ind2, head)
## End(Not run)




cleanEx()
nameEx("dataDWD")
### * dataDWD

flush(stderr()); flush(stdout())

### Name: dataDWD
### Title: Download data from the DWD CDC FTP Server
### Aliases: dataDWD
### Keywords: data file

### ** Examples

## Not run: 
##D  ## requires internet connection
##D # find FTP files for a given station name and file path:
##D link <- selectDWD("Fuerstenzell", res="hourly", var="wind", per="recent")
##D # download file:
##D fname <- dataDWD(link, dir=tempdir(), read=FALSE) ; fname
##D # dir="DWDdata" is the default directory to store files
##D # unless force=TRUE, already obtained files will not be downloaded again
##D 
##D # read and plot file:
##D wind <- readDWD(fname, varnames=TRUE) ; head(wind)
##D metafiles <- readMeta(fname)          ; str(metafiles, max.level=1)
##D column_names <- readVars(fname)       ; head(column_names)
##D 
##D plot(wind$MESS_DATUM, wind$F, main="DWD hourly wind Fuerstenzell", col="blue",
##D      xaxt="n", las=1, type="l", xlab="Date", ylab="Hourly Wind speed  [m/s]")
##D berryFunctions::monthAxis(1)
##D 
##D 
##D # current and historical files:
##D link <- selectDWD("Potsdam", res="daily", var="kl", per="hr"); link
##D potsdam <- dataDWD(link, dir=tempdir())
##D potsdam <- do.call(rbind, potsdam) # this will partly overlap in time
##D plot(TMK~MESS_DATUM, data=tail(potsdam,1500), type="l")
##D # The straight line marks the jump back in time
##D # Keep only historical data in the overlap time period:
##D potsdam <- potsdam[!duplicated(potsdam$MESS_DATUM),]
##D 
##D 
##D # With many files (>>50), use sleep to avoid getting kicked off the FTP server
##D #links <- selectDWD(res="daily", var="solar")
##D #sol <- dataDWD(links, sleep=20) # random waiting time after download (0 to 20 secs)
##D 
##D # Real life examples can be found in the use cases section of the vignette:
##D # browseURL("https://bookdown.org/brry/rdwd")
## End(Not run)




cleanEx()
nameEx("dirDWD")
### * dirDWD

flush(stderr()); flush(stdout())

### Name: dirDWD
### Title: directory management for rdwd
### Aliases: dirDWD
### Keywords: file

### ** Examples

# see source code of dataDWD and metaDWD




cleanEx()
nameEx("dwdparams")
### * dwdparams

flush(stderr()); flush(stdout())

### Name: dwdparams
### Title: DWD parameter explanations
### Aliases: dwdparams
### Keywords: datasets

### ** Examples

head(dwdparams)




cleanEx()
nameEx("findID")
### * findID

flush(stderr()); flush(stdout())

### Name: findID
### Title: find DWD weather station ID from name
### Aliases: findID
### Keywords: character

### ** Examples

# Give weather station name (must be existing in metaIndex):
findID("Potsdam")
findID("potsDam") # capitalization is ignored
# all names containing "Hamburg":
findID("Hamburg", exactmatch=FALSE)
findID("Potsdam", exactmatch=FALSE)

# vectorized:
findID(c("Potsdam","Berlin-Buch"))

# German Umlauts are changed to ue, ae, oe, ss
findID("Muenchen", FALSE)
berryFunctions::convertUmlaut("M?nchen") # use this to convert umlauts in lists




cleanEx()
nameEx("index")
### * index

flush(stderr()); flush(stdout())

### Name: index
### Title: Indexes of files and metadata on the DWD CDC FTP server
### Aliases: index fileIndex metaIndex geoIndex gridIndex formatIndex
### Keywords: datasets

### ** Examples

data(fileIndex)
data(metaIndex)
data(geoIndex)
head(fileIndex)
head(metaIndex)
head(geoIndex)

# in functions, you can use head(rdwd:::fileIndex) etc, but I don't export them
# because Hadley says 'Never @export a data set' in
# browseURL("http://r-pkgs.had.co.nz/data.html#data-data")




cleanEx()
nameEx("indexFTP")
### * indexFTP

flush(stderr()); flush(stdout())

### Name: indexFTP
### Title: Create a recursive index of an FTP Server
### Aliases: indexFTP
### Keywords: file

### ** Examples

## Not run: 
##D  ## Needs internet connection
##D sol <- indexFTP(folder="/daily/solar", dir=tempdir())
##D head(sol)
##D 
##D # mon <- indexFTP(folder="/monthly/kl", dir=tempdir(), verbose=TRUE)
## End(Not run)




cleanEx()
nameEx("localtestdir")
### * localtestdir

flush(stderr()); flush(stdout())

### Name: localtestdir
### Title: local test data directory
### Aliases: localtestdir
### Keywords: file

### ** Examples

localtestdir()




cleanEx()
nameEx("metaInfo")
### * metaInfo

flush(stderr()); flush(stdout())

### Name: metaInfo
### Title: Information for a station ID on the DWD CDC FTP server
### Aliases: metaInfo
### Keywords: datasets

### ** Examples

metaInfo(2849)




cleanEx()
nameEx("nearbyStations")
### * nearbyStations

flush(stderr()); flush(stdout())

### Name: nearbyStations
### Title: Find DWD stations close to given coordinates
### Aliases: nearbyStations

### ** Examples


m <- nearbyStations(49.211784, 9.812475, radius=30,
    res=c("daily","hourly"), var= c("precipitation","more_precip","kl") ,
    mindate=as.Date("2016-05-30"), statname="Braunsbach catchment center")
# View(m)
    
# for a continued example of this, see the vignette in chapter
# use case: plot all rainfall values around a given point
# browseURL("https://bookdown.org/brry/rdwd")




cleanEx()
nameEx("newColumnNames")
### * newColumnNames

flush(stderr()); flush(stdout())

### Name: newColumnNames
### Title: Enhance readDWD column names
### Aliases: newColumnNames

### ** Examples

# mainly for internal usage




cleanEx()
nameEx("plotRadar")
### * plotRadar

flush(stderr()); flush(stdout())

### Name: plotRadar
### Title: plot radar products on a pretty map
### Aliases: plotRadar
### Keywords: aplot spatial

### ** Examples

# See homepage in the section 'See Also'
## Not run: 
##D  ## Excluded from CRAN checks: requires internet connection
##D link <- "seasonal/air_temperature_mean/16_DJF/grids_germany_seasonal_air_temp_mean_188216.asc.gz"
##D rad <- dataDWD(link, base=gridbase, joinbf=TRUE, dir=tempdir())
##D plotRadar(rad, proj="seasonal", extent=rad@extent)
##D plotRadar(rad, ylim=c(52,54), proj="seasonal", extent=rad@extent)
##D plotRadar(rad)
## End(Not run)




cleanEx()
nameEx("projectRasterDWD")
### * projectRasterDWD

flush(stderr()); flush(stdout())

### Name: projectRasterDWD
### Title: project DWD raster data
### Aliases: projectRasterDWD
### Keywords: aplot

### ** Examples

# To be used after readDWD.binary etc



cleanEx()
nameEx("readDWD")
### * readDWD

flush(stderr()); flush(stdout())

### Name: readDWD
### Title: Process data from the DWD CDC FTP Server
### Aliases: readDWD
### Keywords: chron file

### ** Examples

# see dataDWD




cleanEx()
nameEx("readDWD.asc")
### * readDWD.asc

flush(stderr()); flush(stdout())

### Name: readDWD.asc
### Title: read dwd gridded radolan asc data
### Aliases: readDWD.asc

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D # File selection and download:
##D datadir <- localtestdir()
##D radbase <- paste0(gridbase,"/hourly/radolan/historical/asc/")
##D radfile <- "2018/RW-201809.tar" # 25 MB to download
##D file <- dataDWD(radfile, base=radbase, joinbf=TRUE, dir=datadir,
##D                 dbin=TRUE, read=FALSE) # download with mode=wb!!!
##D                 
##D #asc <- readDWD(file) # 4 GB in mem. ~ 20 secs unzip, 30 secs read, 10 min divide
##D asc <- readDWD(file, selection=1:5, dividebyten=TRUE)
##D asc <- projectRasterDWD(asc)
##D 
##D raster::plot(asc[[1]], main=names(asc)[1])
##D addBorders()
##D 
##D rng <- range(raster::cellStats(asc, "range"))
##D nframes <- 3 # raster::nlayers(asc) for all (time intensive!)
##D viddir <- paste0(tempdir(),"/RadolanVideo")
##D dir.create(viddir)
##D png(paste0(viddir,"/Radolan_%03d.png"), width=7, height=5, units="in", res=300)
##D dummy <- pbsapply(1:nframes, function(i) 
##D          raster::plot(asc[[i]], main=names(asc)[i], zlim=rng)) # 3 secs per layer
##D dev.off()
##D berryFunctions::openFile(paste0(viddir,"/Radolan_001.png"))
##D 
##D # Time series of a given point in space:
##D plot(as.vector(asc[800,800,]), type="l", xlab="Time [hours]")
##D 
##D # if dividebyten=FALSE, raster stores things out of memory in the exdir.
##D # by default, this is in tempdir, hence you would need to save asc manually:
##D # raster::writeRaster(asc, paste0(datadir,"/RW2018-09"), overwrite=TRUE) 
## End(Not run)



cleanEx()
nameEx("readDWD.binary")
### * readDWD.binary

flush(stderr()); flush(stdout())

### Name: readDWD.binary
### Title: read dwd gridded radolan binary data
### Aliases: readDWD.binary

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D # SF file as example: ----
##D 
##D SF_link <- "/daily/radolan/historical/bin/2017/SF201712.tar.gz"
##D SF_file <- dataDWD(file=SF_link, base=gridbase, joinbf=TRUE,   # 204 MB
##D                      dir=localtestdir(), read=FALSE)
##D # exdir radardir set to speed up my tests:
##D SF_exdir <- "C:/Users/berry/Desktop/DWDbinarySF"
##D if(!file.exists(SF_exdir)) SF_exdir <- tempdir()
##D # no need to read all 24*31=744 files, so setting selection:
##D SF_rad <- readDWD(SF_file, selection=1:10, exdir=SF_exdir) #with toraster=TRUE 
##D if(length(SF_rad)!=2) stop("length(SF_rad) should be 2, but is ", length(SF_rad))
##D 
##D SF_radp <- projectRasterDWD(SF_rad$dat)
##D raster::plot(SF_radp[[1]], main=SF_rad$meta$date[1])
##D addBorders()
##D 
##D 
##D # RW file as example: ----
##D 
##D RW_link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"
##D RW_file <- dataDWD(file=RW_link, base=gridbase, joinbf=TRUE,   # 25 MB
##D                   dir=localtestdir(), read=FALSE)
##D RW_exdir <- "C:/Users/berry/Desktop/DWDbinaryRW"
##D if(!file.exists(RW_exdir)) RW_exdir <- tempdir()
##D RW_rad <- readDWD(RW_file, selection=1:10, exdir=RW_exdir)
##D RW_radp <- projectRasterDWD(RW_rad$dat, extent="rw")
##D raster::plot(RW_radp[[1]], main=RW_rad$meta$date[1])
##D addBorders()
##D 
##D # ToDo: why are values + patterns not the same?
##D 
##D # list of all Files: ----
##D data(gridIndex)
##D head(grep("historical", gridIndex, value=TRUE))
## End(Not run)



cleanEx()
nameEx("readDWD.meta")
### * readDWD.meta

flush(stderr()); flush(stdout())

### Name: readDWD.meta
### Title: read dwd metadata (Beschreibung*.txt files)
### Aliases: readDWD.meta

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D link <- selectDWD(res="daily", var="kl", per="r", meta=TRUE)
##D if(length(link)!=1) stop("length of link should be 1, but is ", length(link), 
##D                 ":\n", berryFunctions::truncMessage(link,prefix="",sep="\n"))
##D 
##D file <- dataDWD(link, dir=localtestdir(), read=FALSE)
##D meta <- readDWD(file)
##D head(meta)
##D 
##D cnm <- colnames(meta)
##D if(length(cnm)!=8) stop("number of columns should be 8, but is ", length(cnm),
##D                         ":\n", toString(cnm))
## End(Not run)



cleanEx()
nameEx("readDWD.multia")
### * readDWD.multia

flush(stderr()); flush(stdout())

### Name: readDWD.multia
### Title: read multi_annual dwd data
### Aliases: readDWD.multia

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D # Temperature aggregates (2019-04 the 9th file):
##D durl <- selectDWD(res="multi_annual", var="mean_81-10", per="")[9]
##D murl <- selectDWD(res="multi_annual", var="mean_81-10", per="", meta=TRUE)[9]
##D 
##D ma_temp <- dataDWD(durl, dir=localtestdir())
##D ma_meta <- dataDWD(murl, dir=localtestdir())
##D 
##D head(ma_temp)
##D head(ma_meta)
##D 
##D ma <- merge(ma_meta, ma_temp, all=TRUE)
##D berryFunctions::linReg(ma$Stationshoehe, ma$Jahr)
##D op <- par(mfrow=c(3,4), mar=c(0.1,2,2,0), mgp=c(3,0.6,0))
##D for(m in colnames(ma)[8:19])
##D   {
##D   berryFunctions::linReg(ma$Stationshoehe, ma[,m], xaxt="n", xlab="", ylab="", main=m)
##D   abline(h=0)
##D   }
##D par(op)
##D 
##D par(bg=8)
##D berryFunctions::colPoints(ma$geogr..Laenge, ma$geogr..Breite, ma$Jahr, add=F, asp=1.4)
##D 
##D data("DEU")
##D pdf("MultiAnn.pdf", width=8, height=10)
##D par(bg=8)
##D for(m in colnames(ma)[8:19])
##D   {
##D   raster::plot(DEU, border="darkgrey")
##D   berryFunctions::colPoints(ma[-262,]$geogr..Laenge, ma[-262,]$geogr..Breite, ma[-262,m], 
##D                             asp=1.4, # Range=range(ma[-262,8:19]), 
##D                             col=berryFunctions::divPal(200, rev=TRUE), zlab=m, add=T)
##D   }
##D dev.off()
##D berryFunctions::openFile("MultiAnn.pdf")
## End(Not run)



cleanEx()
nameEx("readDWD.nc")
### * readDWD.nc

flush(stderr()); flush(stdout())

### Name: readDWD.nc
### Title: read dwd netcdf data
### Aliases: readDWD.nc

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D library(berryFunctions) # for seqPal and colPointsLegend
##D 
##D url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
##D url <- "daily/Project_TRY/humidity/RH_199509_daymean.nc.gz"    # 25 MB
##D file <- dataDWD(url, base=gridbase, joinbf=TRUE, dir=localtestdir(), read=FALSE)
##D nc <- readDWD(file)
##D ncp <- projectRasterDWD(nc, proj="nc", extent="nc")
##D  for(i in 1:3) raster::plot(ncp[[i]], col=seqPal(), 
##D                             main=paste(nc@title, nc@z[[1]][i]))
##D addBorders()
##D str(nc, max.level=2)
##D 
##D raster::values(nc[[1]]) # obtain actual values into memory
##D  
##D raster::plot(nc[[1]]) # axes 0:938 / 0:720, the number of grid cells
##D raster::plot(ncp[[1]])# properly projected, per default onto latlon
##D 
##D rng <- range(raster::cellStats(nc[[1:6]], "range"))
##D raster::plot(nc, col=seqPal(), zlim=rng, maxnl=6)
##D 
##D # Array instead of raster brick:
##D nc <- readDWD(file, toraster=FALSE)
##D image(nc$var[,,1], col=seqPal(), asp=1.1)
##D colPointsLegend(nc$var[,,1], title=paste(nc$varname, nc$time[1]))
##D 
##D # interactive selection of variable:
##D # nc <- readDWD(file, var="-") # uncommented to not block automated tests 
##D str(nc$var)
## End(Not run)



cleanEx()
nameEx("readDWD.radar")
### * readDWD.radar

flush(stderr()); flush(stdout())

### Name: readDWD.radar
### Title: read dwd gridded radolan radar data
### Aliases: readDWD.radar

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D # recent radar files 
##D rrf <- indexFTP("hourly/radolan/recent/bin", base=gridbase, dir=tempdir())
##D lrf <- dataDWD(rrf[773], base=gridbase, joinbf=TRUE, dir=tempdir(), read=FALSE)
##D r <- readDWD(lrf)
##D 
##D rp <- projectRasterDWD(r$dat)
##D raster::plot(rp, main=r$meta$date)
##D addBorders()
## End(Not run)



cleanEx()
nameEx("readDWD.raster")
### * readDWD.raster

flush(stderr()); flush(stdout())

### Name: readDWD.raster
### Title: read dwd gridded raster data
### Aliases: readDWD.raster

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D rasterbase <- paste0(gridbase,"/seasonal/air_temperature_mean")
##D ftp.files <- indexFTP("/16_DJF", base=rasterbase, dir=tempdir())
##D localfiles <- dataDWD(ftp.files[1:2], base=rasterbase, joinbf=TRUE,
##D                       dir=localtestdir(), read=FALSE)
##D rf <- readDWD(localfiles[1])
##D rf <- readDWD(localfiles[1]) # runs faster at second time due to skip=TRUE
##D raster::plot(rf)
##D 
##D rfp <- projectRasterDWD(rf, proj="seasonal", extent=rf@extent)
##D raster::plot(rfp)
##D addBorders()
##D 
##D testthat::expect_equal(raster::cellStats(rf, range), c(-8.2,4.4))
##D rf10 <- readDWD(localfiles[1], dividebyten=FALSE)
##D raster::plot(rf10)
##D testthat::expect_equal(raster::cellStats(rf10, range), c(-82,44))
## End(Not run)



cleanEx()
nameEx("readDWD.stand")
### * readDWD.stand

flush(stderr()); flush(stdout())

### Name: readDWD.stand
### Title: read subdaily/standard_format dwd data
### Aliases: readDWD.stand

### ** Examples

## Not run: 
##D  # Excluded from CRAN checks, but run in localtests
##D 
##D link <- selectDWD(id=10381, res="subdaily", var="standard_format", per="r")
##D file <- dataDWD(link, dir=localtestdir(), read=FALSE)
##D sf <- readDWD(file)
##D 
##D sf2 <- readDWD(file, fast=FALSE) # 20 secs!
##D stopifnot(all.equal(sf, sf2))
##D 
##D plot(sf$Date, sf$SHK, type="l")
##D 
##D # Plot all columns:
##D if(FALSE){ # not run in any automated testing
##D tmp <- tempfile(fileext=".pdf")
##D char2fact <- function(x) 
##D  {
##D  if(all(is.na(x))) return(rep(-9, len=length(x)))
##D  if(!is.numeric(x)) as.factor(x) else x
##D  }
##D pdf(tmp, width=9)
##D par(mfrow=c(2,1),mar=c(2,3,2,0.1), mgp=c(3,0.7,0), las=1)
##D for(i in 3:ncol(sf)-1) plot(sf$Date, char2fact(sf[,i]), type="l", main=colnames(sf)[i], ylab="")
##D dev.off()
##D berryFunctions::openFile(tmp)
##D }
## End(Not run)



cleanEx()
nameEx("readMeta")
### * readMeta

flush(stderr()); flush(stdout())

### Name: readMeta
### Title: Process data from the DWD CDC FTP Server
### Aliases: readMeta
### Keywords: file

### ** Examples

# see dataDWD




cleanEx()
nameEx("readVars")
### * readVars

flush(stderr()); flush(stdout())

### Name: readVars
### Title: Process data from the DWD CDC FTP Server
### Aliases: readVars
### Keywords: file

### ** Examples

# see dataDWD




cleanEx()
nameEx("selectDWD")
### * selectDWD

flush(stderr()); flush(stdout())

### Name: selectDWD
### Title: Select data from the DWD CDC FTP Server
### Aliases: selectDWD
### Keywords: file

### ** Examples

# Give weather station name (must be existing in metaIndex):
selectDWD("Potsdam", res="daily", var="kl", per="historical")

# all files for all stations matching "Koeln":
selectDWD("Koeln", res="", var="", per="", exactmatch=FALSE)
findID("Koeln", FALSE)

## Not run: 
##D  # Excluded from CRAN checks to save time
##D 
##D # selectDWD("Potsdam") # interactive selection of res/var/per
##D 
##D # directly give station ID, can also be id="00386" :
##D selectDWD(id=386, res="daily", var="kl", per="historical")
##D 
##D # period can be abbreviated:
##D selectDWD(id="00386", res="daily", var="kl", per="h")
##D selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE)
##D 
##D # vectorizable:
##D selectDWD(id="01050", res="daily", var="kl", per="rh") # list if outvec=F
##D selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="r")
##D # vectorization gives not the outer product, but elementwise comparison:
##D selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="hr")
##D 
##D # all zip files in all paths matching id:
##D selectDWD(id=c(1050, 386), res="",var="",per="")
##D # all zip files in a given path (if ID is empty):
##D head(  selectDWD(id="", res="daily", var="kl", per="recent")   )
##D 
## End(Not run)




cleanEx()
nameEx("updateIndexes")
### * updateIndexes

flush(stderr()); flush(stdout())

### Name: updateIndexes
### Title: update rdwd indexes
### Aliases: updateIndexes
### Keywords: data file internal

### ** Examples

# number of files at dwdbase
#  25'757 (2017-03-14) 
# 218'593 (2018-03-25)
# 228'830 (2018-11-26)
# 240'737 (2019-02-19)
# 242'584 (2019-03-11)
# 266'860 (2019-05-15)
# 254'446 (2019-05-30)
# 255'252 (2019-07-31)
# 254'925 (2019-09-17)
# 254'943 (2019-10-26)
 
# gridbase
#  49'247 (2019-05-26)
#  49'402 (2019-05-30)
#  54'314 (2019-07-31)
#  56'759 (2019-09-17)
#  58'656 (2019-10-26)




cleanEx()
nameEx("updateRdwd")
### * updateRdwd

flush(stderr()); flush(stdout())

### Name: updateRdwd
### Title: Update rdwd development version
### Aliases: updateRdwd
### Keywords: file

### ** Examples

# updateRdwd()




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
