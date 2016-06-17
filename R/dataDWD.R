#' download and process data from DWD
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip dataset is downloaded into \code{dir}, unpacked, read, processed and returned. as a data.frame
#'
#' @return data.frame of the desired dataset, if download and processing were successfull. 
#'         Alternatively, links that were opened if \code{browse}!=0.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{download.file}}, \code{\link{monthAxis}}, \code{\link{climateGraph}}
#' @keywords data file
#' @importFrom utils download.file read.fwf read.table unzip write.table
#' @export
#' @examples
#' \dontrun{ ## Not run in CRAN checks because of downloading, writing files, etc
#' # Basic usage:
#' prec <- dataDWD(file="stundenwerte_RR_02787_akt.zip")
#' plot(prec$MESS_DATUM, prec$NIEDERSCHLAGSHOEHE, main="DWD hourly rain", col="blue",
#'      xaxt="n", las=1, type="l", xlab="Date", ylab="Hourly rainfall  [mm]")
#' monthAxis(1, ym=T)
#'
#' prec2 <- dataDWD("stundenwerte_RR_03987_akt.zip") # writes into the same folder
#' 
#' clim <- dataDWD(base2="monthly/kl/recent", file="monatswerte_03987_akt.zip")
#' # Potsdam monthly averages/mins/maxs of: wind, clouds, rainfall, sunshine, temperature
#'
#' # For several stations (do this at your own risk of getting banned)
#' # metadata for existing stations:
#' stats <- dataDWD("RR_Stundenwerte_Beschreibung_Stationen.txt")
#' str(stats)  # data.frame with 8 columns (4 int, 2 num, 2 factor), 1292 rows (July 2016)
#' head(stats)
#'
#' # List of actually available files (needs RCurl):
#' # install.packages("RCurl")
#' files <- dataDWD("", files=TRUE)
#' headtail(sort(files),6)
#'
#' # Apply the function to several files, create a list of data.frames:
#' # Exclude the pdf and txt files, or dataDWD will break
#' prec <- lapply(files[1:2], dataDWD)
#' names(prec) <- substr(files[1:2], 14, 21)
#' str(prec, max.level=1)
#' }
#'
#' @param file Filename (must be available at the location given by \code{base1} and \code{base2})
#' @param stats Return metadata for all stations instead of actual data?
#'              DEFAULT: TRUE if \code{file} extension = .txt
#' @param widths Column widths for \code{\link{read.fwf}} if \code{stats=TRUE}.
#'               Uses internal presets if "h" or "d" for hourly RR and daily KL. DEFAULT: "h"
#' @param files Return vector with all available files? DEFAULT: FALSE
#' @param browse Integer specifying whether and how to open repository via \code{\link{browseURL}}.
#'               0 for regular file download. 1 to open \code{base1}.
#'               2 to open \code{base1/base2}). If 1 or 2, no \code{dir} is created. DEFAULT: 0
#' @param base1 Main directory of DWD ftp server (can probably always be left unchanged)
#' @param base2 Characterstring with subdirectory. DEFAULT: "hourly/precipitation/recent"
#' @param dir Writeable directory on your computer. Created if not existent.
#'            DEFAULT: "precDWD" at your current \code{\link{getwd}()}
#' @param converttime Convert the column MESS_DATUM to POSIXct (if column is present)?
#'                    DEFAULT: TRUE if base2 is missing (because for daily values format must change)
#' @param format Format used in \code{\link{strptime}} if \code{converttime=TRUE}.
#'               DEFAULT: "\%Y\%m\%d\%H"
#' @param \dots Further arguments currently ignored
#'
dataDWD <- function(
file,
stats=substring(file, nchar(file)-3)==".txt",
widths="h",
files=FALSE,
browse=0,
base1="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
base2="hourly/precipitation/recent",
dir="precDWD",
converttime=missing(base2),
format="%Y%m%d%H",
...
)
{
# Open link in browser
if(browse==1)
  {
  browseURL(base1)
  return(base1)
  }
if(browse==2)
  {
  browseURL(paste0(base1,"/",base2))
  return(paste0(base1,"/",base2))
  }
#
# create directory to store original and processed data
if(!file.exists(dir))
  {
  dir.create(dir)
  message("Directory '", dir, "' created at '", getwd(),"'")
  }
owd <- setwd(dir)
on.exit(setwd(owd))
#
# input checks:
if(length(file)!=1) stop("file must be one character string")
if(widths[1]=="h") widths <- c( 6,9,9,15,12,10,41,100)
if(widths[1]=="d") widths <- c(11,9,9,15,12,10,42,100)
#
link <- paste0(base1,"/",base2,"/",file)
#
# station info:
if(stats)
  {
  # List of available stations: suppress readLines warning about EOL
  stats <- suppressWarnings(read.fwf(link, widths=widths,
                                     skip=2, strip.white=TRUE)              )  
  colnames(stats) <- strsplit(readLines(link, n=1, warn=FALSE), " ")[[1]]
  write.table(stats, file=file, row.names=FALSE, quote=FALSE, sep="\t")
  return(stats)
  }
# list available files
if(files)
  {
  if(!require("RCurl", quietly=TRUE)) stop("R package 'Rcurl' not available. dataDWD(files=TRUE) not possible.")
  f <- RCurl::getURL(paste0(base1,"/",base2,"/"), verbose=F, ftp.use.epsv=TRUE, dirlistonly=TRUE)
  f <- strsplit(f, "\r\n")[[1]]
  write.table(f, file=paste0("INDEX_of_DWD_", gsub("/","_",base2),".txt"), 
              row.names=FALSE, col.names=FALSE, quote=FALSE)
  return(f)
  }
#
# normal file download:
download.file(url=link, destfile=file, quiet=TRUE)
# unzip:
exdir <- substr(file, 1, nchar(file)-4)
unzip(file, exdir=exdir)
# read datafile:
dat <- read.table(dir(exdir, pattern="produkt*", full=TRUE),
                  na.strings=na9(), header=TRUE, sep=";", as.is=FALSE)
# process time-stamp:
if(converttime & "MESS_DATUM" %in% colnames(dat))
dat$MESS_DATUM <- as.POSIXct(strptime(dat$MESS_DATUM, format))
# write textfile for later reading:
write.table(dat, file=paste0(exdir, ".txt"))
# return dataset:
dat
}
