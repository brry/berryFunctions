#' download and process data from DWD
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip dataset is downloaded into \code{dir}, unpacked, read, processed and returned. as a data.frame
#'
#' @return data.frame of the desired dataset, if download and processing were successfull
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{download.file}}, \code{\link{monthAxis}}, \code{\link{climateGraph}}
#' @keywords data file
#' @importFrom utils download.file read.table unzip write.table
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
#' clim <- dataDWD(base2="climate/monthly/kl/recent", file="monatswerte_03987_akt.zip")
#' # Potsdam monthly averages/mins/maxs of: wind, clouds, rainfall, sunshine, temperature
#'
#' # For several stations (do this at your own risk):
#' link_base <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/",
#'                     "climate/hourly/precipitation/recent/")
#' # List of existing stations:
#' link_stats <- paste0(link_base, "/RR_Stundenwerte_Beschreibung_Stationen.txt")
#' stats <- read.fwf(link_stats, widths=c(6,9,9,15,12,10,41,100), skip=2, strip.white=TRUE)
#' colnames(stats) <- strsplit(readLines(link_stats, n=1), " ")[[1]]
#' str(stats)  # data.frame with 8 columns (4 int, 2 num, 2 factor), 1292 rows (July 2016)
#' head(stats)
#'
#' # List of actually available files:
#' # install.packages("RCurl")
#' files <- RCurl::getURL(link_base, verbose=FALSE, ftp.use.epsv=TRUE, dirlistonly=TRUE)
#' files <- strsplit(files, "\r\n")[[1]]
#' headtail(sort(files),6)
#'
#' # Apply the function to several files, create a list of data.frames:
#' prec <- lapply(files[1:2], dataDWD)
#' names(prec) <- substr(files[1:2], 14, 21)
#' str(prec, max.level=1)
#' }
#'
#' @param file Filename (must be available at the location given by \code{base1} and \code{base2})
#' @param base1 Main directory of DWD ftp server (can probably always be left unchanged)
#' @param base2 Characterstring with subdirectory. DEFAULT: "climate/hourly/precipitation/recent"
#' @param dir Writeable directory on your computer. Created if not existent.
#'            DEFAULT: "precDWD" at your current \code{\link{getwd}()}
#' @param converttime Convert the column MESS_DATUM to POSIXct? DEFAULT: TRUE
#' @param format Format for \code{\link{strptime}}. DEFAULT: "\%Y\%m\%d\%H"
#' @param \dots Further arguments currently ignored
#'
dataDWD <- function(
file,
base1="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany",
base2="climate/hourly/precipitation/recent",
dir="precDWD",
converttime=TRUE,
format="%Y%m%d%H",
...
)
{
# create directory to store original and processed data
if(!file.exists(dir))
  {
  dir.create(dir)
  message("Directory '", dir, "' created at '", getwd(),"'")
  }
owd <- setwd(dir)
on.exit(setwd(owd))
# input checks:
if(length(file)!=1) stop("file must be one character string")
# download: 
download.file(url=paste0(base1,"/",base2,"/",file), destfile=file, quiet=TRUE)
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
