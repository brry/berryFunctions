# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Weather Data Germany download with R, Climate Data Germany
# Deutscher Wetterdienst R Daten download Klimastationen

# For html rendered documentation, please visit
#   https://www.rdocumentation.org/packages/berryFunctions/versions/1.12.3/topics/dataDWD
# 
#' download data from DWD
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip dataset is downloaded into \code{dir}, read, processed and returned as a data.frame
#' 
#' @note These functions are now in the package rdwd and will be removed here!
#' \url{https://github.com/brry/rdwd}
#'
#' @return data.frame of the desired dataset (returned by \code{\link{readDWD}} if meta=0), 
#'         presuming downloading and processing were successfull.
#'         Alternatively, links that were opened if \code{browse}!=0.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{readDWD}}, \code{\link{download.file}}, \code{\link{monthAxis}}, \code{\link{climateGraph}}
#' @keywords data file
#' @importFrom utils download.file read.fwf read.table unzip write.table
#' @export
#' @examples
#' 
#' # see 
#' bowseURL("https://github.com/brry/rdwd#rdwd")
#' 
#'
#' @param file Filename (must be available at the location given by \code{base1} and \code{base2})
#' @param base1 Main directory of DWD ftp server (can probably always be left unchanged)
#' @param base2 Characterstring with subdirectory. DEFAULT: "hourly/precipitation/recent"
#' @param dir Writeable directory on your computer. Created if not existent.
#'            DEFAULT: "DWDdata" at your current \code{\link{getwd}()}
#' @param browse Integer specifying whether and how to open repository via \code{\link{browseURL}}.\cr
#'               0 for regular file download. \cr
#'               1 to open \code{base1}.\cr
#'               2 to open \code{base1/base2}).\cr
#'               If base= 1 or 2, no \code{dir} is created and no download performed. DEFAULT: 0
#' @param meta Integer specifying whether to get metadata instead of actual data.\cr
#'               0 for regular file. \cr
#'               1 for meta data of all stations
#'               (\code{meta} is automatically set to 1 if \code{file} ends in ".txt".
#'               Column widths for \code{\link{read.fwf}} are computed internally).\cr
#'               2 for a list of the available files (requires \code{RCurl} to be installed.
#'               If meta=2, \code{file=""} is possible, as it is ignored anyways).\cr
#'               DEFAULT: 0
#' @param read Read the file with \code{\link{readDWD}}?
#'             If FALSE, only download is performed. DEFAULT: TRUE
#' @param format Format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#' @param \dots Further arguments currently ignored
#'
dataDWD <- function(
file,
base1="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
base2="hourly/precipitation/recent",
dir="DWDdata",
browse=0:2,
meta=0:2,
read=TRUE,
format=NA,
quiet=FALSE,
...
)
{
warning("dataDWD will be removed from berryFunctions. Please use the package rdwd")
browse <- browse[1]
meta <- meta[1]
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
  if(!quiet) message("Directory '", dir, "' created at '", getwd(),"'")
  } else
  if(!quiet) message("Adding to directory '", dir, "' at '", getwd(),"'")
owd <- setwd(dir)
on.exit(setwd(owd))
#
# input checks:
if(length(file)!=1) stop("file must be a single character string")
if(substring(file, nchar(file)-3)==".txt") meta <- 1
#
link <- paste0(base1,"/",base2,"/",file)
#
# station info:
if(meta==1)
  {
  # one line to confirm widths and get column names
  widths <- c( 6,9,9,15,12,10,41,100)
  if(file=="") stop("dataDWD argument 'file' may not be empty if meta=1")
  oneline <- readLines(link, n=3)
  if(substr(oneline[3],1,6)=="      ") widths[1] <- 11
  # read metadata: suppress readLines warning about EOL
  stats <- suppressWarnings(read.fwf(link, widths=widths, skip=2, strip.white=TRUE) )
  colnames(stats) <- strsplit(oneline[1], " ")[[1]]
  if(file.exists(file)) warning("File '",file,"' already existed, is now overwritten.")
  write.table(stats, file=file, row.names=FALSE, quote=FALSE, sep="\t")
  return(stats)
  }
# list available files
if(meta==2)
  {
  if(!requireNamespace("RCurl", quietly=TRUE)) 
    stop("The R package 'Rcurl' is not available. dataDWD(..., meta=2) is not possible.
         install.packages('RCurl')    to enable this.")
  f <- RCurl::getURL(paste0(base1,"/",base2,"/"), verbose=F, ftp.use.epsv=TRUE, dirlistonly=TRUE)
  f <- strsplit(f, "\r\n")[[1]]
  file <- paste0("INDEX_of_DWD_", gsub("/","_",base2),".txt")
  if(file.exists(file)) warning("File '",file,"' already existed, is now overwritten.")
  write.table(f, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
  return(f)
  }
#
# Regular file download:
download.file(url=link, destfile=file, quiet=TRUE)
if(read) readDWD(file=file, format=format)
}
