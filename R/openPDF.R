#' @title open PDF file with sumatra viewer
#' @description open PDF file with SumatraPDF viewer, which does not lock files against being edited.
#' It is only available on windows, but comes bundled with Rstudio.
#' If the executable is not found, \code{\link{openFile}} is called instead.\cr
#' I suggest to first change some settings with \code{\link{sumatraInitialize}()}.
#' @return Result of try(system, ...), invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2020
#' @seealso \code{\link{openFile}} for the default opening programm\cr
#'          \code{\link{sumatraInitialize}} for nice Sumatra default settings\cr
#'          \code{\link{pdfpng}} to create PDFs and PNGs simultaneously.
#' @keywords file
#' @export
#' @examples
#' # only desired in an interactive session, not on CRAN checks
#' # openPDF( system.file("extdata/Anhang.pdf", package="berryFunctions")  )
#' # openPDF( system.file(c("extdata/Anhang.pdf", "extdata/RainfallStationsMap.pdf"), 
#' #                      package="berryFunctions")  )
#' 
#' @param file   Filename to be opened, as character string.
#'               Files not ending in ".pdf" are ignored with a warning.
#' @param rspath The path to Rstudio bin files. 
#'               DEFAULT: \code{\link{Sys.getenv}("RSTUDIO_PANDOC")}
#' @param sumexe The path to SumatraPDF.exe. DEFAULT: Null: extracted from \code{rspath}, e.g. 
#'               "C:/Program Files/RStudio/bin/sumatra/SumatraPDF.exe"
#' @param \dots  Further arguments passed to \code{\link{system}}
#' 
openPDF <- function(
file,
rspath=Sys.getenv("RSTUDIO_PANDOC"),
sumexe=NULL,
...
)
{
# 
rspath <- sub("pandoc$", "", rspath)
rspath <- sub("quarto/bin", "", rspath) # Rstudio dev version 2022-02-09
if(is.null(sumexe)) sumexe <- paste0(rspath,"/sumatra/SumatraPDF.exe")
 
# check file existence:
file   <- normalizePath(file,   winslash="/", mustWork=FALSE)
sumexe <- normalizePath(sumexe, winslash="/", mustWork=FALSE)
checkFile(file)

# check (and warn on windows) sumexe existence:
if(!file.exists(sumexe))
  {
  if(.Platform$OS.type == "windows") 
     warning("Sumatra exe '", sumexe, "' does not exist.\nOpening file with openFile() instead.", call.=FALSE)
  return(invisible(openFile(file)))
  }

# Ignore non-pdf-files:
pdf <- grepl(".pdf$", file)
if(any(!pdf))
  {
  warning("Ignoring non-pdf file", truncMessage(file[!pdf]))
  file <- file[pdf]
  }

if(length(file)<1) return("No files were left to open with SumatraPDF.exe")

# shQuote() to handle space in "C:/Program Files/R/..."
# https://stackoverflow.com/questions/62032586
sumafile <- paste(shQuote(c(sumexe, file)), collapse=" ")
out <- try(system(sumafile, wait=FALSE, ...),  silent=TRUE)
return(invisible(out))
}
