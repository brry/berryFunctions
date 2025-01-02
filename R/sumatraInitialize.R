#' @title Set useful Sumatra PDF Viewer default settings
#' @description 
#' Set useful Sumatra PDF Viewer default settings. This will only work on windows.
#' Existing files are renamed ("_old_n" appended), not overwritten.\cr
#' At the given \code{path} with "SumatraPDF.exe", it creates "sumatrapdfrestrict.ini" with \code{SavePreferences = 1} and \code{FullscreenAccess = 1}.\cr
#' At the given \code{roampath}, it creates "SumatraPDF-settings.txt" with \code{ShowToc = false} and \code{DefaultDisplayMode = single page}.
#' \code{UiLanguage} gets filled in by Sumatra itself upon first opening.
#' @return path, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2020, Nov 2023
#' @seealso \code{\link{openPDF}}\cr
#' \url{https://www.sumatrapdfreader.org/settings/settings.html}\cr
#' \url{https://github.com/sumatrapdfreader/sumatrapdf/blob/master/docs/sumatrapdfrestrict.ini}
#' @keywords file
#' @export
#' @examples
#' # sumatraInitialize() # only run in interactive mode
#'
#' @param path  Folder (not file) that contains "SumatraPDF.exe".
#'              You need file writing permissions in the folder.
#'              DEFAULT: equivalent of "C:/Program Files/RStudio/resources/app/bin/sumatra"
#' @param roampath Folder that will contain "SumatraPDF-settings.txt".
#'              DEFAULT: equivalent of "C:/Users/berry/AppData/Roaming/SumatraPDF"
#' @param openfolder Logical: Open folders after writing the files?
#'              Uses \code{\link{openFile}()}. DEFAULT: TRUE
#' 
sumatraInitialize <- function(
 path=sumatraPaths()[1],
 roampath=sumatraPaths()[2],
 openfolder=TRUE
 )
{
# Checks
if(.Platform$OS.type != "windows") stop("SumatraPDF is only available on Windows") 
if(!interactive()) stop("sumatraInitialize can only be used in an interactive session.")
checkFile(path)
if(!dir.exists(roampath)) dir.create(roampath)
checkFile(roampath)
message("sumatraInitialize wants to add config files at these two locations:\n",
        path, "\n", roampath,"\nIf files exist, they are kept as a copy.")
ok <- readline("Can I write files as in the message? y/n: ")
if(!tolower(substr(ok,1,1))=="y") stop("You did not give write access.")

# Expand filenames
f1 <- file.path(path, "sumatrapdfrestrict",  fsep="/") # .ini
f2 <- file.path(roampath, "SumatraPDF-settings", fsep="/") # .txt

msg <- paste0("Created two SumatraPDF setting files.")

# Rename files if they exist
replaceFile <- function(base, ext)
  {
  fn <- paste0(base,ext)
  if(!file.exists(fn)) return(fn)
  #
  newname <- newFilename(paste0(base, "_old_1",ext), quiet=TRUE)
  if(!requireNamespace("R.utils", quietly=TRUE)) stop("install R.utils")
  if(R.utils::fileAccess(dirname(newname), mode=2)<0)
   stop("First set writing access for  ", dirname(fn))
  file.rename(fn, newname)
  msg <<- paste0(msg, "\nExisting file was renamed to ", newname)
  return(fn)
  }

f1 <- replaceFile(f1, ".ini")
f2 <- replaceFile(f2, ".txt")

# Create new files
fn1 <- system.file("extdata/sumatrapdfrestrict.ini",  package="berryFunctions")
fn2 <- system.file("extdata/SumatraPDF-settings.txt", package="berryFunctions")
file.copy(fn1, f1)
file.copy(fn2, f2)

message(msg)
if(openfolder) 
 {
 openFile(path)
 openFile(roampath)
 }

# Output:
return(invisible(c(path, roampath)))
}
