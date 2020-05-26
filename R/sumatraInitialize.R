#' @title Set useful Sumatra PDF Viewer default settings
#' @description 
#' Set useful Sumatra PDF Viewer default settings. This will likely only work on windows.
#' At the given \code{path} with "SumatraPDF.exe", this creates two settings files.
#' Existing files are renamed ("_old_n" appended), not overwritten.\cr
#' Creates "sumatrapdfrestrict.ini" with \code{SavePreferences = 1} and \code{FullscreenAccess = 1}.\cr
#' Creates "SumatraPDF-settings.txt" with \code{ShowToc = false} and \code{DefaultDisplayMode = single page}.
#' \code{UiLanguage} gets filled in by Sumatra itself upon first opening.
#' @return path, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2020
#' @seealso \code{\link{openPDF}}, \url{https://www.sumatrapdfreader.org/settings/settings.html}, 
#' \url{https://github.com/sumatrapdfreader/sumatrapdf/blob/master/docs/sumatrapdfrestrict.ini}
#' @keywords file
#' @export
#' @examples
#' # sumatraInitialize() # only run in interactive mode
#'
#' @param path  Folder (not file) that contains "SumatraPDF.exe". 
#'              Hints on automatic path detection would be welcome!
#'              DEFAULT: "C:/Program Files/RStudio/bin/sumatra"
#' @param openfolder Logical: Open folder after writing the files?
#'              Uses \code{\link{openFile}()}. DEFAULT: TRUE
#' 
sumatraInitialize <- function(
 path="C:/Program Files/RStudio/bin/sumatra",
 openfolder=TRUE
 )
{
# Checks
checkFile(path)
# if(.Platform$OS.type != "windows") stop("Platform must be windows") # maybe sumatra is avalable on unix
if(!interactive()) stop("sumatraInitialize can only be used in an interactive session.")
ok <- readline(paste0("Can I write files (no overwriting) at ", path, "? y/n: "))
if(!tolower(substr(ok,1,1))=="y") stop("You did not give write access.")

# Expand filenames
f1 <- file.path(path, "sumatrapdfrestrict.ini",  fsep="/")
f2 <- file.path(path, "SumatraPDF-settings.txt", fsep="/")

msg <- paste0("Created two SumatraPDF setting files.")

# Rename files if they exist
if(file.exists(f1))
  {
  fo1 <- newFilename(sub("restrict.ini$","restrict_old_1.ini",f1), quiet=TRUE)
  file.rename(f1, fo1)
  msg <- paste0(msg, "\nExisting file was renamed to ", fo1)
  }
if(file.exists(f2))
  {
  fo2 <- newFilename(sub("settings.txt$","settings_old_1.txt",f2), quiet=TRUE)
  file.rename(f2, fo2)
  msg <- paste0(msg, "\nExisting file was renamed to ", fo2)
  }

# Create new files

fn1 <- system.file("extdata/sumatrapdfrestrict.ini",  package="berryFunctions")
fn2 <- system.file("extdata/SumatraPDF-settings.txt", package="berryFunctions")
file.copy(fn1, f1)
file.copy(fn2, f2)

message(msg)
if(openfolder) openFile(path)

# Output:
return(invisible(path))
}
