
# Combine Textfiles regardless of their content.
# Berry Boessenkool, berry-b@gmx.de, Nov 2012, Dec 2014, Jul 2015

combineFiles <- function(
   inFiles = dir(),
   inDir = getwd(), 
   outFile = "combined_Textfiles.txt",
   outDir = inDir,
   sep = NULL,
   names=TRUE,
   selection=NULL,
   progbar=!quiet,
   quiet=FALSE)
{
# Function start
inFiles <- inFiles # execute before outFile is added
# Default sep:
if(is.null(sep)) sep <- "\n-------------------------------------------------------
-------------------------------------------------------\n"
# File to write to:
while( substring(outDir, nchar(outDir)) %in% c("/", "\\") ) #"
    outDir <- substring(outDir, 1, nchar(outDir)-1)
File <- paste(outDir, outFile, sep="/")
write("", file=File)
# Meta information if wanted
if(names)
 {
 write(paste(length(inFiles), "Files in", inDir), file=File, append=TRUE)
 write("Combined together with berryFunctions::combineFiles", file=File, append=TRUE)
 write(as.character(Sys.time()), file=File, append=TRUE)
 write(sep, file=File, append=TRUE)
 }
# Progressbar, if wanted:
if(progbar) pb <- txtProgressBar(max=length(inFiles), style=3)
# The actual action
for(i in 1:length(inFiles))
   {
   # Read file:
   inFile_i <- scan(file=paste(inDir, inFiles[i], sep="/"), what="char", 
                    blank.lines.skip=FALSE, sep="\n", quiet=TRUE)
   # Write filename if wanted:
   if(names) write(paste(inFiles[i], "\n"), file=File, append=TRUE)
   # selection of lines to write to output
   selection2 <- eval(substitute(selection), envir=environment())
   if(is.null(selection2)) selection2 <- 1:length(inFile_i)
   # write selection to output
   write(inFile_i[selection2], file=File, append=TRUE)
   # Write separation:
   write(sep, file=File, append=TRUE)
   # Update progres bar:
   if(progbar) setTxtProgressBar(pb, i)
   } # End of for-Loop
if(!quiet) message(i, " files combined to ", File)
if(progbar) close(pb)
} # End of function
