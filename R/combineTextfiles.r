
# Combine Textfiles regardless of their content.
# Berry Boessenkool, berry-b@gmx.de, Nov 2012

combineTextfiles <- function(
   inFiles = dir(),
   inDir = getwd(), 
   outFile = "combined_Textfiles.txt",
   outDir = inDir,
   sep = NULL,
   names=TRUE)
{ # Function start
# Default sep:
if(is.null(sep)) sep <- "\n-------------------------------------------------------
-------------------------------------------------------\n"
# File to write to:
File <- paste(outDir, outFile, sep="/")
write("", file=File)
# The actual action
for(i in 1:length(inFiles))
   {
   inFile_i <- scan(file=paste(inDir, inFiles[i], sep="/"), what="char", 
                    blank.lines.skip=FALSE, sep="\n", quiet=TRUE)
   if(names) write(paste(inFiles[i], "\n"), file=File, append=TRUE)
   write(inFile_i, file=File, append=TRUE)
   write(sep, file=File, append=TRUE)
   } # End of for-Loop
cat(i, "files combined to", File, "\n")
} # End of function
