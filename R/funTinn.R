# Berry Boessenkool, Aug 2014
# Open Function in Tinn R editor with an R command

# http://stackoverflow.com/questions/13873528

funTinn <- function(
name#, # name of function or object to be opened with the editor Tinn-R
#path="C:/Program Files/Tinn-R/bin/Tinn-R.exe" # path to editor.
)
{
File <- paste0(tempdir(), "/", deparse(substitute(name)), ".r")
sink(File)
print(name)
sink()
#dummy <- edit(name, editor=path)
# Open the file with the program associated with its file extension
system2("open", File)
}
