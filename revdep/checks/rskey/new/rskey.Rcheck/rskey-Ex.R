pkgname <- "rskey"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rskey')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addins")
### * addins

flush(stderr()); flush(stdout())

### Name: addins
### Title: Rstudio keyboard shortcuts on F-keys
### Aliases: addins str_addin head_addin tail_addin View_addin
###   funSource_addin summary_addin dim_addin class_addin plot_addin
###   hist_addin

### ** Examples

# Go to Addins - browse Addins - Keyboard shortcuts - map commands as desired
# or use    setKeyboardBindings()

# highlight objects or code (examples below), then press keyboard shortcut
iris
iris$Sepal.Length + 10




cleanEx()
nameEx("selectobject")
### * selectobject

flush(stderr()); flush(stdout())

### Name: selectobject
### Title: Select object
### Aliases: selectobject

### ** Examples

# see str_addin




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
