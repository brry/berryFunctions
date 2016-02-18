# nameSample:
# Find the seed necessary to produce a character sequence by using sample
# can especially impress newbies a lot.

# Berry Boessenkool, berry-b@gmx.de, 15 April 2014


nameSample <- function(
name,
progress=FALSE,
estimatetime=nc>4, # estimate computation time
continue=FALSE # continue without asking
)
{
nc <- nchar(name)
name <- tolower(name)
if(any(!strsplit(name,"")[[1]] %in% letters)) stop("'", name, "' contains characters not available in letters.")
if(nc>3) {message("function may take a bit of time."); flush.console() }
# input length checking:
if(estimatetime)
  {
  estcalctime <- 10^(1.36*nc-4.66)/3600
  unit <- " hours"
  if(estcalctime <1) { estcalctime <- estcalctime*60 ; unit=" minutes"}
  answer <- readline(paste0("Estimated time: ", signif(estcalctime, 2), unit,
  ". Do you want a more exact estimate (takes 5-20 s)? y/n : "))
  if(answer == "y")
    {
    randomwords <- function(n) paste(sample(letters,n), collapse="")
    let <- sapply(rep(4:2, c(3,15,50)), randomwords)
    if(require(pbapply, quietly=TRUE)) sapply <- pbapply::pbsapply
    suppressMessages(calctime <- sapply(let, function(x) system.time(nameSample(x))[3]))
    nchar_name <- nchar(names(calctime))-8
    estcalctime <- expReg(nchar_name, calctime, xlim=c(1, nc),
                    ylim=c(-3, 1.36*nc-4.66), ylab="calculation time seconds", predictnew=nc)/3600
    unit <- " hours"
    if(estcalctime[1] <1) { estcalctime <- estcalctime*60 ; unit=" minutes"}
    if(estcalctime[1] <1) { estcalctime <- estcalctime*60 ; unit=" seconds"}
    }
  if(continue) answer <- "y" else
     answer <- readline(paste0("Estimated time: ", signif(estcalctime[2], 2), " to ",
          signif(estcalctime[3], 2), unit, ". Do you want to continue? y/n : "))
  if(answer == "n") stop("Function cancelled by user.")
  }
#
# The actual work:
anf <- Sys.time()
seed_is_false <- function(i)
  {
  set.seed(i)
  if(progress) if(i%%10000==0) {cat("."); flush.console()}
  if(progress) if(i%%10000==0)  if((i/10000)%%options()[["width"]]==0) cat("\n")
  paste( sample(letters, nc, replace=TRUE), collapse="") != name
  }
i <- 1
while( seed_is_false(i) ) i <- i+1
output <- paste0("set.seed(", i, "); paste(sample(letters,", nc,
                 ",T), collapse='')\n")
message(if(progress) "\n", "Computation time was: ", round(difftime(Sys.time(),anf, units="secs")/60,2), " minutes.")
message(output)
return(invisible(output))
}
