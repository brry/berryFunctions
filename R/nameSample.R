# nameSample:
# Find the seed necessary to produce a character sequence by using sample
# can especially impress newbies a lot.

# Berry Boessenkool, berry-b@gmx.de, 15 April 2014


nameSample <- function(
                      name,
                      progress=nc>3
                      )
{
nc <- nchar(name)
name <- tolower(name)
# input length checking:
if(nc>3)
  {
  estcalctime <- 10^(1.36*nc-4.66)/3600
  unit <- " hours"
  if(estcalctime <1) { estcalctime <- estcalctime*60 ; unit=" minutes"}
  answer <- readline(paste0("Estimated time: ", signif(estcalctime, 2), unit,
  ". Do you want a more exact estimate (takes 5-10 s)? y/n : "))
  if(answer == "y")
  {
  let <- sapply(1:3, function(n) apply(replicate(n, letters[sample(10)]), 1, paste, collapse=""))
  suppressMessages(calctime <- sapply(let, function(x) system.time(nameSample(x, progress=F))[3]))
  nchar_name <- nchar(names(calctime))-8
  estcalctime <- expReg(nchar_name, calctime, xlim=c(1, nc),
                  ylim=c(-3, 1.36*nc-4.66), predictnew=nc)/3600
  unit <- " hours"
  if(estcalctime[1] <1) { estcalctime <- estcalctime*60 ; unit=" minutes"}
  if(estcalctime[1] <1) { estcalctime <- estcalctime*60 ; unit=" seconds"}
  }
  answer <- readline(paste0("Estimated time: ", signif(estcalctime[2], 2), " to ",
          signif(estcalctime[3], 2), unit, ". Do you want to continue? y/n : "))
  if(answer == "n") stop("Function cancelled by user.")
  }
#
# The actual ork:
seed_is_false <- function(i)
  {
  set.seed(i)
  if(progress) if(i%%10000==0) {cat("."); flush.console()}
  if(progress) if(i%%10000==0)  if(i/10000%%options()[["width"]]==0) cat("\n")
  paste( sample(letters, nc, replace=TRUE), collapse="") != name
  }
i <- 1
while( seed_is_false(i) ) i <- i+1
output <- paste0("set.seed(", i, "); paste(sample(letters,", nc,
                 ",T), collapse='')\n")
message(if(progress) "\n", output)
}
