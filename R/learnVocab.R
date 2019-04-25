#' @title spaced learning
#' @description spaced learning e.g. for vocabulary. Uses interactive questions.\cr
#' Note: this currently clears the console!\cr
#' Based on \url{https://ncase.me/remember} by Nicky Case.\cr
#' At the beginning, new vocab will be asked, skip with empty ENTER.
#' @return Updated vocab list, invisibly.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019
#' @keywords file
#' @importFrom utils read.csv2 write.table
#' @export
#' @examples
#' \dontrun{ # Excluded from checks, works only interactively! 
#' # initiate empty vocab list:
#' vocfile <- tempfile("myvocab",fileext=".csv")
#' cat("learning_day 1\n2019-04-19 17:43:47\nLEVEL;DE;FR\n", file=vocfile)
#' 
#' learnVocab(vocfile) # asks new vocab, then tests and changes level as needed
#' }
#'
#' @param vocfile File with vocabulary (or whatever you want to learn).
#'        The first two lines must contain the learning day and date, see examples.
#'        The third line must contain LEVEL;known;new, the last two being
#'        (short) names, e.g. languages (known will be displayed first).
#' @param minhours Minumal number of hours since the last test. If the time 
#'        difference is less, nothing happens. This enables putting learnVocab
#'        in \code{\link{Rprofile}} without being asked stuff all the time
#'        at every restart of R.
#'        DEFAULT: 3 hours
#' @param nnew Number of new entries to be added interactively at the start.
#'        They can still be skipped by writing nothing and pressing the ENTER key.
#'        DEFAULT: 3
learnVocab <- function(
 vocfile="C:/Dropbox/Sonstiges/Vokabeln.csv",
 minhours=3,
 nnew=3
)
{
# do nothing if last test is recent:
lastchange <- as.POSIXct(readLines(vocfile, n=2)[2], format="%F %T")
if(is.na(lastchange)) stop("Time stamp could not be read correctly.")
tdiff <- as.numeric(difftime(Sys.time(), lastchange, units="hours"))
if(tdiff<minhours) return(invisible(tdiff))

# do nothing if the user declines:
now <- readline("Want to do spaced learning now? y/n (ENTER=yes): ")
if(!removeSpace(now) %in% c("yes","y","")) return(invisible(tdiff))

# vocab list for column names:
voc <- read.csv2(vocfile, stringsAsFactors=FALSE, skip=2, nrows=1)
if(ncol(voc)!=3) stop("vocfile must have 3 columns but has ", ncol(voc))
if(colnames(voc)[1]!="LEVEL") stop("First column name must be 'LEVEL'.")
n1 <- colnames(voc)[2]
n2 <- colnames(voc)[3]

# new entries:
if(nnew>0) for(i in 1:nnew)
  {
  new <- readline(paste0("New entry ",i,"/",nnew," (",n1,";",n2,") ",": "))
  nsc <- nchar(new) - nchar(gsub(";", "", new))
  if(new=="") nsc <- 1 # to avoid warning
  if(nsc!=1) {warning("number of semicolons is ", nsc, " instead of 1.",
                      " Ignoring this entry.", call.=FALSE, immediate.=TRUE)
              new <- ""}
  if(new!="") cat(paste("1;", new, "\n"), file=vocfile, append=TRUE)
  }

# current vocab list:
voc <- read.csv2(vocfile, stringsAsFactors=FALSE, skip=2)

# Entries tested today:
day <- as.numeric(gsub("learning_day ", "", readLines(vocfile, n=1)))
levels_to_test <- which(c(
TRUE               , # 1: always
day %%  2 ==  1, # 2: test nr 1 3 5 7 9 11 ...
day %%  4 ==  2, # 3: 2 6 10 14 18 22 26 30 34 38 ...
day %%  8 ==  4, # 4: 4 13 20 29 36 45 52 61
day %% 16 == 12, # 5: 12 28 44 60
day %% 32 == 24, # 6: 24 59
day == 56     )) # 7: 56

for(r in sample(which(voc$LEVEL %in% levels_to_test))) # r: row number
{
cat("\014") # clear console
v1 <- paste0(n1,": ", removeSpace(voc[r,2]), " (ENTER -> Solution)")
known <- readline(v1)
cat("\014")
known <- readline(paste0(v1, ", ",n2,": ", removeSpace(voc[r,3]), ". Known? y/n (ENTER = yes): "))
known <- removeSpace(known) %in% c("yes","y","")
voc$LEVEL[r] <- if(known) voc$LEVEL[r] + 1   else   1
}

# write new output to file + return output:
if(day==64) day <- 0 # recycle from the start
cat("learning_day ", day+1, "\n", as.character(Sys.time()), "\n", 
    paste(colnames(voc), collapse=";"), "\n", file=vocfile, sep="")
write.table(voc, file=vocfile, row.names=FALSE, col.names=FALSE, quote=FALSE,
            append=TRUE, sep=";")
return(invisible(voc))
}
