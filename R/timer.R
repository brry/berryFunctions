# Timer alarm
# Berry Boessenkool, berry-b@gmx.de, 2015-06-20

# with defaults for useR lightning talks: 15 slides, each shown 20 secs, change automatically

timer <- function(
interval=20, # \code{\link{alarm}} interval in seconds
n=15, # number of alarm signals to be given
write=FALSE # Should the actual estimated time be written for overhead computing time control purposes?
)
{
begin <- Sys.time()
pb <- txtProgressBar(max=n, style=3)
for(i in 1:n)
  {
  Sys.sleep(interval)
  setTxtProgressBar(pb, i)
  alarm()
  }
close(pb)
time_used <- round(difftime(Sys.time(), begin, units="secs"), 2)
if(write) message("Actual time passed by: ", time_used,
                  " secs. Deviance from target: ", round(time_used-interval*n, 2),
                  " (", round((time_used-interval*n)/(interval*n)*100, 2), "%).")
}

# Examples:
# timer(interval=1, n=3)
# timer(interval=0.2, n=15, write=TRUE)
# timer() # to practice lightning talks at useR! conferences
