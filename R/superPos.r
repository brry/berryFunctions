# Unit Hydrograph - superposition
# Berry Boessenkool   July 2013    berry-b@gmx.de

# simulate Q from P:
superPos <- function(
  P, # Precipitation
  UH) # discrete UnitHydrograph
{
added <- length(UH)-1
qsim <- rep(0, length(P)+added )
for(i in 1:length(P) ) qsim[i:(i+added)] <- P[i]*UH + qsim[i:(i+added)]
qsim
}

