# Unit Hydrograph - linear storage cascade
# Berry Boessenkool   July 2013    berry-b@gmx.de

# fit parameters, simulate discharge, and plot:
lsc <- function(P,
                Q,
                area=50,
                Qbase=Q[1],
                n=2,
                k=3,
                x=1:length(P),
                fit=1:length(Q),
                plot=TRUE,
                plotsim=TRUE,
                returnsim=FALSE,
                type=c("o", "l"),
                legx="center",
                legy=NULL,
                ...)
{
# checking for wrong input:
if(length(P) != length(Q)) stop("Vectors P and Q are not of equal length.")
if(any(fit>length(Q) | fit<0)) stop("'fit' has to contain positive integers smaller than length(Q).")
# initial parameters (few storages, quickly drained catchment):
param <- c(n=n, k=k)
# root mean square error (RMSE) of Qsim to Q ist to be minimized:
minfun <- function(param)
   { # discrete values of UH:
   UH_val <- unitHydrograph(n=param["n"], k=param["k"], t=1:length(P))
   qsim <- superPos(P/10^3, UH_val) * area*10^6 /3600 + Qbase
   berryFunctions::rmse( Q[fit], qsim[fit])
   }
# do the hard work:
optimized <- optim(par=param, fn=minfun, ...)$par
# calculate optimized UH:
finalUH <- unitHydrograph(optimized["n"], optimized["k"], t=1:length(P))
if(round(sum(finalUH), 1) !=1) warning("sum of UH is not 1, probably the time should be longer")
# simulate runoff:
Qsim <- superPos(P/10^3,  finalUH) * area*10^6 /3600 + Qbase
Qsim <- Qsim[1:length(Q)]
#
# runoff coefficient Psi:
# psi*P * A = Q * t
# psi = Qt / PA  # UNITS:  m^3/s * h * 3600s/h  / (mm=1/1000 m^3/m^2 * km^2)  /  1e6 m^2/km^2
psi <- mean(Q-Qbase) * length(Q) * 3600  /  ( sum(P)/1000 * area) / 1e6
if(psi>1) warning("Psi is larger than 1. The area given is not able to yield so much discharge. Consider the units in  ?lsc")
#
# graphic:
if(plot)
  {
  if(length(type)==1) type <- rep(type,2)
  op <- par(mar=rep(3,4))
  plot(x, P, type="h", col=4, yaxt="n", ylim=c(max(P)*2, 0), lwd=2, ann=FALSE)
  title(main="Precipitation and discharge" )
  axis(2, pretty(P), col=4, las=1, col.axis=4)
  #
  par(new=TRUE); plot(x, Q, type=type[1], col=2, las=1, ylim=range(Q)*c(1,2), ann=FALSE, axes=FALSE)
  axis(4, pretty(Q), col=2, las=1, col.axis=2)
  #
  mtext("P [mm]", line=-2, col=4, adj=0.02, outer=TRUE)
  mtext("Q [m^3/s]", line=-2, col=2, adj=0.98, outer=TRUE)
  #
  if(plotsim)
     {
     lines(x, Qsim, type=type[2], col=8, lwd=2)
     lines(x[fit], Qsim[fit], col=1, lwd=2)
     legend(legx, legy, legend=c("Observed","Simulated"), lwd=c(1,2), pch=c(1,NA), col=c(2,1) )
     }
  par(op)
  } # end if plot
#
if(returnsim) return(Qsim)
else return(c(n=as.vector(optimized["n"]), k=as.vector(optimized["k"]), NSE=berryFunctions::nse(Q, Qsim), psi=psi))
} # end of funtion



