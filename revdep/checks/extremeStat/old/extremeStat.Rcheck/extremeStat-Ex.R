pkgname <- "extremeStat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('extremeStat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("annMax")
### * annMax

flush(stderr()); flush(stdout())

### Name: annMax
### Title: annual discharge maxima (streamflow)
### Aliases: annMax
### Keywords: datasets

### ** Examples


data(annMax)
str(annMax)
str(annMax)
plot(1976:2010, annMax, type="l", las=1, main="annMax dataset from Austria")
# Moving Average with different window widths:
berryFunctions::movAvLines(annMax, x=1976:2010, lwd=3, alpha=0.7)




cleanEx()
nameEx("distLexBoot")
### * distLexBoot

flush(stderr()); flush(stdout())

### Name: distLexBoot
### Title: Bootstrapping uncertainty intervals for return periods
### Aliases: distLexBoot
### Keywords: bootstrap distribution dplot hplot montecarlo ts

### ** Examples


data(annMax)
dlf <- distLextreme(annMax, selection=c("wak","gum","gev","nor"))
dlfB <- distLexBoot(dlf, nbest=4, conf.lev=0.5, n=10) # n low for quick example tests
plotLexBoot(dlfB)




cleanEx()
nameEx("distLextreme")
### * distLextreme

flush(stderr()); flush(stdout())

### Name: distLextreme
### Title: Extreme value stats
### Aliases: distLextreme
### Keywords: distribution dplot hplot ts

### ** Examples


# Basic examples
# BM vs POT
# Plotting options
# weighted mean based on Goodness of fit (GOF)
# Effect of data proportion used to estimate GOF
# compare extremeStat with other packages

library(lmomco)
library(berryFunctions)

data(annMax) # annual streamflow maxima in river in Austria

# Basic examples ---------------------------------------------------------------
dlf <- distLextreme(annMax)
plotLextreme(dlf, log=TRUE)

# Object structure:
str(dlf, max.lev=2)
printL(dlf)

# discharge levels for default return periods:
dlf$returnlev

# Estimate discharge that could occur every 80 years (at least empirically):
Q80 <- distLextreme(dlf=dlf, RPs=80)$returnlev
round(sort(Q80[1:17,1]),1)
# 99 to 143 m^3/s can make a relevant difference in engineering!
# That's why the rows weighted by GOF are helpful. Weights are given as in
plotLweights(dlf) # See also section weighted mean below
# For confidence intervals see ?distLexBoot

# Return period of a given discharge value, say 120 m^3/s:
round0(sort(1/(1-sapply(dlf$parameter, plmomco, x=120) )  ),1)
# exponential:                 every 29 years
# gev (general extreme value dist):  59,
# Weibull:                     every 73 years only


# BM vs POT --------------------------------------------------------------------
# Return levels by Block Maxima approach vs Peak Over Threshold approach:
# BM distribution theoretically converges to GEV, POT to GPD

data(rain, package="ismev")
days <- seq(as.Date("1914-01-01"), as.Date("1961-12-30"), by="days")
BM <- tapply(rain, format(days,"%Y"), max)  ;  rm(days)
dlfBM <- plotLextreme(distLextreme(BM, emp=FALSE), ylim=lim0(100), log=TRUE, nbest=10)
plotLexBoot(distLexBoot(dlfBM, quiet=TRUE), ylim=lim0(100))
plotLextreme(dlfBM, log=TRUE, ylim=lim0(100))

dlfPOT99 <- distLextreme(rain, npy=365.24, trunc=0.99, emp=FALSE)
dlfPOT99 <- plotLextreme(dlfPOT99, ylim=lim0(100), log=TRUE, nbest=10, main="POT 99")
printL(dlfPOT99)

# using only nonzero values (normally yields better fits, but not here)
rainnz <- rain[rain>0]
dlfPOT99nz <- distLextreme(rainnz, npy=length(rainnz)/48, trunc=0.99, emp=FALSE)
dlfPOT99nz <- plotLextreme(dlfPOT99nz, ylim=lim0(100), log=TRUE, nbest=10,
                           main=paste("POT 99 x>0, npy =", round(dlfPOT99nz$npy,2)))

## Not run: 
##D  ## Excluded from CRAN R CMD check because of computing time
##D 
##D dlfPOT99boot <- distLexBoot(dlfPOT99, prop=0.4)
##D printL(dlfPOT99boot)
##D plotLexBoot(dlfPOT99boot)
##D 
##D 
##D dlfPOT90 <- distLextreme(rain, npy=365.24, trunc=0.90, emp=FALSE)
##D dlfPOT90 <- plotLextreme(dlfPOT90, ylim=lim0(100), log=TRUE, nbest=10, main="POT 90")
##D 
##D dlfPOT50 <- distLextreme(rain, npy=365.24, trunc=0.50, emp=FALSE)
##D dlfPOT50 <- plotLextreme(dlfPOT50, ylim=lim0(100), log=TRUE, nbest=10, main="POT 50")
## End(Not run)

ig99 <- ismev::gpd.fit(rain, dlfPOT99$threshold)
ismev::gpd.diag(ig99); title(main=paste(99, ig99$threshold))
## Not run: 
##D ig90 <- ismev::gpd.fit(rain, dlfPOT90$threshold)
##D ismev::gpd.diag(ig90); title(main=paste(90, ig90$threshold))
##D ig50 <- ismev::gpd.fit(rain, dlfPOT50$threshold)
##D ismev::gpd.diag(ig50); title(main=paste(50, ig50$threshold))
## End(Not run)


# Plotting options -------------------------------------------------------------
plotLextreme(dlf=dlf)
# Line colors / select distributions to be plotted:
plotLextreme(dlf, nbest=17, distcols=heat.colors(17), lty=1:5) # lty is recycled
plotLextreme(dlf, selection=c("gev", "gam", "gum"), distcols=4:6, PPcol=3, lty=3:2)
plotLextreme(dlf, selection=c("gpa","glo","wei","exp"), pch=c(NA,NA,6,8),
                 order=TRUE, cex=c(1,0.6, 1,1), log=TRUE, PPpch=c(16,NA), n_pch=20)
# use n_pch to say how many points are drawn per line (important for linear axis)

plotLextreme(dlf, legarg=list(cex=0.5, x="bottom", box.col="red", col=3))
# col in legarg list is (correctly) ignored
## Not run: 
##D ## Excluded from package R CMD check because it's time consuming
##D 
##D plotLextreme(dlf, PPpch=c(1,NA)) # only Weibull plotting positions
##D # add different dataset to existing plot:
##D distLextreme(Nile/15, add=TRUE, PPpch=NA, distcols=1, selection="wak", legend=FALSE)
##D 
##D # Logarithmic axis
##D plotLextreme(distLextreme(Nile), log=TRUE, nbest=8)
##D 
##D 
##D 
##D # weighted mean based on Goodness of fit (GOF) ---------------------------------
##D # Add discharge weighted average estimate continuously:
##D plotLextreme(dlf, nbest=17, legend=FALSE)
##D abline(h=115.6, v=50)
##D RP <- seq(1, 70, len=100)
##D DischargeEstimate <- distLextreme(dlf=dlf, RPs=RP, plot=FALSE)$returnlev
##D lines(RP, DischargeEstimate["weighted2",], lwd=3, col="orange")
##D 
##D # Or, on log scale:
##D plotLextreme(dlf, nbest=17, legend=FALSE, log=TRUE)
##D abline(h=115.9, v=50)
##D RP <- unique(round(logSpaced(min=1, max=70, n=200, plot=FALSE),2))
##D DischargeEstimate <- distLextreme(dlf=dlf, RPs=RP)$returnlev
##D lines(RP, DischargeEstimate["weighted2",], lwd=5)
##D 
##D 
##D # Minima -----------------------------------------------------------------------
##D 
##D browseURL("http://nrfa.ceh.ac.uk/data/station/meanflow/39072")
##D qfile <- system.file("extdata/discharge39072.csv", package="berryFunctions")
##D Q <- read.table(qfile, skip=19, header=TRUE, sep=",", fill=TRUE)[,1:2]
##D rm(qfile)
##D colnames(Q) <- c("date","discharge")
##D Q$date <- as.Date(Q$date)
##D plot(Q, type="l")
##D Qmax <- tapply(Q$discharge, format(Q$date,"%Y"), max)
##D plotLextreme(distLextreme(Qmax, quiet=TRUE))
##D Qmin <- tapply(Q$discharge, format(Q$date,"%Y"), min)
##D dlf <- distLextreme(-Qmin, quiet=TRUE, RPs=c(2,5,10,20,50,100,200,500))
##D plotLextreme(dlf, ylim=c(0,-31), yaxs="i", yaxt="n", ylab="Q annual minimum", nbest=14)
##D axis(2, -(0:3*10), 0:3*10, las=1)
##D -dlf$returnlev[c(1:14,21), ]
##D # Some distribution functions are an obvious bad choice for this, so I use
##D # weighted 3: Values weighted by GOF of dist only for the best half.
##D # For the Thames in Windsor, we will likely always have > 9 m^3/s streamflow
##D 
##D 
##D # compare extremeStat with other packages: ---------------------------------------
##D library(extRemes)
##D plot(fevd(annMax))
##D par(mfrow=c(1,1))
##D return.level(fevd(annMax, type="GEV")) # "GP", "PP", "Gumbel", "Exponential"
##D distLextreme(dlf=dlf, RPs=c(2,20,100))$returnlev["gev",]
##D # differences are small, but noticeable...
##D # if you have time for a more thorough control, please pass me the results!
##D 
##D 
##D # yet another dataset for testing purposes:
##D Dresden_AnnualMax <- c(403, 468, 497, 539, 542, 634, 662, 765, 834, 847, 851, 873,
##D 885, 983, 996, 1020, 1028, 1090, 1096, 1110, 1173, 1180, 1180,
##D 1220, 1270, 1285, 1329, 1360, 1360, 1387, 1401, 1410, 1410, 1456,
##D 1556, 1580, 1610, 1630, 1680, 1734, 1740, 1748, 1780, 1800, 1820,
##D 1896, 1962, 2000, 2010, 2238, 2270, 2860, 4500)
##D plotLextreme(distLextreme(Dresden_AnnualMax))
## End(Not run) # end dontrun




cleanEx()
nameEx("distLfit")
### * distLfit

flush(stderr()); flush(stdout())

### Name: distLfit
### Title: Fit distributions via L-moments
### Aliases: distLfit
### Keywords: distribution dplot hplot univar

### ** Examples


data(annMax)
# basic usage on real data (annual discharge maxima in Austria)
dlf <- distLfit(annMax)
str(dlf, max.lev=2)
printL(dlf)
plotLfit(dlf)

# arguments that can be passed to plotting function:
plotLfit(dlf, lty=2, col=3, nbest=17, legargs=list(lwd=3), main="booh!")
set.seed(42)
dlf_b <- distLfit(rbeta(100, 5, 2))
plotLfit(dlf_b, nbest=10, legargs=c(x="left"))
plotLfit(dlf_b, selection=c("gpa", "glo", "gev", "wak"))
plotLfit(dlf_b, selection=c("gpa", "glo", "gev", "wak"), order=TRUE)
plotLfit(dlf_b, distcols=c("orange",3:6), lty=1:3) # lty is recycled
plotLfit(dlf_b, cdf=TRUE)
plotLfit(dlf_b, cdf=TRUE, histargs=list(do.points=FALSE), sel="nor")


# logarithmic axes:
set.seed(1)
y <- 10^rnorm(300, mean=2, sd=0.3) # if you use 1e4, distLfit will be much slower
hist(y, breaks=20)
berryFunctions::logHist(y, col=8)
dlf <- distLfit(log10(y))
plotLfit(dlf, breaks=50)
plotLfit(dlf, breaks=50, log=TRUE)


# Goodness of fit: how well do the distributions fit the original data?
# measured by RMSE of cumulated distribution function and ?ecdf
# RMSE: root of average of ( errors squared )  ,   errors = line distances
dlf <- distLfit(annMax, ks=TRUE)
plotLfit(dlf, cdf=TRUE, sel=c("wak", "revgum"))
x <- sort(annMax)
segments(x0=x, y0=lmomco::plmomco(x, dlf$parameter$revgum), y1=ecdf(annMax)(x), col=2)
segments(x0=x, y0=lmomco::plmomco(x, dlf$parameter$wak), y1=ecdf(annMax)(x), col=4, lwd=2)
# weights by three different weighting schemes, see distLweights:
plotLweights(dlf)
plotLfit(distLfit(annMax              ), cdf=TRUE, nbest=17)$gof
plotLfit(distLfit(annMax, truncate=0.7), cdf=TRUE, nbest=17)$gof
pairs(dlf$gof[,-(2:5)]) # measures of goodness of fit are correlated quite well here.
dlf$gof

# Kolmogorov-Smirnov Tests for normal distribution return slightly different values:
library(lmomco)
ks.test(annMax, "pnorm", mean(annMax), sd(annMax) )$p.value
ks.test(annMax, "cdfnor", parnor(lmoms(annMax)))$p.value


# Fit all available distributions (30):
## Not run: 
##D # this takes a while...
##D d_all <- distLfit(annMax, speed=FALSE, progbars=TRUE) # 20 sec
##D printL(d_all)
##D plotLfit(d_all, nbest=30, distcols=grey(1:22/29), xlim=c(20,140))
##D plotLfit(d_all, nbest=30, ylim=c(0,0.04), xlim=c(20,140))
##D plotLweights(d_all)
##D d_all$gof
## End(Not run)




cleanEx()
nameEx("distLquantile")
### * distLquantile

flush(stderr()); flush(stdout())

### Name: distLquantile
### Title: distribution quantiles
### Aliases: distLquantile
### Keywords: distribution robust univar

### ** Examples


data(annMax) # Annual Discharge Maxima (streamflow)

distLquantile(annMax, emp=FALSE)[,] # several distribution functions in lmomco
distLquantile(annMax, truncate=0.8, probs=0.95)[,] # POT (annMax already block maxima)
dlf <- distLquantile(annMax, probs=0.95, list=TRUE)
plotLquantile(dlf, linargs=list(lwd=3), nbest=5, breaks=10)
dlf$quant
# Parametric 95% quantile estimates range from 92 to 111!
# But the best fitting distributions all lie aroud 103.

# compare General Pareto Fitting methods
# Theoretically, the tails of distributions converge to GPD (General Pareto)
# q_gpd compares several R packages for fitting and quantile estimation:
dlq <- distLquantile(annMax, weighted=FALSE, quiet=TRUE, probs=0.97, list=TRUE)
dlq$quant
plotLquantile(dlq) # per default best fitting distribution functions
plotLquantile(dlq, row=c("wak","GPD*"), nbest=14)
#pdf("dummy.pdf", width=9)
plotLquantile(dlq, row="GPD*", nbest=13, xlim=c(102,110),
          linargs=list(lwd=3), heights=seq(0.02, 0.005, len=14))
#dev.off()


## Not run: 
##D ## Taken out from CRAN package check because it's slow
##D 
##D # Sanity checks: important for very small samples:
##D x1 <- c(2.6, 2.5, 2.9, 3, 5, 2.7, 2.7, 5.7, 2.8, 3.1, 3.6, 2.6, 5.8, 5.6, 5.7, 5.3)
##D q1 <- distLquantile(x1, sanerange=c(0,500), sanevals=c(NA,500))
##D x2 <- c(6.1, 2.4, 4.1, 2.4, 6, 6.3, 2.9, 6.8, 3.5)
##D q2 <- distLquantile(x2, sanerange=c(0,500), sanevals=c(NA,500), quiet=FALSE)
##D x3 <- c(4.4, 3, 1.8, 7.3, 2.1, 2.1, 1.8, 1.8)
##D q3 <- distLquantile(x3, sanerange=c(0,500), sanevals=c(NA,500))
##D 
##D # weighted distribution quantiles are calculated by different weighting schemes:
##D plotLweights(dlf)
##D 
##D # If speed is important and parameters are already available, pass them via dlf:
##D distLquantile(dlf=dlf, probs=0:5/5, selection=c("wak","gev","kap"))
##D distLquantile(dlf=dlf, truncate=0.3, list=TRUE)$truncate
##D 
##D # censored (truncated, trimmed) quantile, Peak Over Treshold (POT) method:
##D qwak <- distLquantile(annMax, sel="wak", prob=0.95, emp=FALSE, list=TRUE)
##D plotLquantile(qwak, ylim=c(0,0.06) ); qwak$quant
##D qwak2 <-distLquantile(annMax, sel="wak", prob=0.95, emp=FALSE, list=TRUE, truncate=0.6)
##D plotLquantile(qwak2, add=TRUE, distcols="blue")
##D 
##D 
##D # Simulation of truncation effect
##D library(lmomco)
##D #set.seed(42)
##D rnum <- rlmomco(n=1e3, para=dlf$parameter$gev)
##D myprobs <- c(0.9, 0.95, 0.99, 0.999)
##D mytrunc <- seq(0, 0.9, length.out=20)
##D trunceffect <- sapply(mytrunc, function(mt) distLquantile(rnum, selection="gev",
##D                              probs=myprobs, truncate=mt, quiet=TRUE,
##D                              pempirical=FALSE)["gev",])
##D # If more values are truncated, the function runs faster
##D 
##D op <- par(mfrow=c(2,1), mar=c(2,4.5,2,0.5), cex.main=1)
##D dlf1 <- distLquantile(rnum, sel="gev", probs=myprobs, emp=FALSE, list=TRUE)
##D dlf2 <- distLquantile(rnum, sel="gev", probs=myprobs, emp=FALSE, list=TRUE, truncate=0.3)
##D plotLquantile(dlf1, ylab="", xlab="")
##D plotLquantile(dlf2, add=TRUE, distcols=4)
##D legend("right", c("fitted GEV", "fitted with truncate=0.3"), lty=1, col=c(2,4), bg="white")
##D par(mar=c(3,4.5,3,0.5))
##D plot(mytrunc, trunceffect[1,], ylim=range(trunceffect), las=1, type="l",
##D      main=c("High quantiles of 1000 random numbers from gev distribution",
##D            "Estimation based on proportion of lower values truncated"),
##D      xlab="", ylab="parametric quantile")
##D title(xlab="Proportion censored", mgp=c(1.8,1,0))
##D for(i in 2:4) lines(mytrunc, trunceffect[i,])
##D library("berryFunctions")
##D textField(rep(0.5,4), trunceffect[,11], paste0("Q",myprobs*100,"%") )
##D par(op)
##D 
##D trunc <- seq(0,0.1,len=200)
##D dd <- pbsapply(trunc, function(t) distLquantile(annMax,
##D           selection="gpa", weight=FALSE, truncate=t, prob=0.99, quiet=T)[c(1,3),])
##D  plot(trunc, dd[1,], type="o", las=1)
##D lines(trunc, dd[2,], type="o", col=2)
##D 
##D 
##D set.seed(3); rnum <- rlmomco(n=1e3, para=dlf$parameter$gpa)
##D qd99 <- evir::quant(rnum, p=0.99, start=15, end=1000, ci=0.5, models=30)
##D axis(3, at=seq(-1000,0, length=6), labels=0:5/5, pos=par("usr")[3])
##D title(xlab="Proportion truncated", line=-3)
##D mytrunc <- seq(0, 0.9, length.out=30)
##D trunceffect <- sapply(mytrunc, function(mt) distLquantile(rnum, selection="gpa",
##D                       probs=0.99, truncate=mt, plot=FALSE, quiet=TRUE,
##D                       empirical=FALSE, gpd=TRUE))
##D lines(-1000*(1-mytrunc), trunceffect[1,], col=4)
##D lines(-1000*(1-mytrunc), trunceffect[2,], col=3) # interesting...
##D for(i in 3:13) lines(-1000*(1-mytrunc), trunceffect[i,], col=3) # interesting...
##D 
##D # If you want the estimates only for one single truncation, use
##D q_gpd(rnum, probs=myprobs, truncate=0.5)
##D 
## End(Not run) # end dontrun




cleanEx()
nameEx("distLweights")
### * distLweights

flush(stderr()); flush(stdout())

### Name: distLweights
### Title: Compute distribution weights from GOF
### Aliases: distLweights
### Keywords: distribution

### ** Examples

# weights from RMSE vector:
RMSE <- c(gum=0.20, wak=0.17, gam=0.21, gev=0.15)
distLweights(RMSE)
distLweights(RMSE, order=FALSE)

# weights from RMSE in data.frame:
df <- data.frame("99.9%"=2:5, RMSE=sample(3:6))
rownames(df) <- letters[1:4]
df ;  distLweights(df, onlydn=FALSE)

# custom weights:
set.seed(42); x <- data.frame(A=1:5, RMSE=runif(5)) ; x
distLweights(x) # two warnings
distLweights(x, weightc=c("1"=3, "3"=5), onlydn=FALSE)
distLweights(x, weightc=c("1"=3, "3"=5), order=FALSE, onlydn=FALSE)

# real life example:
data(annMax)
cw <- c("gpa"=7, "gev"=3, "wak"=6, "wei"=4, "kap"=3.5, "gum"=3, "ray"=2.1,
        "ln3"=2, "pe3"=2.5, "gno"=4, "gam"=5)
dlf <- distLfit(annMax, weightc=cw, quiet=TRUE, order=FALSE)
plotLweights(dlf)


# GOF judgement by RMSE, not R2 --------
# Both RMSE and R2 are computed with ECDF and TCDF
# R2 may be very good (see below), but fit needs to be close to 1:1 line,
# which is better measured by RMSE

dlf <- distLfit(annMax, ks=TRUE)
op <- par(mfrow=c(1,2), mar=c(3,4,0.5,0.5), mgp=c(1.9,0.7,0))
plot(dlf$gof$RMSE, 17:1, yaxt="n", ylab="", type="o"); axis(2, 17:1, rownames(dlf$gof), las=1)
plot(dlf$gof$R2,   17:1, yaxt="n", ylab="", type="o"); axis(2, 17:1, rownames(dlf$gof), las=1)
par(op)
sel <- c("wak","lap","nor","revgum")
plotLfit(dlf, selection=sel, cdf=TRUE)
dlf$gof[sel,-(2:7)]

x <- sort(annMax, decreasing=TRUE)
ECDF <- ecdf(x)(x)
TCDF <- sapply(sel, function(d) lmomco::plmomco(x,dlf$parameter[[d]]))

plot(TCDF[,"lap"],    ECDF, col="cyan", asp=1, las=1)
points(TCDF[,"nor"],    ECDF, col="green")
#points(TCDF[,"wak"],    ECDF, col="blue")
#points(TCDF[,"revgum"], ECDF, col="red")
abline(a=0, b=1, lwd=3, lty=3)
legend("bottomright", c("lap good RMSE bad R2", "nor bad RMSE good R2"),
       col=c("cyan","green"), lwd=2)
berryFunctions::linReg(TCDF[,"lap"], ECDF, add=TRUE, digits=3, col="cyan", pos1="topleft")
berryFunctions::linReg(TCDF[,"nor"], ECDF, add=TRUE, digits=3, col="green", pos1="left")


# more distinct example (but with fake data)
set.seed(42); x <- runif(30)
y1 <-     x+rnorm(30,sd=0.09)
y2 <- 1.5*x+rnorm(30,sd=0.01)-0.3
plot(x,x, asp=1, las=1, main="High cor (R2) does not necessarily mean good fit!")
berryFunctions::linReg(x, y2, add=TRUE, digits=4, pos1="topleft")
points(x,y2, col="red", pch=3)
points(x,y1, col="blue")
berryFunctions::linReg(x, y1, add=TRUE, digits=4, col="blue", pos1="left")
abline(a=0, b=1, lwd=3, lty=3)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("extremeStat")
### * extremeStat

flush(stderr()); flush(stdout())

### Name: extremeStat
### Title: Extreme value statistics on a linear scale
### Aliases: extremeStat extremeStat-package
### Keywords: documentation package

### ** Examples

data(annMax) # annual discharge maxima from a stream in Austria
plot(annMax, type="l")
dle <- distLextreme(annMax)
dle$returnlev




cleanEx()
nameEx("plotLexBoot")
### * plotLexBoot

flush(stderr()); flush(stdout())

### Name: plotLexBoot
### Title: Bootstrapping uncertainty intervals for return periods
### Aliases: plotLexBoot
### Keywords: bootstrap distribution dplot hplot montecarlo ts

### ** Examples

# see distLexBoot




cleanEx()
nameEx("plotLextreme")
### * plotLextreme

flush(stderr()); flush(stdout())

### Name: plotLextreme
### Title: Plot extreme value statistics
### Aliases: plotLextreme
### Keywords: distribution dplot hplot

### ** Examples

#see
?distLextreme




cleanEx()
nameEx("plotLfit")
### * plotLfit

flush(stderr()); flush(stdout())

### Name: plotLfit
### Title: Plot distributions fitted with L-moments
### Aliases: plotLfit
### Keywords: distribution hplot

### ** Examples

 # See distLfit




cleanEx()
nameEx("plotLquantile")
### * plotLquantile

flush(stderr()); flush(stdout())

### Name: plotLquantile
### Title: Plot quantiles of distributions fitted with L-moments
### Aliases: plotLquantile
### Keywords: distribution hplot

### ** Examples

# See distLquantile




cleanEx()
nameEx("plotLweights")
### * plotLweights

flush(stderr()); flush(stdout())

### Name: plotLweights
### Title: Distribution rank comparison
### Aliases: plotLweights
### Keywords: distribution hplot

### ** Examples

# see distLweights and distLfit




cleanEx()
nameEx("printL")
### * printL

flush(stderr()); flush(stdout())

### Name: printL
### Title: print dlf objects
### Aliases: printL
### Keywords: list methods print

### ** Examples


# see
?distLextreme




cleanEx()
nameEx("q_gpd")
### * q_gpd

flush(stderr()); flush(stdout())

### Name: q_gpd
### Title: GPD quantile of sample
### Aliases: q_gpd
### Keywords: distribution robust univar

### ** Examples

data(annMax)
q_gpd(annMax)
q_gpd(annMax, truncate=0.6)
q_gpd(annMax, truncate=0.85)
q_gpd(annMax, truncate=0.91)

q_gpd(annMax, package="evir")
q_gpd(annMax, package="evir", method="ml")
q_gpd(annMax, package="evd")
q_gpd(annMax, package="extRemes")
q_gpd(annMax, package="extRemes", method="GMLE")
#q_gpd(annMax, package="extRemes", method="Bayesian") # computes a while
q_gpd(annMax, package="extRemes", method="Lmoments")
q_gpd(annMax, package="extRemes", method="nonsense") # NAs
q_gpd(annMax, package="fExtremes")                   # log warnings
q_gpd(annMax, package="fExtremes", efquiet=TRUE)    # silenced warnings
q_gpd(annMax, package="fExtremes", method= "mle")
q_gpd(annMax, package="ismev")
q_gpd(annMax, package="Renext")
q_gpd(annMax, package="Renext", method="f")
berryFunctions::is.error(q_gpd(annMax, package="nonsense"), force=TRUE)

# compare all at once with
d <- distLquantile(annMax); d
# d <- distLquantile(annMax, speed=FALSE); d # for Bayesian also

q_gpd(annMax, truncate=0.85, package="evd")          # Note about quantiles
q_gpd(annMax, truncate=0.85, package="evir")
q_gpd(annMax, truncate=0.85, package="evir", quiet=TRUE) # No note
q_gpd(annMax, truncate=0.85, package="evir", undertruncNA=FALSE)

q_gpd(annMax, truncate=0.85, package="evir", list=TRUE)
str(  q_gpd(annMax, truncate=0.85, probs=0.6, package="evir", list=TRUE) )# NAs
str(  q_gpd(annMax, package="evir",      list=TRUE)   )
str(  q_gpd(annMax, package="evd",       list=TRUE)   )
str(  q_gpd(annMax, package="extRemes",  list=TRUE)   )
str(  q_gpd(annMax, package="fExtremes", list=TRUE)   )
str(  q_gpd(annMax, package="ismev",     list=TRUE)   )
str(  q_gpd(annMax, package="Renext",    list=TRUE)   )

q_gpd(annMax, package="evir", truncate=0.9, method="ml") # NAs (MLE fails often)

trunc <- seq(0,0.9,len=500)
library("pbapply")
quant <- pbsapply(trunc, function(tr) q_gpd(annMax, pack="evir", method = "pwm",
                                            truncate=tr, quiet=TRUE))
quant <- pbsapply(trunc, function(tr) q_gpd(annMax, pack="lmomco", truncate=tr, quiet=TRUE))
plot(trunc, quant["99%",], type="l", ylim=c(80,130), las=1)
lines(trunc, quant["90%",])
lines(trunc, quant["80%",])
plot(trunc, quant["RMSE",], type="l", las=1)

## Not run: 
##D ## Not run in checks because simulation takes too long
##D 
##D trunc <- seq(0,0.9,len=200)
##D dlfs <- pblapply(trunc, function(tr) distLfit(annMax, truncate=tr, quiet=TRUE, order=FALSE))
##D rmses <- sapply(dlfs, function(x) x$gof$RMSE)
##D plot(trunc, trunc, type="n", ylim=range(rmses,na.rm=TRUE), las=1, ylab="rmse")
##D cols <- rainbow2(17)[rank(rmses[,1])]
##D for(i in 1:17) lines(trunc, rmses[i,], col=cols[i])
##D 
##D dlfs2 <- lapply(0:8/10, function(tr) distLfit(annMax, truncate=tr, quiet=TRUE))
##D pdf("dummy.pdf")
##D dummy <- sapply(dlfs2, function(x)
##D {plotLfit(x, cdf=TRUE, main=x$truncate, ylim=0:1, xlim=c(20,135), nbest=1)
##D title(sub=round(x$gof$RMSE[1],4))
##D })
##D dev.off()
##D 
##D # truncation effect
##D mytruncs <- seq(0, 0.9, len=150)
##D oo <- options(show.error.messages=FALSE, warn=-1)
##D myquants <- sapply(mytruncs, function(t) q_gpd(annMax, truncate=t, quiet=TRUE))
##D options(oo)
##D plot(1, type="n", ylim=range(myquants, na.rm=TRUE), xlim=c(0,0.9), las=1,
##D      xlab="truncated proportion", ylab="estimated quantiles")
##D abline(h=quantileMean(annMax, probs=c(0.8,0.9,0.99)))
##D for(i in 1:3) lines(mytruncs, myquants[i,], col=i)
##D text(0.3, c(87,97,116), rownames(myquants), col=1:3)
##D 
##D 
##D # Underestimation in small samples
##D # create known population:
##D dat <- extRemes::revd(1e5, scale=50, shape=-0.02, threshold=30, type="GP")
##D op <- par(mfrow=c(1,2), mar=c(2,2,1,1))
##D hist(dat, breaks=50, col="tan")
##D berryFunctions::logHist(dat, breaks=50, col="tan")
##D par(op)
##D 
##D # function to estimate empirical and GPD quantiles from subsamples
##D samsizeeffect <- function(n, nrep=30, probs=0.999, trunc=0.5, Q=c(0.4,0.5,0.6))
##D {
##D res <- replicate(nrep, {
##D subsample <- sample(dat, n)
##D qGPD <- q_gpd(subsample, probs=probs, truncate=trunc)
##D qEMP <- berryFunctions::quantileMean(subsample, probs=probs, truncate=trunc)
##D c(qGPD=qGPD, qEMP=qEMP)})
##D apply(res, MARGIN=1, berryFunctions::quantileMean, probs=Q)
##D }
##D 
##D # Run and plot simulations
##D samplesize <- c(seq(20, 150, 10), seq(200,800, 100))
##D results <- pbapply::pblapply(samplesize, samsizeeffect)
##D res <- function(row, col) sapply(results, function(x) x[row,col])
##D berryFunctions::ciBand(yu=res(3,1),yl=res(1,1),ym=res(2,1),x=samplesize,
##D   main="99.9% Quantile underestimation", xlab="subsample size", ylim=c(200,400), colm=4)
##D berryFunctions::ciBand(yu=res(3,2),yl=res(1,2),ym=res(2,2),x=samplesize, add=TRUE)
##D abline(h=berryFunctions::quantileMean(dat, probs=0.999))
##D text(300, 360, "empirical quantile of full sample")
##D text(300, 340, "GPD parametric estimate", col=4)
##D text(300, 300, "empirical quantile estimate", col="green3")
##D 
## End(Not run) # end of dontrun




cleanEx()
nameEx("q_weighted")
### * q_weighted

flush(stderr()); flush(stdout())

### Name: q_weighted
### Title: Compute weighted averages of quantile estimates
### Aliases: q_weighted
### Keywords: distribution

### ** Examples

x <- data.frame(A=1:5, RMSE=runif(5))
distLweights(x, onlydn=FALSE)

q_weighted(x,  onlydn=FALSE)
q_weighted(x,  distLweights(x, weightc=c("1"=3, "3"=5), order=FALSE, onlydn=FALSE)  )

x <- rexp(190)
d <- distLquantile(x)
d2 <- q_weighted(d)
stopifnot(all(d==d2, na.rm=TRUE))

# fast option for adding custom weighted estimates:
cw <- runif(17)
names(cw) <- c("exp", "gam", "gev", "glo", "gno", "gpa", "gum", "kap", "lap",
               "ln3", "nor", "pe3", "ray", "revgum", "rice", "wak", "wei")
dw <- distLweights(d, weightc=cw)
qw1 <- q_weighted(d, weightc=cw); qw1
qw2 <- q_weighted(d, weights=dw); qw2
stopifnot(all(qw1==qw2, na.rm=TRUE))
q_weighted(d, weights=dw, onlyc=TRUE)
q_weighted(d, weights=data.frame(weightc=cw), onlyc=TRUE)

## Not run: 
##D  # time consuming
##D system.time(pbreplicate(5000, q_weighted(d, weightc=cw)))             # 8.5 secs
##D system.time(pbreplicate(5000, q_weighted(d, weights=dw, onlyc=TRUE))) # 0.8 secs
## End(Not run)




cleanEx()
nameEx("quantGPD")
### * quantGPD

flush(stderr()); flush(stdout())

### Name: quantGPD
### Title: Fast GPD quantile estimate
### Aliases: quantGPD
### Keywords: distribution robust univar

### ** Examples

data(annMax)
quantile(annMax, 0.99)
quantGPD(annMax, 0.99)

## Not run: 
##D  # Excluded from CRAN checks to reduce checking time
##D data(rain, package="ismev") ;  rain <- rain[rain>0]
##D hist(rain, breaks=50, col=7)
##D tr <- seq(0,0.999, len=50)
##D qu <- pbapply::pbsapply(tr, quantGPD, x=rain, probs=c(0.9,0.99,0.999) ) # 30 s
##D plot(tr, qu[3,], ylim=range(rain), las=1, type="l")
##D lines(tr, qu[2,], col=2); lines(tr, qu[1,], col=4)
##D 
##D tr <- seq(0.88,0.999, len=50)
##D qu <- pbapply::pbsapply(tr, quantGPD, x=rain, probs=c(0.9,0.99,0.999) ) # 5 s
##D plot(tr, qu[3,], ylim=range(rain), las=1, type="l")
##D lines(tr, qu[2,], col=2); lines(tr, qu[1,], col=4);
##D tail(qu["n",])
##D 
##D library(microbenchmark)
##D data(rain, package="ismev"); rain <- rain[rain>0]
##D mb <- microbenchmark(quantGPD(rain[1:200], truncate=0.8, probs=0.99, addn=F),
##D distLquantile(rain[1:200], sel="gpa", emp=F, truncate=0.8, quiet=T, probs=0.99)[1,1]
##D )
##D boxplot(mb)
##D # since computing the lmoments takes most of the computational time,
##D # there's not much to optimize in large samples like n=2000
##D 
## End(Not run)




cleanEx()
nameEx("weightp")
### * weightp

flush(stderr()); flush(stdout())

### Name: weightp
### Title: distribution weights
### Aliases: weightp
### Keywords: datasets

### ** Examples


data(weightp)
data.frame(weightp)
barplot(weightp, horiz=TRUE, las=1)
stopifnot(   all.equal(sum(weightp), 1)   )

data(annMax) ; data(weightp)
dlf <- distLfit(annMax, weightc=weightp)
dlf$gof
quant <- distLquantile(annMax, weightc=weightp)
quant




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
