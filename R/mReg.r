### multiple Regression with several function types ----------------------------
# Berry Boessenkool, 12-2012, updated 04-2013 and 08-2013     berry-b@gmx.de
# Any feedback is welcome!

#  1 linear                 a*x + b
#  2 quadratic (parabola)   a*x^2 + b*x + c
#  3 kubic                  a*x^3 + b*x^2 + c*x + d
#  4 Polynom 4th degree     a*x^4 + b*x^3 + c*x^2 + d*x + e
#  5 Polynom 5              a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
#  6 logarithmic            a*log(x) + b
#  7 exponential            a*e^(b*x)
#  8 power/root             a*x^b
#  9 reciprocal             a/x + b
# 10 rational               1 / (a*x + b)
# 11 exponential 4 Param    a*e^(b*(x+c)) + d
# Yet to add: 12 hyperbolic             sinh(), cosh(), tanh()
#             13 powerplus a^x + b

mReg <- function(
    x,
    y, # x and y Data
    Poly45=FALSE, # Also fit polynomials of 4th and 5th degree?
    exp_4=FALSE, # return exp_4 fits in table? (only best fit is plotted) DEFAULT: FALSE
    xf=deparse(substitute(x)), yf=deparse(substitute(y)), # x and y names for Formula
    ncolumns=9, # number of columns in output. Set lower to avoid overcrowding the console
    plot=TRUE,   # plot data and fitted functions?
    add=FALSE,    #  add lines to existing plot?
    nbest=12, # number of best fitting functions to be plotted (console output table always has all)
    R2min, # minimum Rsquared value for function type to be plotted. Suggestion: 0.6 (2/3 of variation of y is explained by function of x)
    selection=NULL, # Integers of functions to be plotted, assigned as in list above.
    digits=2, # significant digits for rounding formula output and R^2 in legend
    extend=0.4, # extention of axis ranges (proportion of range)
    xlim=range(x, finite=TRUE) + c(-1,1)*extend*diff(range(x, finite=TRUE)), # default xlim
    ylim=range(y, finite=TRUE) + c(-1,1)*extend*diff(range(y, finite=TRUE)), # default ylim
    xlab=xf, #  default labels via substitute before replacing zeros in x and y
    ylab=yf, #
    las=1, # label axis style, see ?par
    lwd=rep(1,12), # some graphical parameters, all of length 12
    lty=rep(1,12),
    col=NULL, # 12 colors for lines and legend texts. DEFAULT: NULL, means they are specified internally
    pcol=par("col"), # color for the data points
    pch=16, #  point character for the data points
    legend=TRUE, #posx="top", posy=NULL, inset=0, # legend options
    legargs=NULL, # legend options
    legendform="nameform", # "full" formula, "form", "nameform" or only "name" in legend in plot
    ...) # more graphical parameters passed to plot
{
# Function start
# input checking
if( (xf %in% letters[1:6] | yf %in% letters[1:6])  &  legendform %in% c("nameform", "form")  )
   warning("Using single letters a to f for input variable names is not recommended, as formula forms will be difficult to read" )
if( xf=="e" | yf=="e" )
   warning("Using 'e' for input variable name is not recommended, as exponential formula forms will be difficult to read" )
if(any(4:5 %in% selection)) Poly45 <- TRUE
if(11 %in% selection) exp_4 <- TRUE
if( ! round(nbest,1) %in% 0:12) stop("nbest has to be an integer between 0 and 12")
if(length(lwd)==1) lwd <- rep(lwd, 12)
if(length(lty)==1) lty <- rep(lty, 12)
#
# Functions needed for function descriptions
# abbreviate parameters of fitted functions:
ab1 <- function(input) signif(input,digits)
# abbreviate parameters of fitted functions with algebraic sign (Vorzeichen):
ab <- function(input) paste0(ifelse(input>0, " + ", " - "),
                             abs(signif(input,digits)))
# Prepare Output Table
output <- as.data.frame(matrix(NA, ncol=if(Poly45) 9 else 7, nrow=11 ))
colnames(output) <- c("R2","Formulas","R2full", letters[1:(ncol(output)-3)] )
#
#  1 linear --------------- a*x + b --------------------------------------------
mod1 <- lm( y ~ x )
output$R2[1] <- summary(mod1)$r.squared
output$a [1] <- coef(mod1)[2]
output$b [1] <- coef(mod1)[1]
#  2 quadratic (parabola) - a*x^2 + b*x + c ------------------------------------
mod2 <- lm(y ~ I(x^2) + x)
output$a [2] <- coef(mod2)[2]
output$b [2] <- coef(mod2)[3]
output$c [2] <- coef(mod2)[1]
output$R2[2] <- rsquare(y, output$a[2]*x^2 + output$b[2]*x + output$c[2])
#  3 cubic ---------------- a*x^3 + b*x^2 + c*x + d ----------------------------
mod3 <- lm(y ~  poly(x,3, raw=TRUE))
output[3,4:7] <- rev(coef(mod3))
output$R2[3] <- rsquare(y, output$a[3]*x^3 + output$b[3]*x^2 + output$c[3]*x + output$d[3])
if(Poly45){
  #  4 Polynom4 ----------- a*x^4 + b*x^3 + c*x^2 + d*x + e --------------------
  mod4 <- lm(y ~  poly(x,4, raw=TRUE))
  output[4, 4:8] <- rev(coef(mod4))
  output$R2[4] <- rsquare(y, output$a[4]*x^4 + output$b[4]*x^3 + output$c[4]*x^2 + output$d[4]*x + output$e[4])
  #  5 Polynom5 ----------- a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f ------------
  mod5 <- lm(y ~  poly(x,5, raw=TRUE))
  output[5,4:9] <- rev(coef(mod5))
  output$R2[5] <- rsquare(y, output$a[5]*x^5 + output$b[5]*x^4 + output$c[5]*x^3 + output$d[5]*x^2 + output$e[5]*x + output$f[5])
  } # if Poly45 end
#  6 logarithmic ---------- a*log(x) + b ---------------------------------------
mod6 <- lm( y ~ log10(replace(x, x==0, 1e-10)) ) # influence needs yet to be checked! ###
output$a [6] <- coef(mod6)[2]
output$b [6] <- coef(mod6)[1]
output$R2[6] <- rsquare(y, output$a[6]*log10(replace(x, x==0, 1e-10)) + output$b[6])
#  7 exponential ---------- a*e^(b*x) ------------------------------------------
log_y <- log(replace(y, y==0, 1e-10))
mod7 <- lm( log_y ~ x )                    # y = a*e^(b*x)
output$a [7] <- exp(coef(mod7)[1])         # ln(y) = ln(a) + ln( e^(b*x) )
output$b [7] <- coef(mod7)[2]              # ln(y) = ln(a) + b*x
output$R2[7] <- rsquare(y, output$a[7]*exp(output$b[7]*x))
#  8 power/root ----------- a*x^b ----------------------------------------------
mod8 <- lm( log_y ~ log(replace(x, x==0, 1e-10)) ) # y = a*x^b
output$a [8] <- exp(coef(mod8)[1])                 # ln(y) = ln(a) + ln(x^b)
output$b [8] <- coef(mod8)[2]                      # ln(y) = ln(a) + b*ln(x)
output$R2[8] <- rsquare(y, output$a[8]*replace(x, x==0, 1e-10)^output$b[8])
#  9 reciprocal ----------- a/x + b --------------------------------------------
mod9 <- lm( y ~ I(1/replace(x, x==0, 1e-10)) )
output$a [9] <- coef(mod9)[2]
output$b [9] <- coef(mod9)[1]
output$R2[9] <- rsquare(y, output$a[9]/replace(x, x==0, 1e-10) + output$b[9])
# 10 rational ------------- 1 / (a*x + b) --------------------------------------
mod10 <- lm( I(1/y) ~ x)
output$a [10] <- coef(mod10)[2]
output$b [10] <- coef(mod10)[1]
output$R2[10] <- rsquare(y, 1 / (output$a[10]*x + output$b[10]))
# 11 exp_4 ---------------- a*e^(b*(x+c))+d ------------------------------------
# 4-parametric exponential distibutions
if(exp_4) output_exp4p <- exp4p(x,y, digits=digits)
if(exp_4) output[11,] <- output_exp4p[1,] # only include best fit for plotting
#
# 12 hyperbolic ----------- sinh(a*x+b)+c, cosh(), tanh() ----------------------
# yet to add
#
# name output rows -------------------------------------------------------------
rownames(output) <- c("linear", "square", "cubic", "poly4", "poly5",
     "logarithmic", "exponential", "power", "reciprocal", "rational", "exp_4p" )#, "hyperbolic")
#
# Formulas of fitted functions -------------------------------------------------
output$Formulas[1] <- paste0(yf," = ", ab1(output$a[1]),"*",xf,      ab(output$b[1]) )
output$Formulas[2] <- paste0(yf," = ", ab1(output$a[2]),"*",xf,"^2", ab(output$b[2]),"*",xf,      ab(output$c[2]) )
output$Formulas[3] <- paste0(yf," = ", ab1(output$a[3]),"*",xf,"^3", ab(output$b[3]),"*",xf,"^2", ab(output$c[3]),"*",xf,      ab(output$d[3]) )
if(Poly45){
output$Formulas[4] <- paste0(yf," = ", ab1(output$a[4]),"*",xf,"^4", ab(output$b[4]),"*",xf,"^3", ab(output$c[4]),"*",xf,"^2", ab(output$d[4]),"*",xf,      ab(output$e[4]) )
output$Formulas[5] <- paste0(yf," = ", ab1(output$a[5]),"*",xf,"^5", ab(output$b[5]),"*",xf,"^4", ab(output$c[5]),"*",xf,"^3", ab(output$d[5]),"*",xf,"^2", ab(output$e[5]),"*",xf, ab(output$f[5]) )
} # end if Poly45
output$Formulas[6] <- paste0(yf," = ", ab1(output$a[6]),"*log10(",xf, ")", ab(output$b[6]) )
output$Formulas[7] <- paste0(yf," = ", ab1(output$a[7]),"*e^(",           ab1(output$b[7]), "*", xf, ")" )
output$Formulas[8] <- paste0(yf," = ", ab1(output$a[8]),"*", xf, "^",     ab1(output$b[8]) )
output$Formulas[9] <- paste0(yf," = ", ab1(output$a[9]),"/",xf,            ab(output$b[9]) )
output$Formulas[10]<- paste0(yf," = 1/( ", ab1(output$a[10]),              ab(output$b[10]), "*", xf, " )" )
if(exp_4)
output$Formulas[11]<- paste0(yf, " = ", ab1(output$a[11]), "*e^(", ab1(output$b[11]), "*(", xf, ab(output$c[11]), "))", ab(output$d[11]))
#
# edit Rsquared columns in output table ----------------------------------------
output$R2full <- output$R2
output$R2 <- round(output$R2, digits)
ord <- order(output$R2full, decreasing=TRUE) # descending order of goodness of fit, for legend
#
# plot data and functions ------------------------------------------------------
if(plot & nbest!=0) {
  if(!add)  plot(x, y, las=las, pch=pch, ylab=ylab, xlab=xlab, xlim=xlim, ylim=ylim, col=pcol, ...)
  # select function types that should be drawn:
  todraw <- rep(FALSE, 12)
  if(missing(R2min) & is.null(selection)) todraw[ord[1:nbest]] <- TRUE
  if(!is.null(selection)) todraw[selection] <- TRUE
  if(!missing(R2min)) todraw[output$R2full>=R2min] <- TRUE
  #
  xdraw <- seqR(par("usr")[1:2], len=200)
  xdrawtab <- data.frame(x=xdraw) # colnames(xdrawtab) <- as.character(xf) # not necessary, as poly uses "x" (the one in the function environment)
  if(is.null(col)) col <- c("black", "red", "green3", "chartreuse", "forestgreen", "blue", "cyan", "magenta", "yellow", "gray", "orange", "deeppink")
  #
  if(todraw[1]) lines(xdraw, predict( mod1 , xdrawtab ),          col=col[1], lwd=lwd[1], lty=lty[1]) # 1 linear
  if(todraw[2]) lines(xdraw, predict( mod2 , xdrawtab ),          col=col[2], lwd=lwd[2], lty=lty[2]) # 2 square
  if(todraw[3]) lines(xdraw, predict( mod3 , xdrawtab ),          col=col[3], lwd=lwd[3], lty=lty[3]) # 3 cubic
  if(Poly45){
  if(todraw[4]) lines(xdraw, predict( mod4 , xdrawtab ),          col=col[4], lwd=lwd[4], lty=lty[4]) # 4 polynomial 4th degree
  if(todraw[5]) lines(xdraw, predict( mod5 , xdrawtab ),          col=col[5], lwd=lwd[5], lty=lty[5]) # 5 polynomial 5th degree
  } # end if Poly45
  if(all(xdraw<=0)) warning("no logarithmic regression could be done, as there are no positive x values")
  xd2 <- xdraw[xdraw>0]
  if(todraw[6]) lines(xd2,   output$a[6]*log10(xd2)+output$b[6],  col=col[6], lwd=lwd[6], lty=lty[6]) # 6 logarithmic
  if(todraw[7]) lines(xdraw, output$a[7]*exp(output$b[7]*xdraw),  col=col[7], lwd=lwd[7], lty=lty[7]) # 7 exponential
  if(todraw[8]) lines(xdraw, output$a[8]*xdraw^output$b[8],       col=col[8], lwd=lwd[8], lty=lty[8]) # 8 power (Potenz)
  if(todraw[9]) lines(xdraw, output$a[9]/xdraw+output$b[9],       col=col[9], lwd=lwd[9], lty=lty[9]) # 9 reciprocal
  if(todraw[10])lines(xdraw, 1/(output$a[10]*xdraw+output$b[10]), col=col[10],lwd=lwd[10],lty=lty[10])# 10 rational
  if(exp_4)
  if(todraw[11])lines(xdraw, output$a[11]*exp(output$b[11]*(xdraw+output$c[11]))+output$d[11], col=col[11],lwd=lwd[11], lty=lty[11]) # 11 exp_4par
  # if(todraw[12])lines(xdraw, output$a[12]*cosh(x)+output$b[12], col=col[12],lwd=lwd[12], lty=lty[12]) # 12 hyperbolic
# prepare and write legend -----------------------------------------------------
if(legend) {
  fForms <- c("a*x + b", "a*x^2 + b*x + c", "a*x^3 + b*x^2 + c*x + d",
     "a*x^4 + b*x^3 + c*x^2 + d*x + e", "a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f",
     "a*log(x) + b", "a*e^(b*x)", "a*x^b", "a/x + b", "1 / (a*x + b)", "a*e^(b*(x+c)) + d")
  #
  legendlabel <- if(legendform=="full") output$Formulas else
                 if(legendform=="form") fForms else
                 if(legendform=="nameform") paste(rownames(output), "  ", fForms) else
                 if(legendform=="name") rownames(output) else
                 stop("wrong legendform. Use 'full', 'form', 'nameform', or 'name'.")
  # remove entries for Poly45 and exp_4 if necessary:
  ###ord <- ord[ !is.na(output$a)]
  if(!Poly45) ord <- ord[-(length(ord)-0:1)] # remove last two (Poly4 and 5) from legend
  if(!exp_4)  ord <- ord[-length(ord)] # remove last one (exp_4) from legend
  ord <- ord[ord %in% which(todraw) ] # keep only the ones that are to be plotted
  #
  legargdefaults <- list(x="top", bty="n", cex=0.8, text.col=col[ord],
       legend=paste(sprintf(paste0("%.",digits,"f"), output$R2), "  ", legendlabel)[ord])
  do.call(graphics::legend, args=owa(legargdefaults, legargs, "legend"))
  } # if legend end
  } # if plot end
#
# final output -----------------------------------------------------------------
if(exp_4) output <- rbind(output, output_exp4p[-1,]) # best fit is already included
if(!exp_4)  output <- output[-11,   ] # remove excess rows
if(!Poly45) output <- output[-(4:5),] # remove excess rows
#
output <- output[order(output$R2full, decreasing=TRUE),]
#
if(ncolumns >9) ncolumns <- 9
if(ncolumns <0) ncolumns <- 0
if(ncolumns >7 & !Poly45) ncolumns <- 7
#
if(ncolumns !=0) return(output[,1:ncolumns])
#
} # Function end ---------------------------------------------------------------
