monthLabs <- function(             # returns Date object
   startyear=2002,
   stopyear=2018,
   npm=2,  # number of labels per month
   npy=NA)
{ # start of function
# check for correct input, especially integers:
if(length(startyear)>1) stop("'startyear' has to be one single number.")
if(length(stopyear)>1) stop("'stopyear' has to be one single number.")
if(startyear > stopyear) stop("'startyear' has to be smaller than 'stopyear'.")
if(!abs(startyear - round(startyear)) < .Machine$double.eps^0.5) stop("'startyear' has to be an integer (or close to one).")
if(!abs(stopyear  - round(stopyear )) < .Machine$double.eps^0.5) stop("'stopyear' has to be an integer (or close to one).")
#
# select labels per month or per year
if(is.na(npy)) # the default
{
# check for wrong npm-argument
if(!npm %in% c(1:3,6)) stop("wrong 'npm'-value: possible are 1,2,3 or 6.")
# define possible combinations
npmval <- list(npm1=1, npm2=c(1,15), npm3=c(1,10,20), npm4=NA, npm5=NA, npm6=c(1,5,10,15,20,25))
# paste the years, months, and (dependent on npm) the days
as.Date(   paste(rep(startyear:stopyear, each=12*npm),
                 rep(1:12,each=npm),
                 npmval[[npm]],       sep="-")
        )
} else
{
# check for wrong npm-argument
if(!npy %in% c(1:4,6)) stop("wrong 'npy'-value: possible are 1,2,3,4 or 6.")
# define possible combinations
npyval <- list(npy1=1, npy2=c(1,7), npy3=c(1,5,9), npy4=c(1,4,7,10), npy5=NA, npy6=c(1,3,5,7,9,11))
# paste the years and (dependent on npy) the months
as.Date(paste(rep(startyear:stopyear, each=npy), npyval[[npy]], "01", sep="-"))
} # end of labels per year
} # end of function


# Old stuff in paste command:
                 # numberofyears <- stopyear-startyear ; if(numberofyears<1) numberofyears <- 1
                 # rep(npmval[[npm]], numberofyears*npm),        # old idea, not necessary, as far as I cann tell right now. Maybe paste behaviour changed? Or it was just unnecessary...

