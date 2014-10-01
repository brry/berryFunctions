# Nash-Sutcliffe efficiency
# Berry Boessenkool, July 2013, based on eval.NSeff  in RHydro Package
nse <- function(
                obs,
                sim)
{
if(!(is.vector(obs) & is.vector(sim))) stop("Input is not a vector.")
if(length(obs) != length(sim)) stop("Vectors are not of equal length.")
if(any(is.na(obs)|is.na(sim)))
     {
     Na <- which(is.na(obs)|is.na(sim))
     warning(length(Na), " NAs were omitted from ", length(obs), " data points.")
     obs <- obs[-Na] ; sim <- sim[-Na]
     } # end if NA
1 - ( sum((obs - sim)^2) / sum((obs - mean(obs))^2) )
}
