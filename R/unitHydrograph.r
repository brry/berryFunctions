# Unit Hydrograph
# Berry Boessenkool   July 2013    berry-b@gmx.de


# continuous UH:
unitHydrograph <- function(
    n,
    k,
    t)
{
if(length(n)>1 | length(k)>1) stop("n and k can only have one single value!
For vectorization, use sapply (see documentation examples).")
t^(n-1) / k^n / gamma(n) * exp(-t/k)  # some say /k^(n-1) for the second term!
}

