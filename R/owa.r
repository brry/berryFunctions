# argument overwriting
# combine default and user-specified argument lists
# Berry Boessenkool, late 2013 / early 2014

owa <- function(  # owa: overwrite arguments
       d,    #d: default
       a,    #a: arguments specified by user
       ...) # arguments that can not be overwritten (are left unchanged)
{
if(is.null(a) | length(a)==0) return( as.list(d) )
if(is.null(names(a))) stop("Arguments must be named!")
if("" %in% names(a) ) stop("All arguments must be named!")
#
u <- list(...) # unchanged arguments
if("u" %in% names(u)) stop("The argument 'u' has been replaced by ellipsis and does not work anymore.")
if( isTRUE(a) ) a <- NULL # catch where useres try to give eg legargs=TRUE
#
a <- a[ ! names(a) %in% u ] # discard arguments that should be left unchanged
#
###d <- d[order(names(d))] # sort lists, so that order of args given in a is irrelevant
###a <- a[order(names(a))]
a_replace <- a[names(a) %in% names(d)]
d[names(a_replace)] <- a_replace # replace (overwrite)
a_add <- a[ !names(a) %in% names(d) ]
result <- c(d,  a_add) # add further arguments given by the user
as.list(result)
}
