#' @title count string occurrences
#' @description count how often a certain string occurs, summing over a vector
#' @return single integer
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2025
#' @export
#' @examples
#' vec210 <- c("with abc + abc + ab", "also abcde", "no alphabet")
#' vec021 <- c("no alphabet", "this has abcabc + ab", "also abcde")
#' vec000 <- c("this has no", "alphabet", "at all")
#' vec4 <-     "this has abc and abcabcabc"
#' stopifnot(count("abc", vec210) == 3)
#' stopifnot(count("abc", vec021) == 3)
#' stopifnot(count("abc", vec000) == 0)
#' stopifnot(count("abc", vec4  ) == 4)
#'
#' @param a     character string (can have regex)
#' @param x     charstring (vector)
#' @param \dots Further arguments passed to \code{\link{gregexpr}}
#'
count <- function(
pattern,
x,
...
)
{
g <- gregexpr(pattern, x, ...)
sum(unlist(g)>0)
}

