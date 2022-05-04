#' call stack of a function
#' 
#' trace the call stack e.g. for error checking and format output for do.call levels
#' 
#' @return Character string with the call stack
# @section Warning: Called from \link{do.call} settings with large objects,
#                   tracing may take a lot of computing time.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2016 + March 2017
#' @seealso \code{\link{tmessage}}, \code{\link{tryStack}}, \code{\link{checkFile}} for example usage
#' @keywords programming error
#' @importFrom utils capture.output head
#' @export
#' @examples
#' lower <- function(a, s) {warning(traceCall(s, vigremove=FALSE), "stupid berry warning: ", a+10); a}
#' upper <- function(b, skip=0) lower(b+5, skip)
#' upper(3)
#' upper(3, skip=1) # traceCall skips last level (R3: warning, R4.1: .makeMessage, R4.2: lapply)
#' upper(3, skip=6) # now the stack is empty
#' d <- tryStack(upper("four"), silent=TRUE)
#' inherits(d, "try-error")
#' cat(d)
#' 
#' lower <- function(a,...) {warning(traceCall(1, prefix="in ", suffix=": "),
#'                           "How to use traceCall in functions ", call.=FALSE); a}
#' upper(3)
#' 
#' @param skip Number of levels to skip in \code{\link{traceback}}
#' @param prefix Prefix prepended to the output character string. DEFAULT: "\\nCall stack: "
#' @param suffix Suffix appended to the end of the output. DEFAULT: "\\n"
#' @param vigremove Logical: remove call created using devtools::build_vignettes()?
#'                  DEFAULT: TRUE
#' 
traceCall <- function(
skip=0,
prefix="\nCall stack: ",
suffix="\n",
vigremove=TRUE
)
{
  if(skip<0) stop("skip must be >=0, not ", skip, ".")
  stack <- lapply(sys.calls(), deparse) # language to character
  tb <- head(stack, -(skip+1)  )
  # check for empty lists because skip is too large:
  if(length(tb)==0) return(paste0(prefix, "--empty stack--", suffix))
  tb <- lapply(tb, "[", 1) # to shorten do.call (function( LONG ( STUFF)))
  tb <- lapply(tb, function(x) if(substr(x,1,7)=="do.call")
    sub(",", "(", sub("(", " - ", x, fixed=TRUE), fixed=TRUE) else x)
  calltrace <- sapply(strsplit(unlist(tb), "(", fixed=TRUE), "[", 1)
  calltrace <- paste(calltrace, collapse=" -> ")
  calltrace <- paste0(prefix, calltrace, suffix)
  if(vigremove)
  {
    elements <- c("-> withCallingHandlers -> withVisible -> eval -> eval ",
                  "-> tryCatch -> tryCatchList -> tryCatchOne -> doTryCatch ",
                  "-> process_file -> withCallingHandlers -> process_group -> process_group.block ",
                  "-> call_block -> block_exec -> in_dir -> evaluate -> evaluate_call -> timing_fn -> handle ",
                  "-> vweave_rmarkdown -> rmarkdown::render -> knitr::knit ",
                  "-> tools::buildVignettes -> engine",
                  "weave ->",
                  "knit -> try ->",
                  "rmarkdown::render_site -> generator$render -> in_dir",
                  "-> render_book_script -> render_book -> render_cur_session -> rmarkdown::render ",
                  "-> knitr::knit -> call_block -> block_exec -> in_dir -> evaluate ",
                  "-> evaluate::evaluate -> evaluate_call -> timing_fn -> handle "
                  )
  for(k in elements) calltrace <- sub(k, "", calltrace)
  
  calltrace <- gsub(" -> .makeMessage -> lapply", "", calltrace) # R > 4.2.0 https://github.com/wch/r-source/commit/a114756ee
  calltrace <- gsub(" -> .makeMessage",           "", calltrace) # R 4.1.0 - 4.1.3
  }
  calltrace <- gsub("->  ->", "->", calltrace)
  calltrace <- gsub("with -> with.default -> eval -> eval", "with ->", calltrace)
  calltrace <- gsub("withVisible -> eval -> eval -> ", "", calltrace)
  calltrace
}
