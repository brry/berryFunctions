#' Create .Rnw presentation template
#'
#' Create folder with .Rnw presentation template and fig_extern folder.
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2017
#' @seealso \code{\link{createFun}}
#' @keywords file
#' @importFrom stats rnorm
#' @export
#' @examples
#' createPres("Berry_Conference")
#'
#' @param presname Name of .Rnw file to be created. DEFAULT: "presentation"
#' @param dir      Name of directory that will contain .Rnw file and 
#'                 fig_extern folder. "_1" will be appended if already existing,
#'                 see \code{\link{newFilename}}. DEFAULT: "presentation"
#' @param path     Location of \code{dir}. Passed to \code{\link{setwd}}.
#'                 DEFAULT: "."
#'
createPres <- function(
presname="presentation",
dir="presentation",
path="."
)
{
#  
owd <- setwd(path)
on.exit(setwd(owd))
dir <- newFilename(dir, quiet=TRUE)
message("Creating '", presname, ".Rnw' in '", normalizePath(path,"/"),"/",dir,"'.")

dir.create(dir)
dir.create(file.path(dir, "fig_extern"))
pdf(file.path(dir, "fig_extern/MyFig.pdf"), height=5)
par(mar=c(3,3,2,1), mgp=c(2.1,0.7,0), las=1)
plot(cumsum(rnorm(800)), type="l", lwd=3, col="purple", main="Existing Figure")
dev.off()
Sys.setlocale(category = "LC_TIME", locale="C")
curmonth <- format(Sys.Date(), "%B %Y")
Sys.setlocale(category = "LC_TIME", locale="")

cat(
"% presentation aboutSomething
% Berry Boessenkool, Potsdam University, Germany
% berry-b@gmx.de

\\documentclass[compress, xcolor=dvipsnames]{beamer}
\\setbeamerfont{frametitle}{size=\\normalsize}

\\usepackage{hyperref, graphicx}
\\usepackage[dvipsnames]{xcolor}
\\renewcommand\\appendixname{Appendix}
\\usepackage[absolute,overlay]{textpos}
\\hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
% \\beamertemplatenavigationsymbolsempty
\\setbeamertemplate{navigation symbols}[only frame symbol]
%\\usetheme{Madrid}
\\useoutertheme[subsection=false]{miniframes}
\\beamersetleftmargin{0.5cm}
\\beamersetrightmargin{0.5cm}
\\let\\Tiny=\\tiny % avoid warning: Font shape `OT1/cmss/m/n' in size <4> not available. size <5> substituted on input line
\\setbeamertemplate{footline}[frame number]

% Reference images:
\\newcommand{\\bildlink}[1]{\\flushleft{\\tiny \\href{#1}{\\textcolor{gray}{#1}} \\normalsize }}
\\newcommand{\\bildlinkt}[2]{\\flushleft{\\tiny \\href{#1}{\\textcolor{gray}{#2}} \\normalsize }}
% format inline R command names in blue courier:
\\newcommand{\\rcode}[1]{\\texttt{\\textcolor{Blue}{#1}}} % or use Blue

% Nice appendix numbering:
\\newcommand{\\appendixbegin}{
   \\newcounter{framenumberappendix}
   \\setcounter{framenumberappendix}{\\value{framenumber}}
}
\\newcommand{\\appendixend}{
   \\addtocounter{framenumberappendix}{-\\value{framenumber}}
   \\addtocounter{framenumber}{\\value{framenumberappendix}} 
}



% ACTUAL SLIDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\begin{document}
\\centering


% ---------------------------

\\section{intro}

% ---------------------------

<<setup, include=FALSE>>=
opts_chunk$set(cache=T, echo=TRUE, fig.height=3.3, fig.width=5, out.width='0.9\\\\textwidth')
@

% ---------------------------

\\begin{frame}%[plain]
\\vspace{1em}
\\Large
\\textbf{NiceTitle\\\\[1.5em] \\large SubTitle}\\\\[2em]
\\normalsize
Berry Boessenkool, \\href{http://www.geo.uni-potsdam.de/geoecology.html}{uni-potsdam.de},",curmonth,"\\\\[1em]
\\texttt{berry-b@gmx.de}\\\\[1em]
\\href{https://github.com/brry/rdwd\\#rdwd}{github.com/brry/rdwd}\\\\
\\href{https://cran.r-project.org/package=extremeStat/vignettes/extremeStat.html}{cran.r-project.org/package=extremeStat}
\\end{frame}

% ---------------------------

\\begin{frame}[fragile]{Frametitle}
<<chunkname, size=\"footnotesize\">>=
plot(rnorm(1000))
@
\\end{frame}

% ---------------------------

\\section{moreStuff}

% ---------------------------

\\begin{frame}[fragile]{Frametitle}
\\pause
\\begin{itemize}[<+->]
\\item ItemOne
\\item ItemTwo
\\end{itemize}
\\onslide<+->
\\begin{flushleft} SomeMore \\end{flushleft}
\\end{frame}

% ---------------------------

\\begin{frame}{NoCodeNoFragile}
R package \\rcode{rdwd} ~~$->$~~ easy usage of weather datasets
\\end{frame}

% ---------------------------

\\begin{frame}{includegraphics}
\\includegraphics[height=0.9\\textheight]{fig_extern/MyFig.pdf}
% \\includegraphics[width=0.99\\textwidth]{fig_extern/MyFig.pdf}
\\end{frame}

% ---------------------------

\\section{conclusion}

% ---------------------------

\\begin{frame}{Conclusions}
\\begin{itemize}[<+->]
\\item ItemOne
\\item ItemTwo
\\end{itemize}
\\end{frame}

% ---------------------------

\\appendixbegin

% ---------------------------

\\begin{frame}[fragile]{AppendixTitle}
<<chunkname2, echo=FALSE>>=
plot(1, type=\"n\", las=1)
text(1,1, \"Appendix Material\", col=\"orange\")
@
\\pause
Works with pause
\\end{frame}

% ---------------------------

\\appendixend

\\end{document}", file=paste0(dir, "/", presname, ".Rnw") )

}