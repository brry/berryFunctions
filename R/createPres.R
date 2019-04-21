#' Create .Rnw presentation template
#' 
#' Create folder with .Rnw presentation template and fig_extern folder.
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2017
#' @seealso \code{\link{createFun}}
#' @keywords file
#' @importFrom graphics par plot
#' @importFrom grDevices dev.off pdf
#' @importFrom stats rnorm
#' @export
#' @examples
#' \dontrun{
#' createPres("Berry_Conference")
#' }
#' 
#' @param presname Name of .Rnw file to be created. DEFAULT: "pres"
#' @param dir      Name of directory that will contain .Rnw file and
#'                 fig_extern folder. "_1" will be appended if already existing,
#'                 see \code{\link{newFilename}}. DEFAULT: "presentation"
#' @param path     Location of \code{dir}. Passed to \code{\link{setwd}}.
#'                 DEFAULT: "."
#' @param navbullets Logical: include navigation slide bullet points in header?
#'                 DEFAULT: FALSE
#' @param bgblack  Logical: set a black background instead of a white one?
#'                 Requires all R graphics fg and bg colors to be changed!
#'                 See "How to avoid death By PowerPoint" at 11:49 minutes 
#'                 \url{https://youtu.be/Iwpi1Lm6dFo?t=11m49s}.
#'                 Change colors manually in the Rnw files searching for 
#'                 \code{bg=}, \code{linkcolor=}, \code{urlcolor=}
#'                 in the preamble and \code{color} right after \code{begin document}.
#'                 DEFAULT bgblack: FALSE
#' @param open     Logical: run \code{\link{openFile}}? DEFAULT: TRUE
#' 
createPres <- function(
presname="pres",
dir="presentation",
path=".",
navbullets=FALSE,
bgblack=FALSE,
open=TRUE
)
{
#
owd <- setwd(path)
on.exit(setwd(owd))
dir <- newFilename(dir, quiet=TRUE)
message("Creating '", presname, ".Rnw' in '", dir,"'.")

dir.create(dir)
dir.create(file.path(dir, "fig_extern"))
pdf(file.path(dir, "fig_extern/MyFig.pdf"), height=5)
par(mar=c(3,3,2,1), mgp=c(2.1,0.7,0), las=1)
plot(cumsum(rnorm(800)), type="l", lwd=3, col="purple", main="Existing Figure")
dev.off()

# Camera image
cam1 <- system.file("extdata/camera1.jpg", package="berryFunctions")
cam2 <- system.file("extdata/camera2.png", package="berryFunctions")
ccby <- system.file("extdata/ccby.png",    package="berryFunctions")
rlog <- system.file("extdata/Rlogo.png",   package="berryFunctions")

file.copy(cam1, file.path(dir, "fig_extern/camera1.jpg"))
file.copy(cam2, file.path(dir, "fig_extern/camera2.png"))
file.copy(ccby, file.path(dir, "fig_extern/ccby.png"))
file.copy(rlog, file.path(dir, "fig_extern/Rlogo.png"))


Sys.setlocale(category = "LC_TIME", locale="C")
curmonth <- format(Sys.Date(), "%B %Y")
Sys.setlocale(category = "LC_TIME", locale="")

             bgcolor <- "white" ; txcolor <- "black"
if(bgblack) {bgcolor <- "black" ; txcolor <- "white"}
             

# header (depending on navbullets):
nb_y <- "\\useoutertheme[subsection=false]{miniframes}"
nb_n <- "\\setbeamertemplate{headline}
{%
  \\begin{beamercolorbox}[ht=3.5ex,dp=1.125ex,%
      leftskip=0cm,rightskip=0cm plus1filll]{section in head/foot}
    \\usebeamerfont{section in head/foot}\\usebeamercolor[fg]{section in head/foot}%
    \\insertsectionnavigationhorizontal{0.99\\textwidth}{}{}
  \\end{beamercolorbox}%
}"
nb_n <- strsplit(nb_n, "\n")[[1]]
 
header <- if(navbullets) paste(  c(nb_y, paste("%",nb_n)  ), collapse="\n")
          else           paste(  c(paste("%",nb_y), nb_n  ), collapse="\n")

cat(
"% presentation aboutSomething
% Template by Berry Boessenkool, berry-b@gmx.de


% Make sure to set weaving to knitr before compiling:
% Rstudio - Tools - Global Options - Sweave - weave Rnw files using: knitr


\\documentclass[compress, xcolor=dvipsnames]{beamer} % handout option for non-animated slides
\\setbeamerfont{frametitle}{size=\\normalsize}

\\usepackage{hyperref, graphicx}
\\usepackage[dvipsnames]{xcolor}
\\renewcommand\\appendixname{Appendix}
\\usepackage[absolute,overlay,showboxes]{textpos}
\\hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
\\setbeamercolor{background canvas}{bg=",bgcolor,"}
\\setbeamercolor{normal text}{fg=",txcolor,"}
% \\setbeamercolor{item}{fg=green}
% \\beamertemplatenavigationsymbolsempty
\\setbeamertemplate{navigation symbols}[only frame symbol]
%\\usetheme{Madrid}
% Navigation slide bullets in header ON / OFF:\n",
header,
"
\\beamersetleftmargin{0.5cm}
\\beamersetrightmargin{0.5cm}
\\let\\Tiny=\\tiny % avoid warning: Font shape `OT1/cmss/m/n' in size <4> not available. size <5> substituted on input line
\\setbeamertemplate{footline}[frame number]
\\setbeamertemplate{footline}[text line]{%
  \\parbox{\\linewidth}{\\vspace*{-12pt}
  \\textcolor{",txcolor,"}{
   % \\scriptsize
  ~~ Berry Boessenkool, ",curmonth,":
  NiceFooterTitle ~~~~~
  \\href{https://github.com/brry/course\\#slides}{github.com/brry/course} \\hfill
  ~~ \\insertframenumber / \\inserttotalframenumber~~~~~~~~~}}}

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
\\color{",txcolor,"}
\\centering


% ---------------------------

\\section{intro}

% ---------------------------

<<setup, include=FALSE>>=
opts_chunk$set(cache=T, echo=TRUE, fig.height=3.3, fig.width=5, out.width='0.9\\\\textwidth')
@

% ---------------------------

\\begin{frame}%[plain]
\\TPshowboxesfalse % no border around this box
\\begin{textblock*}{18em}(10pt,30pt) % topleft corner x=10pt, y=30pt. width=18em
\\centering
\\vspace{1.5em}
\\Large \\textbf{NiceTitle\\\\[1.0em] \\large SubTitle about}
\\includegraphics[width=1.2em]{fig_extern/Rlogo.png}
\\end{textblock*}
\\normalsize
\\vspace{9em}
Berry Boessenkool, ",curmonth,"\\\\[1em]
\\texttt{berry-b@gmx.de}\\\\[1em]
\\href{https://github.com/brry/rdwd\\#rdwd}{github.com/brry/rdwd}\\\\
\\href{https://cran.r-project.org/package=extremeStat/vignettes/extremeStat.html}{cran.r-project.org/package=extremeStat}\\\\[1em]
\\scriptsize
\\textit{Presentation template generated with} \\rcode{berryFunctions::createPres}\\\\
\\normalsize
\\TPshowboxesfalse % no border around this box
\\only<2-3>{ % photography note and licence
\\begin{textblock*}{8em}(250pt,30pt) % topleft corner x=250pt, y=30pt
\\centering
\\textblockcolour{Dandelion}
\\vspace{0.5em}
\\includegraphics[width=3em]{fig_extern/camera2.png}\\\\
\\footnotesize
ENCOURAGED\\\\[0.5em]%
\\onslide<3>{
\\includegraphics[width=6em]{fig_extern/ccby.png}\\\\[-0.3em]
\\href{https://creativecommons.org/licenses/by/4.0}{use freely, cite me}%
}
\\normalsize
\\vspace{0.5em}
\\end{textblock*}
\\TPshowboxestrue % borders around other boxes, as specified by \\usepackage[...,showboxes]{textpos}
} % end camera + licence
\\textblockcolour{} % reset block fill color to none/transparent
\\end{frame}


% ---------------------------

\\begin{frame}[fragile]{Frametitle}
<<chunkname, size=\"footnotesize\"", ifelse(bgblack,", fig.height=3","") , ">>=",
if(bgblack) "\npar(fg=\"white\")", "
plot(rnorm(1000))
@
\\end{frame}

% ---------------------------

\\section{moreStuff}

% ---------------------------

\\begin{frame}[fragile]{Frametitle}
\\textblockrulecolour{red}
\\pause
\\begin{itemize}[<+->]
\\item ItemOne
\\item ItemTwo
\\end{itemize}
\\onslide<+->
\\begin{flushleft} SomeMore \\end{flushleft}
\\only<5>{
\\begin{textblock*}{1.2cm}(2.0cm,3.6cm)
\\vspace{1.1cm} ~
\\end{textblock*}
}
\\end{frame}

% ---------------------------

\\begin{frame}{NoCodeNoFragile}
R package \\rcode{rdwd} ~~$->$~~ easy usage of weather datasets
\\end{frame}

% ---------------------------

\\begin{frame}{includegraphics}
\\includegraphics[height=0.85\\textheight]{fig_extern/MyFig.pdf}
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

\\end{document}", file=paste0(dir, "/", presname, ".Rnw"), sep="")
# Open file if wanted:
if(open) openFile(paste0(dir, "/", presname, ".Rnw"))
}
