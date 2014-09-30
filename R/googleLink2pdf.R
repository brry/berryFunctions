googleLink2pdf <- function(
                           googlelink
                           )
{
pdflink <- strsplit(googlelink, "&url=")[[1]][2]
pdflink <- strsplit(pdflink, "&ei=")[[1]][1]
pdflink <- gsub("%24", "$", pdflink)
pdflink <- gsub("%2F", "/", pdflink)
pdflink <- gsub("%3A", ":", pdflink)
pdflink <- gsub("%25", "%", pdflink)
pdflink
}

