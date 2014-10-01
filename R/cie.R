cie <- function(
  dat,
  lev=.95,
  digits=3,
  p1=0.05,
  p2=0.95)
{
t(round(data.frame(  CI.lower = t.test(dat, conf.level=lev)$conf.int[1] ,
                     CI.upper = t.test(dat, conf.level=lev)$conf.int[2] ,
                     level    = lev ,
                     mean     = mean(dat, na.rm=TRUE) ,
                     sd       = sd(dat, na.rm=TRUE) ,
                     CV       = sd(dat, na.rm=TRUE)/mean(dat, na.rm=TRUE) ,
                     median   = median(dat, na.rm=TRUE) ,
                     Quant.p1 = quantile(dat, prob=p1, na.rm=TRUE) ,
                     Quant.p2 = quantile(dat, prob=p2, na.rm=TRUE) ,
                     p1       = p1 ,
                     p2       = p2 ,
                     min      = min(dat, na.rm=TRUE) ,
                     max      = max(dat, na.rm=TRUE)
       ),digits))
}
