ci <- function(
   dat,
   lev=.95,
   digits=3)
{
round(data.frame(  CI.lower = t.test(dat, conf.level=lev)$conf.int[1] ,
                   CI.upper = t.test(dat, conf.level=lev)$conf.int[2] ,
                   level    = lev                                ),digits)
}
