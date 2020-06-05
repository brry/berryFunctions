context("OSMscale testing")


test_that("earthDist returns zero for duplicated points", {
d <- read.table(header=TRUE, sep=",", text="
lat, long
52.514687,  13.350012   # Berlin
51.503162,  -0.131082   # London
35.685024, 139.753365") # Tokio


expect_equivalent( earthDist(lat, long, d),
                   c(0, 928.155746060382, 8922.21945323624)  )
expect_equivalent( earthDist(lat, long, d, i=2),
                   c(928.155746060382, 0, 9561.9302850532)   )

expect_equivalent( earthDist(lat=rep(51.503162,2), long=rep(-0.131082,2)),
                   rep(0,2) )
expect_equivalent( earthDist(lat=rep(54.0028,2), long=rep(11.1908,2)),
                   rep(0,2) )

})


test_that("earthDist works for close points", {

expect_equivalent( earthDist(lat=c(53,53), long=c(12,12.01))[-1],
                   0.669187770881609 )

# map <- pointsMap(lat=c(53,53), long=c(12,12.01))#, proj=putm(long=12))

})
