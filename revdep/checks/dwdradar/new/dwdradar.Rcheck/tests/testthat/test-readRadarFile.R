# Note:
# I derived "true" range values from versions of the reading function that seem to make sense.
# They are NOT actually checked with DWD, reality or anything!

test_dim_and_vals <- function(x, min, max, ncol, nrow, plot=interactive())
  {
  expect_equal(range(x$dat, na.rm=TRUE), c(min, max) )
  expect_equal(ncol(x$dat), ncol)
  expect_equal(nrow(x$dat), nrow)
  expect_length(x$meta, 12)
  if(plot)
  raster::plot(raster::raster(x$dat),
               main=c(as.character(x$meta$date), x$meta$filename)    )
  }

test_that("readRadarFile works for RW @ GRIDBASE/hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz/onefile", {
w1 <- readRadarFile("../raa01-rw2017.002_10000-1712310850-dwd---bin_hourRadReproc")
test_dim_and_vals(w1, 0.0, 6.2, 900, 1100)
})

test_that("readRadarFile works for RW @ GRIDBASE/hourly/radolan/recent/bin/onefile", {
fz <- "../raa01-rw_10000-1907311350-dwd---bin_hourRadRecentBin.gz"
fu <- R.utils::gunzip(fz, remove=FALSE, skip=TRUE)
w2 <- readRadarFile(fu)
test_dim_and_vals(w2, 0.0, 72.6, 900, 900)
})

test_that("readRadarFile works for RW @ ftp://weather/radar/radolan/rw/onefile", {
rw <- readRadarFile("../raa01-rw_10000-1907010950-dwd---bin_weatherRadolan")
test_dim_and_vals(rw, 0.0, 30.7, 900, 900)
rw <- readRadarFile("../raa01-rw_10000-1910141450-dwd---bin")
test_dim_and_vals(rw, 0.0, 10.7, 900, 900)
})

test_that("readRadarFile works for RY @ ftp://weather/radar/radolan/ry/onefile", {
ry <- readRadarFile("../raa01-ry_10000-1910182355-dwd---bin")
test_dim_and_vals(ry, 0.0, 2.9, 900, 900)
})

test_that("readRadarFile works for SF @ ftp://weather/radar/radolan/sf/onefile", {
sf <- readRadarFile("../raa01-sf_10000-1910141950-dwd---bin")
test_dim_and_vals(sf, 0.0, 43.9, 900, 900)
})

test_that("readRadarFile works for SF @ GRIDBASE/daily/radolan/historical/bin/2016/SF201605.tar.gz/onefile", {
sf <- readRadarFile("../raa01-sf_10000-1605010450-dwd---bin_dailyRadHist")
test_dim_and_vals(sf, 0.0, 39.2, 900, 900)
})

test_that("readRadarFile works for RX for a personally obtained non-public file", {
rx <- readRadarFile("../raa01-rx_10000-1605290600-dwd---bin_Braunsbach")
test_dim_and_vals(rx, 31.5, 95.0, 900, 900)
})
