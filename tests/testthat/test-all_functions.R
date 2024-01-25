skip_on_cran()
skip_if_offline()

n <- sample(seq(10,200, 5), 1)
occ <-
  data.frame(
    species = rep(sample(x = LETTERS, size = 5), times = n/5),
    decimalLongitude = runif(n = n, min = 12, max = 20),
    decimalLatitude = runif(n = n, min = -4, max = 4)
  )

out <- calculate_bias(x = occ, terrestrial = TRUE)

test_that("calculate_bias works", {
  expect_equal(length(out), 4)
  expect_s3_class(out, "sampbias")
})

test_that("plot.sampbias works", {
  p <- plot(out)
  expect_s3_class( p, "gg")
})

proj_out <- project_bias(out)
test_that("project_bias works", {
  expect_equal(length(proj_out), 1)
  expect_s4_class(proj_out, "SpatRaster")
})

map_out <- map_bias(proj_out)
test_that("map_bias works", {
  expect_s3_class(map_out, "gg")
})
