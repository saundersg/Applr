# CASE: num_intervals
# Confidence and prediction ribbons are predict()'s lwr and upr, mapped like
# the line, and stay upright under a decreasing back-transform.

model <- lm(hwy ~ displ + I(displ^2), data = ggplot2::mpg)
base <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy))

covers("shared.intervals.match-predict", "shared.back-transform.ribbons-upright")

# Title: geom_slice() ribbons
test_that("geom_slice() ribbons are predict()'s interval", {
  for (iv in c("confidence", "prediction")) {
    d <- slice_data(base + geom_slice(model, n = 10, interval = iv))
    e <- expected_line(model, list(displ = d$x), interval = iv)
    expect_equal(d$y, e$y)
    expect_equal(d$ymin, e$ymin)
    expect_equal(d$ymax, e$ymax)
  }
})

# Title: slice_2d() bands
test_that("slice_2d() bands are predict()'s interval", {
  prep <- prepare_slice_data(model, "displ", 10, TRUE, "prediction", list())
  e <- expected_line(model, list(displ = prep$line$x), interval = "prediction")
  expect_equal(prep$line$lwr, e$ymin)
  expect_equal(prep$line$upr, e$ymax)
})

# Title: Upright reversed ribbon
test_that("under 1/y the ribbon is back-transformed and stays upright", {
  m <- lm(1 / mpg ~ wt, data = mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10, interval = "confidence"))
  e <- expected_line(m, list(wt = d$x), to_y = function(v) 1 / v, interval = "confidence")
  expect_true(all(d$ymin <= d$y & d$y <= d$ymax))
  expect_equal(d$ymin, e$ymin)
  expect_equal(d$ymax, e$ymax)
  prep <- prepare_slice_data(m, "wt", 10, TRUE, "confidence", list())
  expect_true(all(prep$line$lwr <= prep$line$upr))
})
