# CASE: num_intervals
# Confidence and prediction ribbons are predict()'s lwr and upr, mapped like
# the line, and stay upright under a decreasing back-transform.

model <- lm(hwy ~ displ + I(displ^2), data = ggplot2::mpg)
base <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy))

expectation("geom_slice_ribbons",
            covers = "shared.intervals.match-predict",
            purpose = "Catches geom_slice()'s ribbon edges differing from predict()'s lwr and upr, for both confidence and prediction intervals.", {
  for (iv in c("confidence", "prediction")) {
    d <- slice_data(base + geom_slice(model, n = 10, interval = iv))
    p <- predict(model, data.frame(displ = d$x), interval = iv)
    expect_equal(d$y, unname(p[, "fit"]))
    expect_equal(d$ymin, unname(p[, "lwr"]))
    expect_equal(d$ymax, unname(p[, "upr"]))
  }
})

expectation("slice_2d_bands",
            covers = "shared.intervals.match-predict",
            purpose = "The same rule through slice_2d(), which computes its bands separately from geom_slice().", {
  prep <- prepare_slice_data(model, "displ", 10, TRUE, "prediction", list())
  p <- predict(model, data.frame(displ = prep$line$x), interval = "prediction")
  expect_equal(prep$line$lwr, unname(p[, "lwr"]))
  expect_equal(prep$line$upr, unname(p[, "upr"]))
})

expectation("ribbon_upright",
            covers = "shared.back-transform.ribbons-upright",
            purpose = "Catches a decreasing back-transform (1/y) flipping the ribbon so its lower edge sits above its upper edge.", {
  m <- lm(1 / mpg ~ wt, data = mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10, interval = "confidence"))
  p <- 1 / predict(m, data.frame(wt = d$x), interval = "confidence")
  expect_true(all(d$ymin <= d$y & d$y <= d$ymax))
  expect_equal(d$ymin, unname(p[, "upr"]))
  prep <- prepare_slice_data(m, "wt", 10, TRUE, "confidence", list())
  expect_true(all(prep$line$lwr <= prep$line$upr))
})
