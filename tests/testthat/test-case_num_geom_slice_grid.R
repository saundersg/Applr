# CASE: num_geom_slice_grid
# Where geom_slice() puts its prediction points: n of them, spanning each
# group's own x range unless full_range asks for the panel's.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv, data = mpg)

expectation("n_points",
            covers = "geom_slice.n-points",
            purpose = "Catches n being ignored or off by one, on a single line (the default is 100, so n = 7 makes a change obvious).", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(lm(mpg ~ wt, mtcars), n = 7)
  expect_equal(nrow(slice_data(p)), 7)
})

expectation("range_per_group",
            covers = "geom_slice.range-per-group",
            purpose = "Catches a group's line stretching across the whole panel, past that group's data, which would also stretch the y-axis.", {
  p <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(model)
  d <- slice_data(p)
  for (g in unique(d$group)) {
    lvl <- levels(factor(mpg$drv))[g]
    expect_equal(range(d$x[d$group == g]), range(mpg$displ[mpg$drv == lvl]))
  }
})

expectation("full_range",
            covers = "geom_slice.full-range",
            purpose = "Catches full_range, or its ggplot2 spelling fullrange, failing to extend every line to the panel's range.", {
  for (layer in list(geom_slice(model, full_range = TRUE), geom_slice(model, fullrange = TRUE))) {
    p <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + layer
    d <- slice_data(p)
    for (g in unique(d$group)) expect_equal(range(d$x[d$group == g]), range(mpg$displ))
  }
})

expectation("na_in_x",
            covers = "geom_slice.na-in-x",
            purpose = "Catches a missing x value turning the grid's range into NA, so no line would be drawn.", {
  df <- data.frame(x = c(1, 2, NA, 4, 5), y = c(2, 4, 5, 8, 11))
  m <- lm(y ~ x, data = df)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + geom_slice(m)
  d <- suppressWarnings(slice_data(p))
  expect_equal(range(d$x), c(1, 5))
  expect_equal(d$y, predicted(m, data.frame(x = d$x)))
})

expectation("empty_group",
            covers = "geom_slice.empty-group",
            purpose = "Catches a factor level with no rows breaking the layer, or shifting the lines of the levels that do have rows.", {
  df <- data.frame(x = rep(1:5, 2), g = factor(rep(c("a", "b"), each = 5), levels = c("a", "b", "c")))
  df$y <- df$x * ifelse(df$g == "a", 1, 2)
  m <- lm(y ~ x * g, data = df)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) + geom_slice(m)
  d <- slice_data(p)
  expect_equal(sort(unique(d$group)), 1:2)
  expect_equal(d$y[d$group == 2], predicted(m, data.frame(x = d$x[d$group == 2], g = "b")))
})
