# CASE: num_scale_transforms
# Transformed position scales: the grid is evenly spaced in panel units and
# predicted at data-unit x, and predictions land in panel units.

model <- lm(mpg ~ disp, data = mtcars)

expectation("log_x_grid",
            covers = "geom_slice.scale-transforms",
            purpose = "Catches an evenly spaced data-unit grid under scale_x_log10(), which bunches the points at the right of the log axis, or predictions at panel-unit x.", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + geom_slice(model, n = 20) +
    ggplot2::scale_x_log10()
  d <- slice_data(p)
  expect_equal(d$x, seq(log10(min(mtcars$disp)), log10(max(mtcars$disp)), length.out = 20))
  expect_equal(d$y, predicted(model, data.frame(disp = 10^d$x)))
})

expectation("log_y_panel_units",
            covers = "geom_slice.scale-transforms",
            purpose = "Catches predictions placed in data units under scale_y_log10(), which would put the line far from the points.", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + geom_slice(model, n = 20) +
    ggplot2::scale_y_log10()
  d <- slice_data(p)
  expect_equal(d$y, log10(predicted(model, data.frame(disp = d$x))))
})
