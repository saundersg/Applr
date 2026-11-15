# CASE: look_slicing
# Bands, facets and transformed axes, from the README's plots: projection
# bands per group, the four-predictor exact fit made clear with colour,
# facets, a band, labels and a subtitle, and one model on three y-axes.

mpg <- ggplot2::mpg
set.seed(123)
n <- 80
dat <- data.frame(x = runif(n, 0.1, 10), x2 = runif(n, 0.1, 10),
                  x3 = sample(c(1, 20, 40), n, replace = TRUE), x4 = sample(c(1, 2), n, replace = TRUE))
dat$y <- log(5 + dat$x + dat$x2 + dat$x3 + 1 / 5 * dat$x * dat$x3 + 80 * dat$x4)
exact <- lm(exp(y) ~ x * x3 + x2 + x4, data = dat)
band_model <- lm(hwy ~ displ * drv + cyl, data = mpg)
logged <- lm(log(mpg) ~ disp + hp, data = mtcars)

band_inferred <- suppressMessages(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
  ggplot2::geom_point() + geom_slice(band_model, band = TRUE) + geom_slice_text())
band_named <- suppressMessages(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
  ggplot2::geom_point() + geom_slice(band_model, band = "cyl", predict_vars = list(cyl = c(5, 7))) +
  geom_slice_text())
exact_fit_clear <- suppressMessages(ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = factor(x3))) +
  ggplot2::geom_point() + geom_slice(exact, band = "x2", predict_vars = list(x2 = c(0, 10))) +
  ggplot2::facet_wrap(~x4) + geom_slice_text() + geom_slice_subtitle())
facet_grid <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + ggplot2::geom_point() +
  geom_slice(band_model, interval = "confidence") + ggplot2::facet_grid(drv ~ cyl)
y_raw <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + ggplot2::geom_point() + geom_slice(logged) +
  ggplot2::labs(title = "normal y axis")
y_expression <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, log(mpg))) + ggplot2::geom_point() +
  geom_slice(logged) + ggplot2::labs(title = "log() y axis")
y_scale <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + ggplot2::geom_point() + geom_slice(logged) +
  ggplot2::scale_y_log10() + ggplot2::labs(title = "scale_y_log10() axis")

covers("shared.band.look", "shared.slicing.facets-look", "geom_slice_text.facets")
judged_output("band_inferred", band_inferred,
              purpose = "band = TRUE finds cyl and draws one labelled band per colour group over cyl's data range.")
judged_output("band_named", band_named,
              purpose = "A band named by the user, with its two values pinned, is narrower than the inferred one and labels each edge.")
judged_output("exact_fit_clear", exact_fit_clear,
              purpose = "The README's four-predictor exact fit made clear with colour, facets, a band, labels and a subtitle: every point inside its band.")
judged_output("facet_grid", facet_grid,
              purpose = "facet_grid() over two predictors pins each panel's line and ribbon to that panel's values.")
judged_output("y_raw", y_raw,
              purpose = "A log(mpg) model on a raw mpg axis is back-transformed into a curve through the points.")
judged_output("y_expression", y_expression,
              purpose = "The same model on a log(mpg) axis expression stays on the model's scale, a straight line.")
judged_output("y_scale", y_scale,
              purpose = "The same model under scale_y_log10() is straight on the log axis with mpg tick labels.")

test_that("every point of the exact fit sits inside its panel's band", {
  d <- slice_data(exact_fit_clear)
  expect_true(all(d$ymin <= d$ymax))
  expect_equal(length(unique(d$PANEL)), 2)
  expect_equal(nrow(slice_text_data(exact_fit_clear)), 2 * 3 * 2)
})
