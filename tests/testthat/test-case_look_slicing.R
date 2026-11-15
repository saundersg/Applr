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

judged_output("band_inferred", band_inferred,
              covers = "shared.band.look",
              purpose = "band = TRUE on coloured groups: each band must be one translucent ribbon in its line's colour with both edges drawn, and the labels must say which edge is which.")
judged_output("band_named", band_named,
              covers = "shared.band.look",
              purpose = "Next to band_inferred, a band over two chosen values must look narrower; catches the two looking the same.")
judged_output("exact_fit_clear", exact_fit_clear,
              covers = c("shared.slicing.facets-look", "geom_slice_text.facets"),
              purpose = "The README's showcase, every companion at once: colour, facets, band, labels and subtitle must all fit and read together.")
judged_output("facet_grid", facet_grid,
              covers = "shared.slicing.facets-look",
              purpose = "facet_grid() over two predictors, the busiest panel layout: each panel's line and ribbon must sit in that panel's own data.")
judged_output("y_raw", y_raw,
              covers = "shared.back-transform.auto",
              purpose = "One of three views of one log(mpg) model: on a raw axis the line must be a curve through the points.")
judged_output("y_expression", y_expression,
              covers = "shared.back-transform.axis-expression",
              purpose = "The second view: on a log(mpg) axis the same model must be a straight line.")
judged_output("y_scale", y_scale,
              covers = "geom_slice.scale-transforms",
              purpose = "The third view: under scale_y_log10() the line must be straight with mpg tick labels; the three side by side catch one view drifting.")

expectation("exact_fit_panels",
            covers = "geom_slice_text.facets",
            purpose = "Catches labels missing from a facet panel (there must be 12), a panel lost, or a band with its edges swapped. Despite its old name, it doesn't check points are inside the bands.", {
  d <- slice_data(exact_fit_clear)
  expect_true(all(d$ymin <= d$ymax))
  expect_equal(length(unique(d$PANEL)), 2)
  expect_equal(nrow(slice_text_data(exact_fit_clear)), 2 * 3 * 2)
})
