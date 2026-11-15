# CASE: num_misc
# Smaller computed outputs: raw variables recovered from transformed terms,
# add_slice_2d(), the raw StatSlice layer, a glm, lm_latex()'s return and
# diagnose()'s graphical parameters.

expectation("raw_variable",
            covers = "shared.model-data.raw-variables",
            purpose = "Catches lm(y ~ I(x^2)) being sliced along the column I(x^2) instead of along x.", {
  m <- lm(mpg ~ I(wt^2) + log(hp), mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10))
  expect_equal(d$y, predicted(m, data.frame(wt = d$x, hp = mean(mtcars$hp))))
})

expectation("add_slice_2d_same_line",
            covers = "add_slice_2d.line-is-prediction",
            purpose = "Catches add_slice_2d() drawing a different line from the one slice_2d() would for the same arguments.", {
  m <- lm(mpg ~ disp + hp, mtcars)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(mpg ~ disp, data = mtcars)
  usr <- graphics::par("usr")
  expect_silent(add_slice_2d(m, x_axis = "disp", hp = 150))
  expect_equal(graphics::par("usr"), usr)
  a <- prepare_slice_data(m, "disp", 100, TRUE, "none", list(hp = 150), fn = "add_slice_2d")
  b <- prepare_slice_data(m, "disp", 100, TRUE, "none", list(hp = 150), fn = "slice_2d")
  expect_equal(a$line, b$line)
  expect_equal(a$line$fit, predicted(m, data.frame(disp = a$line$x, hp = 150)))
})

expectation("raw_layer",
            covers = "ggproto_objects.raw-layer",
            purpose = "Catches StatSlice used directly through layer() drifting from what geom_slice() draws, for users and extension authors who build layers by hand.", {
  m <- lm(mpg ~ disp + hp + cyl, mtcars)
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg))
  raw <- base + ggplot2::layer(stat = StatSlice, geom = GeomSlice, position = "identity",
                               inherit.aes = TRUE,
                               params = list(model = m, predict_vars = list(hp = 110),
                                             mapping = ggplot2::aes(disp, mpg)))
  layered <- base + geom_slice(m, predict_vars = list(hp = 110))
  expect_equal(slice_data(raw)[c("x", "y")], slice_data(layered)[c("x", "y")])
})

expectation("glm_link_scale",
            covers = "shared.validation.glm",
            purpose = "Catches a glm() being refused, or sliced on a scale other than the link scale predict.glm() gives by default.", {
  m <- glm(am ~ wt, family = binomial, data = mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, am)) + geom_slice(m, n = 10))
  expect_equal(d$y, predicted(m, data.frame(wt = d$x)))
})

expectation("lm_latex_returns",
            covers = "lm_latex.prints-and-returns",
            purpose = "Catches lm_latex() printing without returning its string, or returning it visibly so it prints twice.", {
  m <- lm(mpg ~ wt, mtcars)
  out <- capture.output(v <- withVisible(lm_latex(m)))
  expect_false(v$visible)
  expect_equal(out, v$value)
  expect_match(v$value, "^\\$\\$.*\\$\\$$")
})

expectation("diagnose_restores_mfrow",
            covers = "diagnose.restores-par",
            purpose = "Catches diagnose() leaving the device split into panels, so the user's next plot comes out small.", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::par(mfrow = c(2, 2))
  diagnose(lm(mpg ~ wt, mtcars))
  expect_equal(graphics::par("mfrow"), c(2, 2))
})
