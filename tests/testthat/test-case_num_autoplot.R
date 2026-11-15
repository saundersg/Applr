# CASE: num_autoplot
# What autoplot() builds: the model's data as points, a geom_slice() line
# with a confidence ribbon, the axes and groupings it chooses, and a
# subtitle.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ + drv + cyl, data = mpg)
quiet_autoplot <- function(...) suppressMessages(autoplot(..., summary = FALSE))
expr_of <- function(p, aes) rlang::as_label(p$mapping[[aes]])

expectation("complete_plot",
            covers = "autoplot.complete-plot",
            purpose = "Catches autoplot() returning something other than an ordinary, extendable ggplot: it must have points, a slice whose ribbon is the real confidence interval, a subtitle, and accept +.", {
  p <- quiet_autoplot(lm(mpg ~ wt + hp, mtcars))
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[2]]$stat, "StatSlice")
  d <- slice_data(p)
  ci <- predict(lm(mpg ~ wt + hp, mtcars), data.frame(wt = d$x, hp = mean(mtcars$hp)), interval = "confidence")
  expect_equal(d$ymin, unname(ci[, "lwr"]))
  expect_match(p$labels$subtitle, "^mpg = ")
  expect_equal((p + ggplot2::labs(title = "T"))$labels$title, "T")
})

expectation("summary_first",
            covers = "autoplot.summary-first",
            purpose = "Catches the summary going missing, or printing when the user turned it off with summary = FALSE.", {
  out <- capture.output(p <- suppressMessages(autoplot(lm(mpg ~ wt, mtcars))))
  expect_true(any(grepl("Residual standard error", out)))
  expect_length(capture.output(quiet_autoplot(lm(mpg ~ wt, mtcars))), 0)
})

expectation("x_first_numeric",
            covers = "autoplot.x-first-numeric",
            purpose = "Catches autoplot() putting a factor on x when a numeric predictor comes later, or ignoring a user's aes(x = ...).", {
  expect_equal(expr_of(quiet_autoplot(lm(hwy ~ drv + displ, mpg)), "x"), "displ")
  expect_equal(expr_of(quiet_autoplot(model, ggplot2::aes(x = cyl)), "x"), "cyl")
  expect_equal(expr_of(quiet_autoplot(model), "y"), "hwy")
})

expectation("colour_then_facet",
            covers = "autoplot.categorical-to-colour-facet",
            purpose = "Catches the wrong predictor becoming colour or facet, a few-valued numeric not being wrapped in factor(), or a user's colour mapping being replaced.", {
  p <- quiet_autoplot(model)
  expect_equal(expr_of(p, "colour"), "drv")
  expect_equal(names(p$facet$params$facets), "cyl")
  p <- quiet_autoplot(lm(hwy ~ displ + cyl, mpg))
  expect_equal(expr_of(p, "colour"), "factor(cyl)")
  p <- quiet_autoplot(model, ggplot2::aes(colour = year))
  expect_equal(expr_of(p, "colour"), "year")
})

expectation("mapping_any_column",
            covers = "autoplot.mapping-any-column",
            purpose = "Catches autoplot() dropping columns the model doesn't use, so a mapping like shape = factor(gear) would fail or lose rows.", {
  p <- quiet_autoplot(lm(mpg ~ wt, mtcars), ggplot2::aes(shape = factor(gear)))
  expect_equal(nrow(ggplot2::layer_data(p, 1)), nrow(mtcars))
  expect_equal(length(unique(ggplot2::layer_data(p, 1)$shape)), 3)
})

expectation("dots_reach_geom_slice",
            covers = "autoplot.dots-to-geom-slice",
            purpose = "Catches arguments in ... not reaching geom_slice(): predict_vars must move the line, and a band must replace the default ribbon rather than error.", {
  d <- slice_data(quiet_autoplot(lm(mpg ~ wt + hp, mtcars), interval = "none",
                                 predict_vars = list(hp = 100)))
  expect_null(d$ymin)
  expect_equal(d$y, predicted(lm(mpg ~ wt + hp, mtcars), data.frame(wt = d$x, hp = 100)))
  d <- slice_data(quiet_autoplot(lm(mpg ~ wt + hp, mtcars), band = "hp"))
  a <- predicted(lm(mpg ~ wt + hp, mtcars), data.frame(wt = d$x, hp = min(mtcars$hp)))
  expect_equal(d$ymax, a)
})

expectation("facet_not_held",
            covers = "autoplot.facet-before-subtitle",
            purpose = "Catches the subtitle being added before the facet, which would report the facet variable as held at one value.", {
  s <- quiet_autoplot(model)$labels$subtitle
  expect_false(grepl("cyl =", s, fixed = TRUE))
})
