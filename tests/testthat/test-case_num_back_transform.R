# CASE: num_back_transform
# Predictions of a transformed response land on the plotted y-axis: inverted
# for a raw axis, mapped for an axis expression, or as back_transform says.

grid_of <- function(d) data.frame(disp = d$x, hp = mean(mtcars$hp))
on_axis <- function(model, y, ...) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, {{ y }})) + geom_slice(model, n = 15, ...)
  slice_data(p)
}

expectation("inverted_on_raw_axis",
            covers = "shared.back-transform.auto",
            purpose = "Catches a transformed response being drawn in transformed units on a raw y-axis, for each common transform, in both geom_slice() and slice_2d().", {
  cases <- list(
    list(lm(log(mpg) ~ disp + hp, mtcars), exp),
    list(lm(sqrt(mpg) ~ disp + hp, mtcars), function(v) v^2),
    list(lm(1 / mpg ~ disp + hp, mtcars), function(v) 1 / v),
    list(lm(log10(mpg) ~ disp + hp, mtcars), function(v) 10^v),
    list(lm(mpg + 1 ~ disp + hp, mtcars), function(v) v - 1)
  )
  for (cs in cases) {
    d <- on_axis(cs[[1]], mpg)
    expect_equal(d$y, cs[[2]](predicted(cs[[1]], grid_of(d))))
  }
  # slice_2d() does the same with no y-axis to read: it always plots raw mpg
  m <- cases[[1]][[1]]
  prep <- suppressMessages(prepare_slice_data(m, "disp", 15, TRUE, "none", list(hp = 120)))
  expect_equal(prep$line$fit, exp(predicted(m, data.frame(disp = prep$line$x, hp = 120))))
})

expectation("axis_expression_units",
            covers = "shared.back-transform.axis-expression",
            purpose = "Catches geom_slice() ignoring a y-axis like aes(y = log(mpg)): the line must be in the axis's units whether the model was fitted on log(mpg) or mpg.", {
  logged <- lm(log(mpg) ~ disp + hp, mtcars)
  raw <- lm(mpg ~ disp + hp, mtcars)
  d <- on_axis(logged, log(mpg))
  expect_equal(d$y, predicted(logged, grid_of(d)))
  d <- on_axis(raw, log(mpg))
  expect_equal(d$y, log(predicted(raw, grid_of(d))))
})

expectation("override",
            covers = "shared.back-transform.override",
            purpose = "Catches back_transform being ignored: FALSE, a function, every known name (in any case) and TRUE must each give exactly their own mapping.", {
  m <- lm(log(mpg) ~ disp + hp, mtcars)
  d <- on_axis(m, mpg, back_transform = FALSE)
  expect_equal(d$y, predicted(m, grid_of(d)))
  d <- on_axis(m, mpg, back_transform = function(v) v * 2)
  expect_equal(d$y, 2 * predicted(m, grid_of(d)))
  expect_equal(on_axis(m, mpg, back_transform = "LOG")$y, on_axis(m, mpg)$y)
  for (nm in c("log10", "log2", "sqrt", "exp", "inverse", "1/x")) {
    d <- on_axis(m, mpg, back_transform = nm)
    expect_equal(d$y, back_transform_names[[nm]](predicted(m, grid_of(d))), label = nm)
  }
  expect_equal(on_axis(m, mpg, back_transform = TRUE)$y, on_axis(m, mpg)$y)
})

expectation("nested",
            covers = "shared.back-transform.nested",
            purpose = "Catches nested transforms being undone in the wrong order or only partly, which simple ones like log(y) wouldn't reveal.", {
  cases <- list(
    list(lm(sqrt(log(mpg)) ~ disp + hp, mtcars), function(v) exp(v^2)),
    list(lm(log(mpg + 1) ~ disp + hp, mtcars), function(v) exp(v) - 1),
    list(lm(2 * log(mpg) ~ disp + hp, mtcars), function(v) exp(v / 2))
  )
  for (cs in cases) {
    d <- on_axis(cs[[1]], mpg)
    expect_equal(d$y, cs[[2]](predicted(cs[[1]], grid_of(d))))
  }
})

expectation("box_cox_known_bug",
            covers = "shared.back-transform.box-cox",
            purpose = "Pins a known bug (only the first box_cox point survives) so its fix is noticed: when this fails, the test should become the full inverse.", {
  # KNOWN BUG (for_devs/known_issues.Rmd): the inverse wraps a scalar
  # condition in ifelse(), so only the first prediction survives and the rest
  # are NA. This pins today's behaviour; when it fails, the bug is fixed and
  # the expectation should become the full inverse.
  box_cox <- function(y, lambda) if (lambda == 0) log(y) else (y^lambda - 1) / lambda
  m <- lm(box_cox(mpg, 0.5) ~ disp + hp, mtcars)
  d <- on_axis(m, mpg)
  expected <- (0.5 * predicted(m, grid_of(d)) + 1)^2
  expect_equal(d$y[1], expected[1])
  expect_true(all(is.na(d$y[-1])))
})
