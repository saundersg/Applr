# CASE: num_model_shapes
# Model shapes and plot set-ups from the salvage audit: exp and power
# responses in both engines, an x-axis expression of one predictor, a factor
# made inside the formula, `y ~ .`, two grouping aesthetics, a numeric facet,
# prediction past the data, autoplot() on transformed and locally fitted
# models, and a back-transform with no value.

mpg <- ggplot2::mpg
set.seed(3)
pos <- data.frame(x = runif(40, 1, 10), w = runif(40))
pos$y <- sqrt(pos$x) + pos$w + rnorm(40, sd = 0.05)
expr_of <- function(p, aes) rlang::as_label(p$mapping[[aes]])

expectation("every_engine_inverts",
            covers = "shared.back-transform.every-engine",
            purpose = "Catches exp(y) or a power response, which take their own branches of the inverse, being undone differently by geom_slice() and slice_2d(); num_back_transform covers the common log/sqrt cases.", {
  cases <- list(
    list(lm(exp(y) ~ x + w, pos), log),
    list(lm(y^2 ~ x + w, pos), sqrt),
    list(lm(sqrt(y) ~ x + w, pos), function(v) v^2),
    list(lm(1 / y ~ x + w, pos), function(v) 1 / v)
  )
  for (cs in cases) {
    label <- deparse(formula(cs[[1]])[[2]])
    d <- slice_data(ggplot2::ggplot(pos, ggplot2::aes(x, y)) + geom_slice(cs[[1]], n = 15, predict_vars = list(w = 0.5)))
    expect_equal(d$y, cs[[2]](predicted(cs[[1]], data.frame(x = d$x, w = 0.5))), label = label)
    prep <- prepare_slice_data(cs[[1]], "x", 15, TRUE, "none", list(w = 0.5))
    expect_equal(prep$line$fit, cs[[2]](predicted(cs[[1]], data.frame(x = prep$line$x, w = 0.5))), label = label)
  }
})

expectation("one_predictor_expression",
            covers = "shared.composite-x.one-predictor",
            purpose = "Catches an x-axis like log(disp) being treated as a plain variable with an even grid; unlike num_composite_x, only one predictor is involved, which takes a different branch.", {
  m <- lm(mpg ~ log(disp) + hp, mtcars)
  rows <- mtcars[order(mtcars$disp), ]
  rows$hp <- 120
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(log(disp), mpg)) + geom_slice(m, n = 5, predict_vars = list(hp = 120)))
  expect_equal(d$x, log(rows$disp))
  expect_equal(d$y, predicted(m, rows))
  prep <- prepare_slice_data(m, "log(disp)", 5, TRUE, "none", list(hp = 120))
  expect_equal(prep$line$x, log(rows$disp))
  expect_equal(prep$line$fit, predicted(m, rows))
})

expectation("formula_factor_pinned",
            covers = "shared.slicing.factor-in-formula-pinned",
            purpose = "Catches factor(cyl) made inside the formula breaking the line even when a grouping, predict_vars or autoplot() says which level to draw; num_imputation pins the unpinned case.", {
  m <- lm(mpg ~ wt + as.factor(cyl), mtcars)
  cyls <- sort(unique(mtcars$cyl))
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = as.factor(cyl))) + geom_slice(m, n = 10))
  for (g in unique(d$group)) {
    line <- d[d$group == g, ]
    expect_equal(line$y, predicted(m, data.frame(wt = line$x, cyl = cyls[g])))
  }
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10, predict_vars = list(cyl = 6)))
  expect_equal(d$y, predicted(m, data.frame(wt = d$x, cyl = 6)))
  p <- suppressMessages(autoplot(m, summary = FALSE))
  expect_equal(expr_of(p, "colour"), "factor(cyl)")
  expect_equal(length(unique(slice_data(p)$group)), 3)
})

expectation("dot_formula",
            covers = "shared.model-data.dot-formula",
            purpose = "Catches y ~ . models losing their predictors when the raw variables are recovered from the formula, so they slice differently from the spelled-out model.", {
  cars <- mtcars[c("mpg", "wt", "hp")]
  dot <- lm(mpg ~ ., data = cars)
  spelled <- lm(mpg ~ wt + hp, data = cars)
  drawn <- function(m) slice_data(ggplot2::ggplot(cars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10))
  expect_equal(drawn(dot), drawn(spelled))
  prep <- function(m) suppressMessages(prepare_slice_data(m, "wt", 10, TRUE, "none", list()))$line
  expect_equal(prep(dot), prep(spelled))
})

expectation("two_aesthetics",
            covers = "shared.slicing.two-aesthetics",
            purpose = "Catches colour and linetype on two predictors pinning only one of them, or drawing lines for pairs that never occur.", {
  m <- lm(hwy ~ displ + cyl + year, data = mpg)
  d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = factor(cyl), linetype = factor(year))) +
                    geom_slice(m, n = 10))
  pairs <- unique(d[c("group", "colour", "linetype")])
  expect_equal(nrow(pairs), nrow(unique(mpg[c("cyl", "year")])))
  cyl_of <- setNames(sort(unique(mpg$cyl)), unique(d$colour[order(d$group)]))
  year_of <- setNames(sort(unique(mpg$year)), unique(d$linetype[order(d$group)]))
  for (g in pairs$group) {
    line <- d[d$group == g, ]
    expect_equal(line$y, predicted(m, data.frame(displ = line$x, cyl = cyl_of[[line$colour[1]]],
                                                 year = year_of[[line$linetype[1]]])))
  }
})

expectation("numeric_facet",
            covers = "shared.slicing.numeric-facet",
            purpose = "Catches a facet on a numeric predictor being treated as text, so panels aren't pinned to their numbers; num_slicing covers factor facets.", {
  m <- lm(mpg ~ disp + hp + cyl, mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + geom_slice(m, n = 10, predict_vars = list(hp = 110)) +
                    ggplot2::facet_wrap(~cyl))
  cyls <- sort(unique(mtcars$cyl))
  for (panel in unique(d$PANEL)) {
    line <- d[d$PANEL == panel, ]
    expect_equal(line$y, predicted(m, data.frame(disp = line$x, hp = 110, cyl = cyls[as.integer(panel)])))
  }
})

expectation("full_range_past_data",
            covers = "geom_slice.full-range-beyond-data",
            purpose = "Catches full_range stopping at the data when the user widened the scale limits, which is exactly when they want the line extrapolated.", {
  m <- lm(mpg ~ wt, mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 9, full_range = TRUE) +
                    ggplot2::scale_x_continuous(limits = c(0, 8)))
  expect_equal(d$x, seq(0, 8, length.out = 9))
  expect_equal(d$y, predicted(m, data.frame(wt = d$x)))
})

expectation("autoplot_transformed_axes",
            covers = "autoplot.transformed-terms",
            purpose = "Catches autoplot() putting log(mpg) or log(disp) on the axes instead of the raw variables a student recognises.", {
  m <- lm(log(mpg) ~ log(disp), mtcars)
  p <- suppressMessages(autoplot(m, summary = FALSE))
  expect_equal(c(expr_of(p, "x"), expr_of(p, "y")), c("disp", "mpg"))
  d <- slice_data(p)
  expect_equal(d$y, exp(predicted(m, data.frame(disp = d$x))))
})

expectation("autoplot_local_data",
            covers = "autoplot.local-data",
            purpose = "Catches autoplot() failing on a model fitted inside a function or local(), whose data isn't visible where autoplot() is called.", {
  m <- local({
    class_data <- data.frame(x = c(1, 3, 2, 5, 4, 6, 8, 7, 9, 10))
    class_data$y <- 2 * class_data$x + c(0.5, -0.3, 0.2, 0.1, -0.4, 0.3, -0.2, 0.4, -0.1, 0)
    lm(y ~ x, data = class_data)
  })
  expect_false(exists("class_data"))
  p <- autoplot(m, summary = FALSE)
  expect_equal(nrow(ggplot2::layer_data(p, 1)), 10)
  d <- slice_data(p)
  expect_equal(d$y, predicted(m, data.frame(x = d$x)))
})

expectation("outside_domain_known_issue",
            covers = "shared.back-transform.outside-domain",
            purpose = "Pins a known issue: where the back-transform has no value, the line silently has a gap and Applr says nothing. If Applr starts warning, this fails and should be updated.", {
  # KNOWN ISSUE (for_devs/known_issues.Rmd): sqrt() of a negative y^2
  # prediction is NaN. slice_2d() drops those points without a word;
  # geom_slice() only through ggplot2's "Removed rows" warning at draw time.
  # This pins today's build-time behaviour; when Applr warns, expect it here.
  s <- data.frame(x = 1:10, y = c(0.3, 0.3, 0.4, 0.5, 2, 3, 3.5, 4, 4.4, 4.8))
  m <- lm(y^2 ~ x, s)
  r <- conditions_of(function() ggplot2::ggplot(s, ggplot2::aes(x, y)) + geom_slice(m, n = 10))
  expect_equal(r$kinds, "message")
  d <- r$value$data[[1]]
  raw <- predicted(m, data.frame(x = d$x))
  expect_true(any(raw < 0))
  expect_true(all(is.na(d$y[raw < 0])))
  expect_equal(d$y[raw >= 0], sqrt(raw[raw >= 0]))
  prep <- prepare_slice_data(m, "x", 10, TRUE, "none", list())
  expect_true(all(is.na(prep$line$fit[predicted(m, data.frame(x = prep$line$x)) < 0])))
})
