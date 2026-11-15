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

covers("shared.back-transform.every-engine", "shared.composite-x.one-predictor",
       "shared.slicing.factor-in-formula-pinned", "shared.model-data.dot-formula",
       "shared.slicing.two-aesthetics", "shared.slicing.numeric-facet",
       "geom_slice.full-range-beyond-data", "autoplot.transformed-terms",
       "autoplot.local-data", "shared.back-transform.outside-domain")

test_that("exp(y), y^2, sqrt(y) and 1/y responses are inverted the same way by both engines", {
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

test_that("an x-axis expression of one predictor predicts at the data rows, in both engines", {
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

test_that("a factor made in the formula is drawn per level once a grouping, predict_vars or autoplot() pins it", {
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

test_that("a `y ~ .` model slices like its spelled-out formula", {
  cars <- mtcars[c("mpg", "wt", "hp")]
  dot <- lm(mpg ~ ., data = cars)
  spelled <- lm(mpg ~ wt + hp, data = cars)
  drawn <- function(m) slice_data(ggplot2::ggplot(cars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10))
  expect_equal(drawn(dot), drawn(spelled))
  prep <- function(m) suppressMessages(prepare_slice_data(m, "wt", 10, TRUE, "none", list()))$line
  expect_equal(prep(dot), prep(spelled))
})

test_that("colour and linetype on two predictors give one line per pair, each pinned", {
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

test_that("a facet on a numeric predictor pins each panel to its number", {
  m <- lm(mpg ~ disp + hp + cyl, mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + geom_slice(m, n = 10, predict_vars = list(hp = 110)) +
                    ggplot2::facet_wrap(~cyl))
  cyls <- sort(unique(mtcars$cyl))
  for (panel in unique(d$PANEL)) {
    line <- d[d$PANEL == panel, ]
    expect_equal(line$y, predicted(m, data.frame(disp = line$x, hp = 110, cyl = cyls[as.integer(panel)])))
  }
})

test_that("full_range under wider scale limits predicts past the data", {
  m <- lm(mpg ~ wt, mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 9, full_range = TRUE) +
                    ggplot2::scale_x_continuous(limits = c(0, 8)))
  expect_equal(d$x, seq(0, 8, length.out = 9))
  expect_equal(d$y, predicted(m, data.frame(wt = d$x)))
})

test_that("autoplot() puts the raw response and predictor on the axes of a transformed model", {
  m <- lm(log(mpg) ~ log(disp), mtcars)
  p <- suppressMessages(autoplot(m, summary = FALSE))
  expect_equal(c(expr_of(p, "x"), expr_of(p, "y")), c("disp", "mpg"))
  d <- slice_data(p)
  expect_equal(d$y, exp(predicted(m, data.frame(disp = d$x))))
})

test_that("autoplot() finds data that exists only where the model was fitted", {
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

test_that("where a back-transform has no value the line has none there, with no Applr warning (known issue)", {
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
