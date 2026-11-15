window.PROTO_CODE = {
  "look_autoplot/iris_quick": {
    "setup": ["build <- function(p) suppressMessages(p)", "set.seed(123)", "iris_quick <- build(autoplot(lm(Petal.Length ~ Sepal.Length + I(Sepal.Length^2):Species, iris), summary = FALSE))"],
    "call": "judged_output(\"iris_quick\", iris_quick)"
  },
  "look_autoplot/chosen": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ + drv + cyl, data = mpg)", "build <- function(p) suppressMessages(p)", "set.seed(123)", "chosen <- build(autoplot(model, summary = FALSE))"],
    "call": "judged_output(\"chosen\", chosen)"
  },
  "look_autoplot/mapped": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ + drv + cyl, data = mpg)", "build <- function(p) suppressMessages(p)", "set.seed(123)", "mapped <- build(autoplot(model, ggplot2::aes(colour = factor(cyl)), summary = FALSE) + ggplot2::facet_wrap(~drv))"],
    "call": "judged_output(\"mapped\", mapped)"
  },
  "look_autoplot/other_axis": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ + drv + cyl, data = mpg)", "build <- function(p) suppressMessages(p)", "set.seed(123)", "other_axis <- build(autoplot(model, ggplot2::aes(x = cyl, colour = factor(drv)), interval = \"none\", summary = FALSE) +\n  ggplot2::labs(title = \"The Same Model From a Different Angle\"))"],
    "call": "judged_output(\"other_axis\", other_axis)"
  },
  "look_autoplot/exact_composite": {
    "setup": ["set.seed(123)", "n <- 80", "dat <- data.frame(x = runif(n, 0.1, 10), x2 = runif(n, 0.1, 10),\n                  x3 = sample(c(1, 20, 40), n, replace = TRUE), x4 = sample(c(1, 2), n, replace = TRUE))", "dat$y <- log(5 + dat$x + dat$x2 + dat$x3 + 1 / 5 * dat$x * dat$x3 + 80 * dat$x4)", "exact <- lm(exp(y) ~ x * x3 + x2 + x4, data = dat)", "exact_composite <- ggplot2::ggplot(dat, ggplot2::aes(x = log(5 + x + x2 + x3 + 1 / 5 * x * x3 + 80 * x4), y)) +\n  ggplot2::geom_point() + geom_slice(exact) + ggplot2::labs(title = \"Same Model, Different X-Axis\")"],
    "call": "judged_output(\"exact_composite\", exact_composite)"
  },
  "look_autoplot/zigzag": {
    "setup": ["set.seed(123)", "zig <- data.frame(x = rep(1:10, 2), x2 = rep(c(1, 3), each = 10))", "zig$y <- zig$x + 2 * zig$x2 + c(0.3, -0.2)[(1:20 %% 2) + 1]", "zig_model <- lm(y ~ x + x2, data = zig)", "zigzag <- ggplot2::ggplot(zig, ggplot2::aes(x * x2, y)) + ggplot2::geom_point() + geom_slice(zig_model) +\n  ggplot2::labs(title = \"Predictions that aren't a function of x * x2\")"],
    "call": "judged_output(\"zigzag\", zigzag)"
  },
  "cond_band/band_range_message": {
    "setup": ["mpg <- ggplot2::mpg", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_range_message\", coloured(lm(hwy ~ displ * drv + cyl, mpg), band = \"cyl\"),\n       \"message\", \"Band range for `cyl` not specified\")"
  },
  "cond_band/band_true_several_multi": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_true_several_multi\", coloured(model, band = TRUE,\n                                           predict_vars = list(cyl = c(4, 8), year = c(1999, 2008))),\n       \"warning\", \"band = TRUE is ambiguous: several\")"
  },
  "cond_band/band_true_no_candidate": {
    "setup": ["mpg <- ggplot2::mpg", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_true_no_candidate\", coloured(lm(hwy ~ displ * drv, mpg), band = TRUE),\n       \"warning\", \"no variable is available\")"
  },
  "cond_band/band_true_several_candidates": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_true_several_candidates\", coloured(model, band = TRUE),\n       c(\"warning\", \"message\", \"message\"), \"band = TRUE is ambiguous: any of\")"
  },
  "cond_band/band_not_predictor": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_not_predictor\", coloured(model, band = \"cty\"),\n       c(\"warning\", \"message\", \"message\"), '\"cty\" is not a predictor')"
  },
  "cond_band/band_on_x": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_on_x\", coloured(model, band = \"displ\"),\n       c(\"warning\", \"message\", \"message\"), \"is on the x-axis\")"
  },
  "cond_band/band_pinned": {
    "setup": ["mpg <- ggplot2::mpg", "model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)", "coloured <- function(...) function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)\n}"],
    "call": "raises(\"band_pinned\", coloured(model, band = \"drv\"),\n       c(\"warning\", \"message\", \"message\"), \"used by the plot's grouping or faceting\")"
  },
  "cond_band/band_not_numeric": {
    "setup": "mpg <- ggplot2::mpg",
    "call": "raises(\"band_not_numeric\", function() {\n  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + geom_slice(lm(hwy ~ displ + drv, mpg), band = \"drv\")\n}, c(\"warning\", \"message\"), \"is not numeric, so it has no range\")"
  },
  "look_labels/colour_by_group": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "colour_by_group <- build(on_d(ggplot2::aes(x, y, colour = g)) +\n  geom_slice(model, predict_vars = list(x2 = c(0, 2, 4))) + geom_slice_text())"],
    "call": "judged_output(\"colour_by_group\", colour_by_group)"
  },
  "look_labels/legend_style": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "legend_style <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 2, 4), g = c(\"A\", \"B\"))) +\n  geom_slice_text(style = \"legend\"))"],
    "call": "judged_output(\"legend_style\", legend_style)"
  },
  "look_labels/legend_one_colour": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "legend_one_colour <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4)), colour = \"darkgreen\") +\n  geom_slice_text(style = \"legend\"))"],
    "call": "judged_output(\"legend_one_colour\", legend_one_colour)"
  },
  "look_labels/left_offset": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "left_offset <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) +\n  geom_slice_text(location = \"left\", offset = 15, color = \"black\"))"],
    "call": "judged_output(\"left_offset\", left_offset)"
  },
  "look_labels/manual_hjust": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "manual_hjust <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) +\n  geom_slice_text(hjust = 0.5, vjust = -0.5))"],
    "call": "judged_output(\"manual_hjust\", manual_hjust)"
  },
  "look_labels/no_expand": {
    "setup": ["set.seed(123)", "d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),\n                g = factor(sample(c(\"A\", \"B\"), 80, replace = TRUE)))", "d$y <- d$x * ifelse(d$g == \"A\", 0.5, 2) + ifelse(d$g == \"A\", 5, -5) + 4 * d$x2 + rnorm(80)", "model <- lm(y ~ x * g + x2, data = d)", "build <- function(p) suppressMessages(p)", "on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)", "no_expand <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) + geom_slice_text(expand = FALSE))"],
    "call": "judged_output(\"no_expand\", no_expand)"
  },
  "look_labels/subtitle": {
    "setup": ["set.seed(123)", "iris_model <- lm(Petal.Length ~ Sepal.Length + I(Sepal.Length^2):Species + Sepal.Width, iris)", "build <- function(p) suppressMessages(p)", "subtitle <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +\n  ggplot2::geom_point() + geom_slice(iris_model, interval = \"confidence\") + geom_slice_subtitle(wrap = 6))"],
    "call": "judged_output(\"subtitle\", subtitle)"
  },
  "look_labels/caption": {
    "setup": ["set.seed(123)", "iris_model <- lm(Petal.Length ~ Sepal.Length + I(Sepal.Length^2):Species + Sepal.Width, iris)", "build <- function(p) suppressMessages(p)", "caption <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +\n  ggplot2::geom_point() + geom_slice(iris_model, interval = \"confidence\") + geom_slice_caption(wrap = 6))"],
    "call": "judged_output(\"caption\", caption)"
  },
  "look_labels/subtitle_narrow": {
    "setup": ["set.seed(123)", "iris_model <- lm(Petal.Length ~ Sepal.Length + I(Sepal.Length^2):Species + Sepal.Width, iris)", "build <- function(p) suppressMessages(p)", "subtitle_narrow <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +\n  ggplot2::geom_point() + geom_slice(iris_model) + geom_slice_subtitle(wrap = 3.5) +\n  ggplot2::theme(plot.margin = ggplot2::margin(5, 250, 5, 5)))"],
    "call": "judged_output(\"subtitle_narrow\", subtitle_narrow)"
  }
};
