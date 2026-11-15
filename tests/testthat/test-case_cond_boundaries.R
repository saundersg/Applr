# CASE: cond_boundaries
# Conditions at the edges of what a model can show: a back-transform taken
# outside its domain, an interval with no residual degrees of freedom, and
# every entry point refusing a bad model or back_transform the same way.

covers("shared.back-transform.outside-domain", "shared.intervals.no-residual-df",
       "shared.validation.every-entry-point")

# exp(y) predictions go negative at small x, where log() has no value
low <- data.frame(x = 1:10, y = log(c(1, 1.2, 1.5, 2, 3, 5, 8, 13, 21, 34)))
two_rows <- data.frame(x = c(1, 2), y = c(1, 3))

raises("log_of_negative_prediction", function() {
  ggplot2::ggplot(low, ggplot2::aes(x, y)) + geom_slice(lm(exp(y) ~ x, low))
}, c("message", "warning"), "NaNs produced", format = FALSE,
       purpose = "A back-transform taken outside its domain (log of negative exp(y) predictions) shows what users get today: bare base R and ggplot2 warnings.")
raises("interval_from_two_rows", function() {
  ggplot2::ggplot(two_rows, ggplot2::aes(x, y)) + geom_slice(lm(y ~ x, two_rows), interval = "confidence")
}, "warning", "NaNs produced", format = FALSE,
       purpose = "A confidence interval on a two-row model, with no residual degrees of freedom, shows base R's bare 'NaNs produced' while the plot still builds.")

test_that("every entry point refuses a non-lm model, `$` names and an unknown back_transform alike", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(mpg ~ wt, mtcars)
  error_of <- function(call) {
    r <- conditions_of(call)
    expect_equal(r$kinds, "error")
    r$messages
  }
  dollar <- lm(mtcars$mpg ~ mtcars$wt)
  on_plot <- function(...) function() ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(...)
  entry <- list(
    geom_slice = list(model = on_plot, back = function(m) on_plot(m, back_transform = "ln")),
    slice_2d = list(model = function(m) function() slice_2d(m), back = function(m) function() slice_2d(m, back_transform = "ln")),
    add_slice_2d = list(model = function(m) function() add_slice_2d(m),
                        back = function(m) function() add_slice_2d(m, back_transform = "ln")),
    autoplot = list(model = function(m) function() autoplot(m, summary = FALSE),
                    back = function(m) function() autoplot(m, back_transform = "ln", summary = FALSE))
  )
  for (fn in names(entry)) {
    expect_match(error_of(entry[[fn]]$model(dollar)), "Models with `$` in their variable names", fixed = TRUE)
    expect_match(error_of(entry[[fn]]$back(lm(mpg ~ wt, mtcars))), '"ln" is not a known `back_transform` name.',
                 fixed = TRUE)
  }
  for (fn in c("slice_2d", "add_slice_2d")) {
    expect_match(error_of(function() get(fn)(data.frame(x = 1))),
                 paste0("received a \"data.frame\".\n    Fit a model first, such as 'model <- lm(y ~ x, data = your_data)', then call '",
                        fn, "(model)'."), fixed = TRUE)
  }
  expect_match(error_of(function() scatter_3d(lm(mtcars$mpg ~ mtcars$wt + mtcars$hp))),
               "Models with `$` in their variable names", fixed = TRUE)
  # KNOWN BUG (for_devs/known_issues.Rmd): scatter_3d() checks the model as
  # geom_slice() does, so its hint names the wrong function. When this
  # fails, expect 'scatter_3d(model)'.
  expect_match(error_of(function() scatter_3d(data.frame(x = 1))), "then call 'geom_slice(model)'.", fixed = TRUE)
})
