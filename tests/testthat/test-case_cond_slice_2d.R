# CASE: cond_slice_2d
# Conditions from the base graphics functions: the x-axis, data changed
# since the fit, a slice outside the plot, no plot open, and the deprecated
# xaxis argument.

model <- lm(mpg ~ wt + hp, data = mtcars)
on_device <- function(code) function() {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  code()
}

raises("x_axis_default", on_device(function() slice_2d(model, hp = 100)), "message",
       "X-axis not specified",
       covers = c("slice_2d.default-x-axis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "slice_2d() choosing the x-axis for the user; the message must say which one and how to choose. Note its hint lacks a final full stop, unlike other messages.")
raises("x_axis_not_string", function() slice_2d(model, x_axis = 1), "error",
       "`x_axis` must be the name of one model predictor",
       covers = c("slice_2d.x-axis-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A number given as x_axis, perhaps a column position; the stop must say it needs a predictor's name as a string.")
raises("x_axis_not_found", function() slice_2d(model, x_axis = "disp"), "error",
       '`x_axis` variable "disp" not found in the model.',
       covers = c("slice_2d.x-axis-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "x_axis naming a column outside the model; unlike x_axis_not_string, the hint must list the predictors.")
raises("x_axis_not_numeric", function() {
  slice_2d(lm(Sepal.Length ~ Species + Sepal.Width, iris), x_axis = "Species")
}, "error", '`x_axis` must be numeric; the class "factor"',
       covers = c("slice_2d.x-axis-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A factor x_axis can't carry a line; the stop must name its class and ask for a numeric variable.")
raises("data_changed", function() {
  cars <- mtcars
  m <- lm(mpg ~ wt, data = cars)
  cars$mpg <- cars$mpg + 1
  slice_2d(m, x_axis = "wt")
}, "error", "The data being plotted is not the data `m` was fitted to.",
       covers = c("shared.model-data.refit-needed", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Data edited after fitting (common in a notebook) would draw a line for data the model never saw; the stop must name the model as the user called it (`m`) and show the refit.")
raises("outside_plot", on_device(function() {
  plot(1:2, 1:2)
  add_slice_2d(model, x_axis = "wt", hp = 100)
}), "warning", "Slice is outside the current plot range",
       covers = c("add_slice_2d.outside-plot", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "add_slice_2d() on a plot whose window the line misses; the warning must say why the user may see nothing.")
raises("no_plot_open", on_device(function() add_slice_2d(model, x_axis = "wt", hp = 100)),
       c("warning", "error"), "plot.new has not been called yet", format = FALSE,
       covers = c("add_slice_2d.no-plot", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Pins what happens with no plot open: a misleading range warning, then base R's error. You can judge whether Applr should say 'no plot open' itself.")
raises("xaxis_deprecated", on_device(function() slice_2d(model, xaxis = "wt", hp = 100)),
       "warning", "The `xaxis` argument of `slice_2d()` is deprecated.",
       covers = c("shared.conditions.xaxis-deprecated", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Old code using xaxis must keep working with a warning naming the new spelling.")
raises("xaxis_deprecated_add", on_device(function() {
  plot(mpg ~ wt, mtcars)
  add_slice_2d(model, xaxis = "wt", hp = 100)
}), "warning", "The `xaxis` argument of `add_slice_2d()` is deprecated.",
       covers = c("shared.conditions.xaxis-deprecated", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "add_slice_2d()'s version; catches it naming slice_2d() in its warning, since the two share the check.")

expectation("x_axis_wins",
            covers = "shared.conditions.xaxis-deprecated",
            purpose = "Catches the old xaxis overriding the new x_axis when a user gives both, for example after a half-finished update.", {
  prep <- suppressWarnings(suppressMessages(resolve_deprecated_xaxis("hp", "wt", "slice_2d")))
  expect_equal(prep, "hp")
})
