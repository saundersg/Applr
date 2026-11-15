# CASE: cond_build
# Conditions geom_slice() raises once the plot is added or built: the plot's
# data and axes, groupings that can't pin, and values the plot can't use.

mpg <- ggplot2::mpg
model <- lm(mpg ~ wt + hp, data = mtcars)
on_mtcars <- function(mapping, ...) function() ggplot2::ggplot(mtcars, mapping) + geom_slice(...)

raises("plot_data_mismatch", function() {
  ggplot2::ggplot(mtcars[1:20, ], ggplot2::aes(wt, mpg)) + geom_slice(model)
}, "error", "The data on the plot is not the data `model` was fitted to.",
       covers = c("shared.model-data.plot-data-mismatch", "geom_slice.model-name", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Plotting a subset of the model's data would draw a slice of other data; the stop must name the model as the user called it (`model`). The only output showing that naming.")
raises("plot_data_reordered", function() {
  ggplot2::ggplot(mtcars[32:1, ], ggplot2::aes(wt, mpg)) + geom_slice(model, predict_vars = list(hp = 100))
}, character(),
       covers = c("shared.model-data.plot-data-mismatch", "shared.conditions.severity"),
       purpose = "Catches the data check being too strict: the same rows in another order are the same data and must draw with nothing said.")
raises("model_data_gone", function() {
  fit <- function() { d <- mtcars; m <- lm(mpg ~ wt, data = d); rm(d); m }
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(fit())
}, "error", "The data the model was fitted on could not be found.",
       covers = c("shared.model-data.fitted-in-function", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A model fitted inside a function whose data has gone; without this check users get an obscure eval error instead of 'could not be found'.")
# Fitted without `data`, the model's variables are found in the formula's
# environment, so the slice is drawn with nothing to say.
raises("no_data_argument_recovered", function() {
  wt <- mtcars$wt
  mpg <- mtcars$mpg
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(lm(mpg ~ wt))
}, character(),
       covers = c("shared.model-data.no-data-arg", "shared.conditions.severity"),
       purpose = "Catches geom_slice() refusing a model fitted without `data` when its variables can still be found; unlike cond_autoplot/no_data_argument, this one must draw silently.")
raises("axis_unmapped", function() ggplot2::ggplot(mtcars, ggplot2::aes(wt)) + geom_slice(model),
       "error", "needs both `x` and `y` mapped",
       covers = c("geom_slice.unmapped-axis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A plot with no y mapped: the stop must name both axes and show a full aes(), since geom_slice() takes no mapping of its own.")
raises("inherit_aes_false", on_mtcars(ggplot2::aes(wt, mpg), model, inherit.aes = FALSE),
       "error", "needs both `x` and `y` mapped",
       covers = c("geom_slice.unmapped-axis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "inherit.aes = FALSE removes the plot's axes from the layer, a less obvious way to reach axis_unmapped; the message is the same, which you can judge is still helpful here.")
# Raised inside the stat, so ggplot2 turns the error into a "Computation
# failed" warning and draws the plot without the line.
raises("discrete_x", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(drv, hwy)) + geom_slice(lm(hwy ~ drv, mpg))
}, "warning", "needs a continuous x-axis", format = FALSE,
       covers = c("geom_slice.discrete-axis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A factor on x fails inside the stat, so ggplot2 turns Applr's error into a warning and the line just disappears; you can judge whether the wrapped wording is clear enough.")
raises("x_uses_no_predictor", on_mtcars(ggplot2::aes(drat, mpg), lm(mpg ~ wt, mtcars)),
       c("warning", "message"), "uses none of the model's predictors",
       covers = c("geom_slice.x-uses-no-predictor", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "An x-axis outside the model gives a flat line that looks like 'no effect'; the warning must say why it's flat and list the predictors.")
raises("bare_group", on_mtcars(ggplot2::aes(wt, mpg, group = cyl), lm(mpg ~ wt + cyl, mtcars)),
       c("warning", "message"), "cannot pin `cyl`",
       covers = c("shared.slicing.bare-group", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "group = cyl looks like it should give one line per cyl but can't pin values; the warning must say so and suggest colour.")
raises("continuous_colour", on_mtcars(ggplot2::aes(wt, mpg, colour = hp), model),
       c("warning", "message"), "is on a continuous scale",
       covers = c("shared.slicing.continuous-colour", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "colour = hp on a continuous scale can't split lines; unlike bare_group, the fix is factor(), which the hint must show.")
raises("predict_vars_on_x", on_mtcars(ggplot2::aes(wt, mpg), model, predict_vars = list(wt = 3, hp = 100)),
       "warning", "is ignored because it is on the x-axis",
       covers = c("shared.held-values.on-x-axis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Holding the x variable makes no sense, so the value is ignored; the warning must say why rather than leave the user wondering where their value went.")
raises("unsupported_class", function() {
  d <- data.frame(x = 1:6, flag = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), y = c(1, 3, 2, 5, 4, 6))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_slice(lm(y ~ x + flag, d))
}, "error", 'unsupported class "logical"',
       covers = c("shared.imputation.unsupported-class", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A logical predictor has no imputation rule; the stop must name the variable and its class, and list the classes that work.")
raises("raw_layer_no_mapping", function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
    ggplot2::layer(stat = StatSlice, geom = GeomSlice, position = "identity",
                   params = list(model = lm(mpg ~ disp, mtcars)))
}, "error", "needs both `x` and `y` mapped",
       covers = c("ggproto_objects.raw-layer-no-mapping", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "For extension authors building layer(stat = StatSlice) by hand: forgetting the mapping must give axis_unmapped's message, not an obscure failure.")
