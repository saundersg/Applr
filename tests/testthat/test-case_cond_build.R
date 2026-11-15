# CASE: cond_build
# Conditions geom_slice() raises once the plot is added or built: the plot's
# data and axes, groupings that can't pin, and values the plot can't use.

mpg <- ggplot2::mpg
model <- lm(mpg ~ wt + hp, data = mtcars)
on_mtcars <- function(mapping, ...) function() ggplot2::ggplot(mtcars, mapping) + geom_slice(...)

covers("shared.model-data.plot-data-mismatch", "shared.model-data.no-data-arg",
       "shared.model-data.fitted-in-function", "geom_slice.unmapped-axis",
       "geom_slice.discrete-axis", "geom_slice.x-uses-no-predictor", "shared.slicing.bare-group",
       "shared.slicing.continuous-colour", "shared.held-values.on-x-axis",
       "shared.imputation.unsupported-class", "ggproto_objects.raw-layer-no-mapping")

raises("plot_data_mismatch", function() {
  ggplot2::ggplot(mtcars[1:20, ], ggplot2::aes(wt, mpg)) + geom_slice(model)
}, "error", "The data on the plot is not the data `model` was fitted to.",
       purpose = "A plot drawn on a subset of the model's data stops, naming the model, because the slice would describe other data.")
raises("plot_data_reordered", function() {
  ggplot2::ggplot(mtcars[32:1, ], ggplot2::aes(wt, mpg)) + geom_slice(model, predict_vars = list(hp = 100))
}, character(),
       purpose = "The model's own rows in another order are the same data, so the slice is drawn with nothing to say.")
raises("model_data_gone", function() {
  fit <- function() { d <- mtcars; m <- lm(mpg ~ wt, data = d); rm(d); m }
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(fit())
}, "error", "The data the model was fitted on could not be found.",
       purpose = "A model fitted inside a function whose data no longer exists stops with 'could not be found' rather than an obscure error.")
# Fitted without `data`, the model's variables are found in the formula's
# environment, so the slice is drawn with nothing to say.
raises("no_data_argument_recovered", function() {
  wt <- mtcars$wt
  mpg <- mtcars$mpg
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(lm(mpg ~ wt))
}, character(),
       purpose = "A model fitted without `data` is recovered from its formula's environment, so geom_slice() draws it silently.")
raises("axis_unmapped", function() ggplot2::ggplot(mtcars, ggplot2::aes(wt)) + geom_slice(model),
       "error", "needs both `x` and `y` mapped",
       purpose = "A plot with no y mapped stops geom_slice(), which needs both axes.")
raises("inherit_aes_false", on_mtcars(ggplot2::aes(wt, mpg), model, inherit.aes = FALSE),
       "error", "needs both `x` and `y` mapped",
       purpose = "inherit.aes = FALSE leaves geom_slice() without the plot's x and y, so it stops as if they were unmapped.")
# Raised inside the stat, so ggplot2 turns the error into a "Computation
# failed" warning and draws the plot without the line.
raises("discrete_x", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(drv, hwy)) + geom_slice(lm(hwy ~ drv, mpg))
}, "warning", "needs a continuous x-axis", format = FALSE,
       purpose = "A discrete x-axis fails inside the stat, so ggplot2 turns the error into a warning and draws the plot without the line.")
raises("x_uses_no_predictor", on_mtcars(ggplot2::aes(drat, mpg), lm(mpg ~ wt, mtcars)),
       c("warning", "message"), "uses none of the model's predictors",
       purpose = "An x-axis that is none of the model's predictors warns that the line will be flat and lists the predictors.")
raises("bare_group", on_mtcars(ggplot2::aes(wt, mpg, group = cyl), lm(mpg ~ wt + cyl, mtcars)),
       c("warning", "message"), "cannot pin `cyl`",
       purpose = "group = cyl can't pin cyl's value for each line, so it warns and suggests colour instead.")
raises("continuous_colour", on_mtcars(ggplot2::aes(wt, mpg, colour = hp), model),
       c("warning", "message"), "is on a continuous scale",
       purpose = "A continuous colour scale can't split lines by value, so it warns and suggests factor().")
raises("predict_vars_on_x", on_mtcars(ggplot2::aes(wt, mpg), model, predict_vars = list(wt = 3, hp = 100)),
       "warning", "is ignored because it is on the x-axis",
       purpose = "A predict_vars value for the x-axis variable is ignored, with a warning saying why.")
raises("unsupported_class", function() {
  d <- data.frame(x = 1:6, flag = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), y = c(1, 3, 2, 5, 4, 6))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_slice(lm(y ~ x + flag, d))
}, "error", 'unsupported class "logical"',
       purpose = "A logical predictor has no imputation rule, so the slice stops naming the variable and the class.")
raises("raw_layer_no_mapping", function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
    ggplot2::layer(stat = StatSlice, geom = GeomSlice, position = "identity",
                   params = list(model = lm(mpg ~ disp, mtcars)))
}, "error", "needs both `x` and `y` mapped",
       purpose = "A raw layer() with StatSlice and no mapping in params stops asking for x and y rather than failing obscurely.")
