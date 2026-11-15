# CASE: cond_validation
# Arguments checked before any plot is built: the model, n, interval,
# back_transform, full_range, a stray aes(), predict_vars and band. Each
# call's conditions are judged as wording.

model <- lm(mpg ~ wt + hp, data = mtcars)
cars <- mtcars
on_plot <- function(...) function() ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(...)

covers("shared.validation.model", "shared.validation.dollar-names", "shared.validation.n",
       "shared.validation.interval", "shared.validation.back-transform-arg",
       "geom_slice.stray-aes", "geom_slice.full-range-twice", "shared.held-values.malformed",
       "shared.held-values.bad-level", "shared.band.with-interval", "geom_slice.model-name",
       "shared.conditions.two-part-format", "shared.conditions.severity")

raises("no_model", function() geom_slice(), "error", "geom_slice() needs a fitted model.",
       purpose = "geom_slice() with no model stops when the layer is created, with a fit-then-call example.")
raises("not_lm", function() geom_slice(data.frame(x = 1)), "error", 'received a "data.frame"',
       purpose = "geom_slice() given a data frame instead of a model stops, naming the class it received.")
raises("dollar_names", function() geom_slice(lm(cars$mpg ~ cars$wt)), "error", "`cars$mpg`",
       purpose = "A model fitted with `$` in its variable names stops, since the plot's columns can't be matched to it.")
raises("n_too_small", function() geom_slice(model, n = 1), "error", "`n` must be",
       purpose = "An `n` of 1 can't draw a line, so geom_slice() stops.")
raises("interval_unknown", function() geom_slice(model, interval = "ci"), "error", "`interval` must be one of",
       purpose = "An unknown `interval` name stops, listing the choices.")
raises("back_transform_name", function() geom_slice(model, back_transform = "ln"), "error", '"ln" is not a known',
       purpose = "An unknown back_transform name stops, listing every known name.")
raises("back_transform_arity", function() geom_slice(model, back_transform = function(a, b) a), "error",
       "exactly one argument",
       purpose = "A back_transform function of two arguments stops, since it must take exactly one.")
raises("back_transform_class", function() geom_slice(model, back_transform = 2), "error", 'received a "numeric"',
       purpose = "A number given as back_transform stops, naming the class received.")
raises("stray_aes", function() geom_slice(model, ggplot2::aes(wt, mpg)), "error",
       "does not take an aesthetic mapping",
       purpose = "An aes() passed to geom_slice() stops, since the layer takes its mapping from the plot.")
raises("full_range_twice", function() geom_slice(model, full_range = TRUE, fullrange = TRUE), "error",
       "same argument",
       purpose = "full_range and its alias fullrange given together stop as the same argument twice.")
raises("full_range_not_logical", function() geom_slice(model, full_range = "yes"), "error",
       "`full_range` must be TRUE or FALSE.",
       purpose = "A string for full_range stops, since it must be TRUE or FALSE.")
raises("predict_vars_unnamed", function() geom_slice(model, predict_vars = list(100)), "error",
       "named list",
       purpose = "An unnamed predict_vars entry stops, since each value must name its variable.")
raises("predict_vars_response", function() geom_slice(model, predict_vars = list(mpg = 20)), "error",
       "`mpg` is the model's response",
       purpose = "Holding the response in predict_vars stops, since the response is what is predicted.")
raises("predict_vars_unknown", function() geom_slice(model, predict_vars = list(cyl = 4)), "error",
       '"cyl" not found in the model',
       purpose = "Holding a variable the model doesn't use stops, listing the predictors.")
raises("predict_vars_empty", function() geom_slice(model, predict_vars = list(hp = NULL, wt = list(1))), "error",
       "must be one or more values",
       purpose = "A predict_vars entry with no values (NULL) or a list stops, naming the variable.")
raises("predict_vars_bad_level", function() {
  ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length)) +
    geom_slice(lm(Sepal.Length ~ Petal.Length + Species, iris), predict_vars = list(Species = "rose"))
}, "error", '"rose" is not a level',
       purpose = "A held factor value that is not one of its levels stops at build time, listing the levels.")
# A character predictor isn't a factor, so the unknown value only fails in
# predict(); ggplot2 turns that into a warning and draws the plot without
# the line.
raises("predict_vars_unknown_character", function() {
  ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
    geom_slice(lm(hwy ~ displ + drv, ggplot2::mpg), predict_vars = list(drv = "x"))
}, "warning", "predict() failed for this slice", format = FALSE,
       purpose = "An unknown value for a character predictor only fails in predict(), so ggplot2 warns and draws the plot without the line.")
raises("predict_vars_not_number", on_plot(model, predict_vars = list(hp = "fast")), "error",
       '"fast" for the numeric variable `hp`',
       purpose = "A string held for a numeric predictor stops at build time, naming the variable.")
raises("band_wrong_type", function() geom_slice(model, band = 3), "error", "`band` must be TRUE, FALSE",
       purpose = "A number given as band stops, since it must be TRUE, FALSE or a variable name.")
raises("band_with_interval", function() geom_slice(model, band = TRUE, interval = "confidence"), "error",
       "cannot be combined",
       purpose = "A band and an interval asked for together stop, since both would draw a ribbon.")
