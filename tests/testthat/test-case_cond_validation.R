# CASE: cond_validation
# Arguments checked before any plot is built: the model, n, interval,
# back_transform, full_range, a stray aes(), predict_vars and band. Each
# call's conditions are judged as wording, against the two-part format and
# the severity rules; the kind raised and a key phrase are checked by raises().

model <- lm(mpg ~ wt + hp, data = mtcars)
cars <- mtcars
on_plot <- function(...) function() ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(...)

raises("no_model", function() geom_slice(), "error", "geom_slice() needs a fitted model.",
       covers = c("shared.validation.model", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The first mistake a beginner can make, calling geom_slice() before fitting anything; unlike not_lm there's nothing to name, so the fit-then-call hint carries the message.")
raises("not_lm", function() geom_slice(data.frame(x = 1)), "error", 'received a "data.frame"',
       covers = c("shared.validation.model", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Unlike no_model, something was passed, so the message must name what it was; a data frame is the likeliest wrong thing.")
raises("dollar_names", function() geom_slice(lm(cars$mpg ~ cars$wt)), "error", "`cars$mpg`",
       covers = c("shared.validation.dollar-names", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A model fitted as lm(cars$mpg ~ cars$wt) is common in teaching code and would otherwise fail obscurely later; the message must show the offending name and how to refit.")
raises("n_too_small", function() geom_slice(model, n = 1), "error", "`n` must be",
       covers = c("shared.validation.n", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The only check of the `n` message: one point can't make a line, and the hint must give a usable n.")
raises("interval_unknown", function() geom_slice(model, interval = "ci"), "error", "`interval` must be one of",
       covers = c("shared.validation.interval", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "\"ci\" is a natural guess for a confidence interval; the message must list the three real choices.")
raises("back_transform_name", function() geom_slice(model, back_transform = "ln"), "error", '"ln" is not a known',
       covers = c("shared.validation.back-transform-arg", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "\"ln\" is a plausible but unknown name; this is the one message that lists every known back_transform name.")
raises("back_transform_arity", function() geom_slice(model, back_transform = function(a, b) a), "error",
       "exactly one argument",
       covers = c("shared.validation.back-transform-arg", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A function is the right kind of value here but the wrong shape, so the message differs from back_transform_class: it must say one argument and show one.")
raises("back_transform_class", function() geom_slice(model, back_transform = 2), "error", 'received a "numeric"',
       covers = c("shared.validation.back-transform-arg", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The wrong kind of value entirely (a number), so the message must name the class received and the kinds allowed.")
raises("stray_aes", function() geom_slice(model, ggplot2::aes(wt, mpg)), "error",
       "does not take an aesthetic mapping",
       covers = c("geom_slice.stray-aes", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "ggplot users habitually pass aes() to a layer, where it would land in some other argument; the stop catches it first, and the hint must show where the mapping goes.")
raises("full_range_twice", function() geom_slice(model, full_range = TRUE, fullrange = TRUE), "error",
       "same argument",
       covers = c("geom_slice.full-range-twice", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "full_range has an alias (fullrange, ggplot2's spelling); given both, the message must say they are one argument rather than pick one silently.")
raises("full_range_not_logical", function() geom_slice(model, full_range = "yes"), "error",
       "`full_range` must be TRUE or FALSE.",
       covers = c("geom_slice.full-range-twice", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "\"yes\" reads as true to a person but not to R; unlike full_range_twice, the hint must say what full_range = TRUE does.")
raises("predict_vars_unnamed", function() geom_slice(model, predict_vars = list(100)), "error",
       "named list",
       covers = c("shared.held-values.malformed", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A value with no variable name can't be held anywhere; the hint must show the variable = value form.")
raises("predict_vars_response", function() geom_slice(model, predict_vars = list(mpg = 20)), "error",
       "`mpg` is the model's response",
       covers = c("shared.held-values.malformed", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Holding the response is a conceptual slip (it is what's predicted), not a typo, so the message explains rather than lists names.")
raises("predict_vars_unknown", function() geom_slice(model, predict_vars = list(cyl = 4)), "error",
       '"cyl" not found in the model',
       covers = c("shared.held-values.malformed", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "cyl is in the data but not the model, an easy mix-up; the hint must list the predictors the model does have.")
raises("predict_vars_empty", function() geom_slice(model, predict_vars = list(hp = NULL, wt = list(1))), "error",
       "must be one or more values",
       covers = c("shared.held-values.malformed", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A NULL value (often left from code that built the list) names a real predictor but holds nothing; the hint shows one value and several.")
raises("predict_vars_bad_level", function() {
  ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length)) +
    geom_slice(lm(Sepal.Length ~ Petal.Length + Species, iris), predict_vars = list(Species = "rose"))
}, "error", '"rose" is not a level',
       covers = c("shared.held-values.bad-level", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A misspelt or invented factor level stops only when the plot is built, so the message arrives wrapped by ggplot2; Applr's part must still list the levels.")
# A character predictor isn't a factor, so the unknown value only fails in
# predict(); ggplot2 turns that into a warning and draws the plot without
# the line.
raises("predict_vars_unknown_character", function() {
  ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
    geom_slice(lm(hwy ~ displ + drv, ggplot2::mpg), predict_vars = list(drv = "x"))
}, "warning", "predict() failed for this slice", format = FALSE,
       covers = c("shared.held-values.bad-level", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The same mistake as predict_vars_bad_level on a character column instead of a factor: it only warns and the line goes missing, which you may judge too weak.")
raises("predict_vars_not_number", on_plot(model, predict_vars = list(hp = "fast")), "error",
       '"fast" for the numeric variable `hp`',
       covers = c("shared.held-values.bad-level", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The numeric half of the bad-value rule (predict_vars_bad_level is the factor half): the message must name hp and suggest a real number for it.")
raises("band_wrong_type", function() geom_slice(model, band = 3), "error", "`band` must be TRUE, FALSE",
       covers = c("shared.band.with-interval", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "band takes TRUE, FALSE or a name, which is unusual; given a number, the message must show both accepted forms.")
raises("band_with_interval", function() geom_slice(model, band = TRUE, interval = "confidence"), "error",
       "cannot be combined",
       covers = c("shared.band.with-interval", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Each argument is fine alone, but both would draw a ribbon; the hint must say which to drop for which result.")
