# CASE: cond_scatter_3d
# scatter_3d()'s refusals, and that a good model builds with no plotly
# warnings.

built <- function(...) function() plotly::plotly_build(scatter_3d(...))

raises("one_predictor", built(lm(mpg ~ wt, mtcars)), "error", "needs exactly two predictor variables",
       covers = c("scatter_3d.shape-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A one-predictor model has no surface; the stop must say exactly two are needed and how many the model has.")
raises("three_predictors", built(lm(mpg ~ wt + hp + qsec, mtcars)), "error", "the model has 3",
       covers = c("scatter_3d.shape-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The same message as one_predictor with a count of 3; catches the count being hard-coded or the check accepting more than two.")
raises("two_response_variables", built(lm(I(mpg + hp) ~ wt + qsec, mtcars)), "error",
       "needs a single response variable",
       covers = c("scatter_3d.shape-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A response built from two variables has no z-axis; the stop must name the response and how many variables it uses.")
raises("factor_predictor", built(lm(Sepal.Length ~ Sepal.Width + Species, iris)), "error",
       'but `Species` is a "factor"',
       covers = c("scatter_3d.shape-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A factor has no axis in 3D; the stop must name it and point to geom_slice(), which shows categories as groups.")
raises("factor_in_formula", built(lm(mpg ~ wt + factor(cyl), mtcars)), "error",
       "could not evaluate the model across `wt` and `cyl`",
       covers = c("scatter_3d.predict-fails", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "factor(cyl) inside the formula breaks prediction on a grid; the stop must explain why and show refitting with transformed data.")
raises("not_a_model", built(NULL), "error", "needs a fitted model",
       covers = c("shared.validation.function-name", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Pins a wrong name: scatter_3d(NULL) says geom_slice() needs a model. When fixed, this output changes.")
raises("builds_quietly", built(lm(mpg ~ wt + hp, mtcars), n = 10), character(),
       covers = c("scatter_3d.no-build-warnings", "shared.conditions.severity"),
       purpose = "Catches plotly warnings leaking into a normal build, which students read as errors; the only scatter_3d() output that must say nothing.")
