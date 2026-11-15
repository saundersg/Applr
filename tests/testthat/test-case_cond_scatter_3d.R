# CASE: cond_scatter_3d
# scatter_3d()'s refusals, and that a good model builds with no plotly
# warnings.

covers("scatter_3d.shape-errors", "scatter_3d.predict-fails", "scatter_3d.no-build-warnings",
       "shared.validation.function-name")

built <- function(...) function() plotly::plotly_build(scatter_3d(...))

raises("one_predictor", built(lm(mpg ~ wt, mtcars)), "error", "needs exactly two predictor variables",
       purpose = "scatter_3d() refuses a one-predictor model, which has no surface to draw.")
raises("three_predictors", built(lm(mpg ~ wt + hp + qsec, mtcars)), "error", "the model has 3",
       purpose = "scatter_3d() refuses a three-predictor model, saying how many it has.")
raises("two_response_variables", built(lm(I(mpg + hp) ~ wt + qsec, mtcars)), "error",
       "needs a single response variable",
       purpose = "scatter_3d() refuses a response built from two variables, which has no single z-axis.")
raises("factor_predictor", built(lm(Sepal.Length ~ Sepal.Width + Species, iris)), "error",
       'but `Species` is a "factor"',
       purpose = "scatter_3d() refuses a factor predictor, which has no axis of its own, and points to geom_slice().")
raises("factor_in_formula", built(lm(mpg ~ wt + factor(cyl), mtcars)), "error",
       "could not evaluate the model across `wt` and `cyl`",
       purpose = "A predictor converted inside the formula, factor(cyl), fails to predict across the grid, so scatter_3d() stops with a refit hint.")
raises("not_a_model", built(NULL), "error", "needs a fitted model",
       purpose = "scatter_3d(NULL) should name scatter_3d() in its error; today it says geom_slice() needs a fitted model.")
raises("builds_quietly", built(lm(mpg ~ wt + hp, mtcars), n = 10), character(),
       purpose = "A good two-predictor model builds its widget with no plotly warnings.")
