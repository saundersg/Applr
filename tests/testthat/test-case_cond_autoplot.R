# CASE: cond_autoplot
# autoplot()'s refusals and the choices it announces.

mpg <- ggplot2::mpg
quiet_summary <- function(...) function() autoplot(..., summary = FALSE)

raises("mapping_not_aes", quiet_summary(lm(mpg ~ wt, mtcars), mapping = list(x = "wt")), "error",
       "`mapping` must be created by aes().",
       covers = c("autoplot.input-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "ggplot users may pass mapping = list(x = \"wt\") or a string; this is the only check that the message points them to aes() before anything is built.")
raises("no_data_argument", function() {
  y <- mtcars$mpg
  x <- mtcars$wt
  autoplot(lm(y ~ x), summary = FALSE)
}, "error", "fitted without a `data` argument",
       covers = c("autoplot.input-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A model fitted as lm(y ~ x) from loose vectors is common in intro courses, and autoplot() can't find its data; the message must say why and show the refit.")
raises("two_response_variables", quiet_summary(lm(I(mpg / hp) ~ wt, mtcars)), "error",
       "does not use exactly one variable",
       covers = c("autoplot.input-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A response built from two variables, I(mpg / hp), has no y-axis to choose; unlike the other refusals, the way out is to build the plot by hand.")
raises("no_predictors", quiet_summary(lm(mpg ~ 1, mtcars)), "error", "The model has no predictors",
       covers = c("autoplot.input-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "An intercept-only model, lm(y ~ 1), is a real teaching example with nothing to put on x; the message must say so rather than fail inside ggplot2.")
raises("no_numeric_predictor", quiet_summary(lm(hwy ~ drv + fl, mpg)), "error",
       "None of the model's predictors are numeric",
       covers = c("autoplot.input-errors", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Only factor predictors means no continuous x-axis; unlike no_predictors there are two ways out (refit, or map x), and the hint must offer both.")
raises("announces_choices", quiet_summary(lm(hwy ~ drv + displ + cyl + year, mpg)),
       c("message", "message", "message", "message"),
       c("The first predictor `drv` is not numeric - used `displ`", "Coloured the plot by `drv`",
         "Faceted the plot by `cyl`", "Value for `year` not specified"),
       covers = c("autoplot.x-first-numeric", "autoplot.categorical-to-colour-facet", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The one place every choice autoplot() makes on the user's behalf (x-axis, colour, facet, imputed value) is announced together, in order, each with how to change it.")
