# CASE: cond_misc
# Conditions from the smaller functions: lm_equation()'s style, diagnose(),
# and the deprecated drawit() and defunct geom_fit().

on_device <- function(code) function() {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  code()
}

raises("equation_bad_style", function() lm_equation(lm(mpg ~ wt, mtcars), style = "fancy"), "warning",
       'so "prettier" was used',
       covers = c("lm_equation.bad-style", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "lm_equation() called directly with an unknown style; the same warning as cond_companions/subtitle_bad_style but reached without a plot, where a stop might suit better.")
raises("diagnose_not_lm", function() diagnose(1), "error", "model must be an object of class 'lm'",
       format = FALSE,
       covers = c("diagnose.not-lm", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Pins diagnose()'s bare stop(), the one message in Applr not in the two-part format, so you can judge whether to bring it in line.")
raises("diagnose_three_rows", on_device(function() {
  diagnose(lm(y ~ x, data.frame(x = 1:3, y = c(1, 3, 2))))
}), character(),
       covers = c("diagnose.few-rows", "shared.conditions.severity"),
       purpose = "Catches diagnose() failing or labelling NA when the model has only three residuals to label.")
raises("geom_fit_defunct", function() geom_fit(lm(mpg ~ wt, mtcars)), "error",
       "`geom_fit()` has been removed; use `geom_slice()` instead.",
       covers = c("geom_fit.errors-with-pointer", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Old course material still calls geom_fit(); the stop must name geom_slice() and how each old argument maps, so students can update their code.")
raises("drawit_deprecated", on_device(function() drawit(lm(mpg ~ wt + hp, mtcars), "hp", wt = 3)),
       "warning", "`drawit()` is deprecated; use `slice_2d()` instead.",
       covers = c("drawit.warns-and-forwards", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "drawit() still appears in old scripts; the warning must give a slice_2d() call to copy and explain the one behaviour change.")
raises("drawit_no_xaxis", on_device(function() drawit(lm(mpg ~ disp, mtcars))),
       c("warning", "message"), "x_axis = \"wt\"",
       covers = c("drawit.no-xaxis", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Pins a wrong hint: without xaxis, the suggested call names \"wt\" even for a model on disp. When fixed, this output changes.")

expectation("drawit_draws_slice_2d",
            covers = "drawit.warns-and-forwards",
            purpose = "Catches drawit() drawing something other than slice_2d()'s line for the same model and held values, or dropping its deprecation warning.", {
  m <- lm(mpg ~ wt + hp, mtcars)
  a <- suppressWarnings(prepare_slice_data(m, "hp", 100, TRUE, "none", list(wt = 3)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_warning(drawit(m, "hp", wt = 3, col = "red"), "deprecated")
  expect_equal(a$line$fit, predicted(m, data.frame(hp = a$line$x, wt = 3)))
})
