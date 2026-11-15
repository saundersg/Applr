# CASE: text_equations
# lm_equation() and lm_latex() across representative models. Both outputs
# are judged as text; expectations check what each string must contain.

models <- list(
  simple = lm(mpg ~ wt, mtcars),
  multiple = lm(mpg ~ disp + hp, mtcars),
  transformed = lm(log(mpg) ~ wt + I(wt^2), mtcars),
  no_intercept = lm(mpg ~ 0 + wt, mtcars),
  rank_deficient = lm(mpg ~ wt + I(2 * wt), mtcars),
  exact_fit = lm(y ~ x, data.frame(x = 1:5, y = 2 * (1:5))),
  intercept_only = lm(mpg ~ 1, mtcars),
  underscore_name = lm(mpg ~ wt_lb, transform(mtcars, wt_lb = wt * 1000))
)
equations <- vapply(models, lm_equation, "")
latex <- vapply(models, function(m) {
  out <- capture.output(v <- lm_latex(m))
  v
}, "")

judged_output("equations", equations, type = "returned_text",
              covers = c("lm_equation.format", "lm_equation.no-intercept", "lm_equation.na-and-residue", "lm_equation.intercept-only"),
              purpose = "lm_equation() on the awkward model shapes students meet (no intercept, rank-deficient, exact fit, intercept-only) so you can judge each reads naturally; the intercept-only one is a known bug.")
judged_output("latex", latex, type = "returned_text",
              covers = c("lm_latex.format", "lm_latex.special-characters"),
              purpose = "lm_latex() on the same shapes: the underbraced form must read well typeset, and you can see the known unescaped underscore and empty intercept-only term.")

expectation("rounding_and_signs",
            covers = "lm_equation.format",
            purpose = "Catches coefficients not rounded to 3 significant figures, '+ -' instead of '- ', or a transformed response losing its name.", {
  co <- signif(coef(models$multiple), 3)
  expect_equal(equations[["multiple"]],
               paste0("mpg = ", co[[1]], " - ", abs(co[["disp"]]), "*disp - ", abs(co[["hp"]]), "*hp"))
  expect_equal(equations[["transformed"]], sub("^", "", equations[["transformed"]]))
  expect_match(equations[["transformed"]], "^log\\(mpg\\) = .*\\*I\\(wt\\^2\\)$")
})

expectation("intercept_na_residue",
            covers = c("lm_equation.no-intercept", "lm_equation.na-and-residue"),
            purpose = "Catches an intercept appearing in a no-intercept model, 'NA' printed for an aliased term, or floating-point residue like 2e-15 shown instead of 0.", {
  expect_match(equations[["no_intercept"]], "^mpg = -?[0-9.]+\\*wt$")
  expect_false(grepl("NA", equations[["rank_deficient"]], fixed = TRUE))
  expect_equal(equations[["exact_fit"]], "y = 0 + 2*x")
})

expectation("intercept_only_known_bug",
            covers = "lm_equation.intercept-only",
            purpose = "Pins a known bug ('mpg = 20.1 + *') so its fix is noticed: when this fails, expect 'mpg = 20.1'.", {
  # KNOWN BUG (for_devs/known_issues.Rmd): should be "mpg = 20.1".
  expect_equal(equations[["intercept_only"]], "mpg = 20.1 + *")
})

expectation("latex_underbraces",
            covers = "lm_latex.format",
            purpose = "Catches lm_latex() losing the 'Pred.' label, numbering terms from 0, or breaking the $$ wrapping that makes it render.", {
  expect_match(latex[["multiple"]], "^\\$\\$\\\\underbrace\\{\\\\hat\\{Y_i\\}\\}_\\{\\\\text\\{Pred\\. mpg\\}\\} = ")
  expect_match(latex[["multiple"]], "\\\\underbrace\\{X_\\{1i\\}\\}_\\{\\\\text\\{disp\\}\\}")
  expect_match(latex[["multiple"]], "\\\\underbrace\\{X_\\{2i\\}\\}_\\{\\\\text\\{hp\\}\\}\\$\\$$")
})

expectation("underscore_known_issue",
            covers = "lm_latex.special-characters",
            purpose = "Pins a known issue (an underscore unescaped, which breaks LaTeX) so its fix is noticed: when this fails, expect \\_.", {
  # LaTeX needs \_ inside \text{}; this pins today's output. When it fails,
  # the name is escaped: expect \\_ instead.
  expect_match(latex[["underscore_name"]], "\\text{wt_lb}", fixed = TRUE)
})
