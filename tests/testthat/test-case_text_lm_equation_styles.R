# CASE: text_lm_equation_styles
# lm_equation() on a model with a factor and an interaction, in each style.
# The three strings are judged together as text.

model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
equations <- c(
  prettier = lm_equation(model),
  brackets = lm_equation(model, style = "brackets"),
  raw = lm_equation(model, style = "raw")
)

judged_output("equations", equations, type = "returned_text",
              covers = "lm_equation.style",
              purpose = "The returned_text type's demo for the suite's own tests, and the one place all three equation styles sit side by side on a factor interaction, so you can compare how each reads.")

expectation("style_terms",
            covers = c("lm_equation.style", "lm_equation.terms-from-coefficients"),
            purpose = "Catches a style naming factor or interaction terms another style's way, or a partial style name like \"b\" not matching brackets.", {
  expect_match(equations[["prettier"]], '(Species="versicolor")', fixed = TRUE)
  expect_match(equations[["prettier"]], 'Sepal.Width:(Species="virginica")', fixed = TRUE)
  expect_match(equations[["brackets"]], "*[versicolor]", fixed = TRUE)
  expect_match(equations[["raw"]], "*Speciesversicolor", fixed = TRUE)
  expect_equal(lm_equation(model, style = "b"), equations[["brackets"]])
})

expectation("starts_with_response",
            covers = "lm_equation.format",
            purpose = "Catches the equation losing its response, mis-rounding the intercept, or writing '+ -' instead of '- '.", {
  co <- coef(model)
  expect_match(equations[["raw"]], paste0("^Sepal.Length = ", signif(co[[1]], 3), " "))
  expect_false(grepl("+ -", equations[["raw"]], fixed = TRUE))
})
