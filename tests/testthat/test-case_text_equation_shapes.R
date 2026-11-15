# CASE: text_equation_shapes
# lm_latex() and lm_equation() on factor terms, including factors made
# inside the formula, in each style; the response and term names as
# written; a subtitle with nothing held; and one whose pinned style keeps
# its names when narrow.

iris_model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
latex_of <- function(...) {
  out <- capture.output(v <- lm_latex(...))
  v
}

examples <- c(
  latex_prettier = latex_of(iris_model),
  latex_brackets = latex_of(iris_model, style = "brackets"),
  latex_raw = latex_of(iris_model, style = "raw"),
  equation_factor_call = lm_equation(lm(mpg ~ disp + factor(cyl), mtcars)),
  equation_as_factor_interaction = lm_equation(lm(mpg ~ wt * as.factor(am), mtcars))
)

judged_output("factor_terms", examples, type = "returned_text",
              covers = c("lm_latex.factor-styles", "lm_equation.response-and-term-names"),
              purpose = "The LaTeX version of the three styles, plus factors made inside the formula; text_lm_equation_styles shows the plain-text styles, so you can check the two read alike.")

expectation("latex_factor_styles",
            covers = "lm_latex.factor-styles",
            purpose = "Catches lm_latex() labelling factor terms differently from lm_equation() in any style, or the abbreviation \"bracket\" not resolving.", {
  expect_match(examples[["latex_prettier"]], '\\text{(Species="versicolor")}', fixed = TRUE)
  expect_match(examples[["latex_prettier"]], '\\text{Sepal.Width:(Species="virginica")}', fixed = TRUE)
  expect_match(examples[["latex_brackets"]], "\\text{[versicolor]}", fixed = TRUE)
  expect_match(examples[["latex_raw"]], "\\text{Sepal.Width:Speciesvirginica}", fixed = TRUE)
  expect_equal(latex_of(iris_model, style = "bracket"), examples[["latex_brackets"]])
})

expectation("formula_factor_names",
            covers = "lm_equation.response-and-term-names",
            purpose = "Catches factor(cyl) or as.factor(am) losing their call in the term name, which would hide that the factor was made in the formula.", {
  expect_match(examples[["equation_factor_call"]], '*(factor(cyl)="6")', fixed = TRUE)
  expect_match(examples[["equation_as_factor_interaction"]], 'wt:(as.factor(am)="1")', fixed = TRUE)
})

expectation("names_as_written",
            covers = "lm_equation.response-and-term-names",
            purpose = "Catches a transformed response (1/mpg, sqrt, exp, a power) or a term like wt:hp or I(wt * hp) being renamed instead of shown as the formula writes it.", {
  lhs <- function(f) sub(" = .*", "", lm_equation(lm(f, mtcars)))
  expect_equal(vapply(list(1 / mpg ~ wt, sqrt(mpg) ~ wt, exp(mpg / 10) ~ wt, mpg^2 ~ wt), lhs, ""),
               c("1/mpg", "sqrt(mpg)", "exp(mpg/10)", "mpg^2"))
  expect_match(lm_equation(lm(mpg ~ wt:hp, mtcars)), "\\*wt:hp$")
  expect_match(lm_equation(lm(mpg ~ I(wt * hp), mtcars)), "\\*I\\(wt \\* hp\\)$")
})

expectation("nothing_held_no_line",
            covers = "geom_slice_subtitle.held-line",
            purpose = "Catches a dangling 'held at:' line, or any extra text, when nothing is held.", {
  m <- lm(mpg ~ wt, mtcars)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m) + geom_slice_subtitle(wrap = FALSE)
  expect_identical(p$labels$subtitle, lm_equation(m))
})

expectation("pinned_style_kept",
            covers = "geom_slice_subtitle.style-pinned",
            purpose = "Catches a style the user chose being overridden by the automatic [level] shortening at a narrow width, or a shortening message appearing anyway.", {
  m <- lm(Sepal.Length ~ Sepal.Width * Species + Petal.Length, iris)
  r <- conditions_of(function() {
    ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length, colour = Species)) +
      geom_slice(m) + geom_slice_subtitle(wrap = 3.5, style = "prettier")
  })
  expect_equal(r$kinds, "message")
  expect_false(any(grepl("shortened", r$messages)))
  expect_false(grepl("[", r$value$plot$labels$subtitle, fixed = TRUE))
  expect_match(r$value$plot$labels$subtitle, '(Species="virginica")', fixed = TRUE)
})
