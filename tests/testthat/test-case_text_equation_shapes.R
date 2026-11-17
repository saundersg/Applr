# CASE: text_equation_shapes
# lm_latex() and lm_equation() on factor terms, including factors made
# inside the formula, in each style; the response and term names as
# written; a subtitle with nothing held; and one whose pinned style keeps
# its names when narrow.

iris_model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)

examples <- c(
  latex_prettier = lm_latex(iris_model),
  latex_brackets = lm_latex(iris_model, style = "brackets"),
  latex_raw = lm_latex(iris_model, style = "raw"),
  equation_factor_call = lm_equation(lm(mpg ~ disp + factor(cyl), mtcars)),
  equation_as_factor_interaction = lm_equation(lm(mpg ~ wt * as.factor(am), mtcars))
)

covers("lm_latex.factor-styles", "lm_equation.response-and-term-names", "geom_slice_subtitle.style-pinned",
       "geom_slice_subtitle.held-line")
judged_output("factor_terms", examples, type = "returned_text",
              title = "Factor term names",
              purpose = "lm_latex() names factor terms in each style as lm_equation() does, and factors made in the formula keep their call in the term name.")

test_that("lm_latex() names factor terms in each style as lm_equation() does", {
  expect_match(examples[["latex_prettier"]], '\\text{(Species="versicolor")}', fixed = TRUE)
  expect_match(examples[["latex_prettier"]], '\\text{Sepal.Width:(Species="virginica")}', fixed = TRUE)
  expect_match(examples[["latex_brackets"]], "\\text{[versicolor]}", fixed = TRUE)
  expect_match(examples[["latex_raw"]], "\\text{Sepal.Width:Speciesvirginica}", fixed = TRUE)
  expect_equal(lm_latex(iris_model, style = "bracket"), examples[["latex_brackets"]])
})

test_that("a factor made in the formula keeps its call in the term name", {
  expect_match(examples[["equation_factor_call"]], '*(factor(cyl)="6")', fixed = TRUE)
  expect_match(examples[["equation_as_factor_interaction"]], 'wt:(as.factor(am)="1")', fixed = TRUE)
})

test_that("the response and terms are named as the formula writes them", {
  lhs <- function(f) sub(" = .*", "", lm_equation(lm(f, mtcars)))
  expect_equal(vapply(list(1 / mpg ~ wt, sqrt(mpg) ~ wt, exp(mpg / 10) ~ wt, mpg^2 ~ wt), lhs, ""),
               c("1/mpg", "sqrt(mpg)", "exp(mpg/10)", "mpg^2"))
  expect_match(lm_equation(lm(mpg ~ wt:hp, mtcars)), "\\*wt:hp$")
  expect_match(lm_equation(lm(mpg ~ I(wt * hp), mtcars)), "\\*I\\(wt \\* hp\\)$")
})

test_that("with nothing held the subtitle is the equation alone, with no empty held line", {
  m <- lm(mpg ~ wt, mtcars)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m) + geom_slice_subtitle(wrap = FALSE)
  expect_identical(p$labels$subtitle, lm_equation(m))
})

test_that("a pinned style keeps its names at a narrow width and says nothing about shortening", {
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
