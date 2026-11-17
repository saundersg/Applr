# CASE: condition_imputed_values
# geom_slice() on a three-predictor model with nothing held: each unheld
# predictor is imputed with a message, and the plot is still built. The
# messages' wording is judged.

model <- lm(mpg ~ wt + hp + qsec, data = mtcars)
call <- function() ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_slice(model)

covers("shared.imputation.message")
judged_output("wording", call, type = "conditions",
              title = "Imputed values message",
              purpose = "A three-predictor model with nothing held imputes each unheld predictor with one message, and the plot still builds.")

test_that("each unheld predictor is imputed with a message and the plot still builds", {
  expect_message(expect_message(p <- ggplot2::ggplot_build(call()), "`hp`"), "`qsec`")
  expect_s3_class(p, "ggplot_built")
})
