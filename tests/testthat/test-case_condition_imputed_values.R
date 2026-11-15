# CASE: condition_imputed_values
# geom_slice() on a three-predictor model with nothing held: each unheld
# predictor is imputed with a message, and the plot is still built. The
# messages' wording is judged.

model <- lm(mpg ~ wt + hp + qsec, data = mtcars)
call <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    geom_slice(model)
}

judged_output("wording", call, type = "conditions",
              covers = c("shared.imputation.message", "shared.conditions.two-part-format", "shared.conditions.severity"),
              purpose = "The numeric imputation message on its own, with nothing else raised; cond_imputation covers factors and characters. Also the conditions type's demo, used by the suite's own tests.")

expectation("imputed_and_built",
            covers = "shared.imputation.message",
            purpose = "Catches an unheld predictor going unannounced, or the plot failing to build after the messages.", {
  expect_message(expect_message(p <- ggplot2::ggplot_build(call()), "`hp`"), "`qsec`")
  expect_s3_class(p, "ggplot_built")
})
