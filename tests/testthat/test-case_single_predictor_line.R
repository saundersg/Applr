# CASE: single_predictor_line
# A one-predictor model: the plotted line is the model's prediction, and the
# plot as a whole is judged.

set.seed(1)
df <- data.frame(x = runif(50, -10, 10))
df$y <- 2 * df$x + 3 + rnorm(50)
model <- lm(y ~ x, data = df)

p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
  ggplot2::geom_point() +
  geom_slice(model) +
  ggplot2::labs(title = "Single predictor")

judged_output("plot", p,
              covers = "geom_slice.user-theme",
              purpose = "The static_plot type's demo for the suite's own tests, and the simplest geom_slice() picture: one line in the user's plot, title and default theme, with nothing added.")

expectation("line_is_prediction",
            covers = "geom_slice.line-is-prediction",
            purpose = "Catches geom_slice()'s line not being predict() over an even grid across x's data range: the core promise of the package, in its simplest setting.", {
  d <- ggplot2::ggplot_build(p)$data[[2]]
  grid <- seq(min(df$x), max(df$x), length.out = 100)
  expect_equal(d$x, grid)
  expect_equal(d$y, unname(predict(model, data.frame(x = grid))))
})
