# CASE: base_slice_2d
# slice_2d() on a two-predictor model, with hp held at a chosen value: the
# drawn line is the model's prediction, and the base graphics plot is judged.

model <- lm(mpg ~ wt + hp, data = mtcars)

covers("slice_2d.line-is-prediction")
judged_output("plot", function() slice_2d(model, x_axis = "wt", hp = 150), type = "base_graphics",
              title = "Held value slice",
              purpose = "slice_2d() on a two-predictor model with hp held at 150 draws the points, the prediction line, a formula title and a caption naming the held value.")

test_that("the prepared line equals the model's prediction with hp held at 150", {
  prep <- prepare_slice_data(model = model, x_axis = "wt", n = 100, back_transform = TRUE,
                             interval = "none", dots = list(hp = 150), model_name = "model",
                             fn = "slice_2d")
  grid <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
  expect_equal(prep$line$x, grid)
  expect_equal(prep$line$fit, expected_line(model, list(wt = grid), list(hp = 150)))
})
