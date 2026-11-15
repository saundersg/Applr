# CASE: base_slice_2d
# slice_2d() on a two-predictor model, with hp held at a chosen value: a test
# checks the line is the model's prediction, and the plot is judged for how it
# looks. Also the base_graphics type's demo, used by the suite's own tests.

model <- lm(mpg ~ wt + hp, data = mtcars)
draw <- function() slice_2d(model, x_axis = "wt", hp = 150)

judged_output("plot", draw, type = "base_graphics",
              covers = c("slice_2d.plot-labels", "slice_2d.caption"),
              purpose = "Mainly the base_graphics type's demo for the suite's own tests. For you, the plainest slice_2d() plot (untransformed model, default style, one held value); look_base/slice_2d_default and slice_2d_styled cover the rest.")

expectation("line_is_prediction",
            covers = "slice_2d.line-is-prediction",
            purpose = "Catches slice_2d() drawing anything but the model's prediction, or ignoring a held value: the line must equal predict() at hp = 150 along an even wt grid.", {
  prep <- prepare_slice_data(model = model, x_axis = "wt", n = 100, back_transform = TRUE,
                             interval = "none", dots = list(hp = 150), model_name = "model",
                             fn = "slice_2d")
  grid <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
  expect_equal(prep$line$x, grid)
  expect_equal(prep$line$fit, unname(predict(model, data.frame(wt = grid, hp = 150))))
})
