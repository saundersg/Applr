# CASE: scatter_3d_two_predictors
# scatter_3d() on a two-predictor model: the surface is the model's
# prediction over the predictor grid. The widget is judged from three
# camera presets plus live use.

model <- lm(mpg ~ wt + hp, data = mtcars)
widget <- scatter_3d(model, n = 20)

judged_output("widget", widget, states = list(
  default = list(eye = list(x = 1.25, y = 1.25, z = 1.25)),
  top = list(eye = list(x = 0, y = 0, z = 2.5)),
  side = list(eye = list(x = 2.5, y = 0, z = 0))
),
              covers = "scatter_3d.readable-from-any-angle",
              purpose = "The plotly_3d type's demo for the suite's own tests, and the plainest scatter_3d(): a flat surface through the points, from three cameras and live. widget_scatter_3d covers curved and twisted ones.")

expectation("surface_is_prediction",
            covers = "scatter_3d.surface-is-prediction",
            purpose = "Catches the 3D surface being anything but predict() over the predictor grid, including its rows and columns being transposed, which still looks plausible.", {
  built <- plotly::plotly_build(widget)$x$data
  surface <- Filter(function(t) identical(t$type, "surface"), built)[[1]]
  grid <- expand.grid(wt = surface$x, hp = surface$y)
  expected <- matrix(predict(model, grid), nrow = length(surface$x))
  expect_equal(matrix(surface$z, nrow = nrow(surface$z)), t(unname(expected)))
})
