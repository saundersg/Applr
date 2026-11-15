# CASE: num_scatter_3d
# What scatter_3d()'s built traces hold: the data as markers, a surface over
# the raw predictors, the colour ramp and the axis titles.

# plotly tags built vectors with an apiSrc attribute; the values are what count.
strip_api <- function(x) {
  if (is.list(x)) return(lapply(x, strip_api))
  attr(x, "apiSrc") <- NULL
  x
}
built_traces <- function(widget) strip_api(plotly::plotly_build(widget)$x)
markers_of <- function(x) Filter(function(t) identical(t$type, "scatter3d"), x$data)[[1]]
surface_of <- function(x) Filter(function(t) identical(t$type, "surface"), x$data)[[1]]

expectation("markers_are_data",
            covers = "scatter_3d.points-are-data",
            purpose = "Catches scatter_3d() plotting anything but the model's own rows at (first predictor, second predictor, response).", {
  x <- built_traces(scatter_3d(lm(mpg ~ wt + hp, mtcars), n = 5))
  m <- markers_of(x)
  expect_equal(m$x, mtcars$wt)
  expect_equal(m$y, mtcars$hp)
  expect_equal(m$z, mtcars$mpg)
})

expectation("raw_predictors",
            covers = "scatter_3d.raw-predictors",
            purpose = "Catches a model with I(x^2) getting a third axis or an axis named I(x^2) instead of graphing over x and y.", {
  model <- lm(mpg ~ wt + I(wt^2) + hp, mtcars)
  x <- built_traces(scatter_3d(model, n = 6))
  s <- surface_of(x)
  expect_equal(s$x, seq(min(mtcars$wt), max(mtcars$wt), length.out = 6))
  expect_equal(s$y, seq(min(mtcars$hp), max(mtcars$hp), length.out = 6))
  expected <- outer(s$y, s$x, function(hp, wt) predicted(model, data.frame(wt = wt, hp = hp)))
  expect_equal(unname(s$z), expected)
})

expectation("marker_colours",
            covers = "scatter_3d.marker-colours",
            purpose = "Catches markers ignoring `colors`, a single colour giving an invalid ramp, or the colour bar losing the response's name.", {
  x <- built_traces(scatter_3d(lm(mpg ~ wt + hp, mtcars), n = 5, colors = c("purple", "orange")))
  m <- markers_of(x)
  expect_equal(m$marker$color, mtcars$mpg)
  stops <- m$marker$colorscale
  expect_equal(vapply(stops, function(s) as.numeric(s[[1]]), 0), c(0, 1))
  expect_equal(vapply(stops, function(s) s[[2]], ""), c("#A020F0", "#FFA500"))
  expect_equal(m$marker$colorbar$title, "mpg")
  flat <- markers_of(built_traces(scatter_3d(lm(mpg ~ wt + hp, mtcars), n = 5, colors = "red")))
  expect_equal(vapply(flat$marker$colorscale, function(s) s[[2]], ""), c("#FF0000", "#FF0000"))
})

expectation("axis_titles",
            covers = "scatter_3d.axis-titles",
            purpose = "Catches the 3D scene's axes being left as plotly's defaults (x, y, z) instead of the variable names.", {
  scene <- built_traces(scatter_3d(lm(mpg ~ wt + hp, mtcars), n = 5))$layout$scene
  expect_equal(c(scene$xaxis$title, scene$yaxis$title, scene$zaxis$title), c("wt", "hp", "mpg"))
})

expectation("transformed_response_known_issue",
            covers = "scatter_3d.transformed-response",
            purpose = "Pins a known mismatch (raw points, log-scale surface) so whoever settles the open question sees this test change.", {
  # The surface is predict() on the log scale while the markers are raw mpg,
  # so they don't line up (for_devs/known_issues.Rmd). This pins today's
  # behaviour; when it fails, decide which scale both should use.
  model <- lm(log(mpg) ~ wt + hp, mtcars)
  x <- built_traces(scatter_3d(model, n = 5))
  expect_equal(markers_of(x)$z, mtcars$mpg)
  expect_lt(max(surface_of(x)$z), min(mtcars$mpg))
})

expectation("autoplot_3d",
            covers = "autoplot.type-3d",
            purpose = "Catches autoplot(type = \"3d\") building something other than scatter_3d(), or dropping the arguments passed to it.", {
  model <- lm(mpg ~ wt + hp, mtcars)
  a <- built_traces(autoplot(model, type = "3d", summary = FALSE, n = 5, colors = "red"))
  b <- built_traces(scatter_3d(model, n = 5, colors = "red"))
  expect_equal(surface_of(a)$z, surface_of(b)$z)
  expect_equal(markers_of(a)$marker$colorscale, markers_of(b)$marker$colorscale)
})
