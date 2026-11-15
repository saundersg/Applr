# CASE: widget_scatter_3d
# scatter_3d() widgets judged from the default, top and side cameras plus
# live use: a curved surface over raw predictors with a custom gradient, a
# twisted interaction surface, and a transformed response, whose surface and
# points are known not to line up.

cameras <- list(
  default = list(eye = list(x = 1.25, y = 1.25, z = 1.25)),
  top = list(eye = list(x = 0, y = 0, z = 2.5)),
  side = list(eye = list(x = 2.5, y = 0, z = 0))
)
curved <- scatter_3d(lm(mpg ~ wt + I(wt^2) + hp, mtcars), n = 25, colors = c("purple", "orange"))
twisted <- scatter_3d(lm(Sepal.Length ~ Sepal.Width * Petal.Length, iris), n = 25)
log_response <- scatter_3d(lm(log(mpg) ~ wt + hp, mtcars), n = 25)

judged_output("curved", curved, states = cameras,
              covers = c("scatter_3d.surface-style", "scatter_3d.readable-from-any-angle"),
              purpose = "A curved surface with a custom gradient: from every camera the points must stay visible through it and the colours must match the gradient asked for.")
judged_output("twisted", twisted, states = cameras,
              covers = c("scatter_3d.twisted-surface", "scatter_3d.readable-from-any-angle"),
              purpose = "An interaction surface looks flat from the default camera; this checks the side camera reveals the twist, which is the point of showing it in 3D.")
judged_output("log_response", log_response, states = cameras,
              covers = "scatter_3d.transformed-response",
              purpose = "Shows the known mismatch (surface on the log scale, points raw) so you can see how bad it looks while the open question is settled.")

expectation("surface_translucent",
            covers = "scatter_3d.surface-style",
            purpose = "Catches the surface becoming opaque or gaining its own colour bar, which would hide points behind it and duplicate the legend.", {
  for (w in list(curved, twisted)) {
    s <- Filter(function(t) identical(t$type, "surface"), plotly::plotly_build(w)$x$data)[[1]]
    expect_equal(s$opacity, 0.6)
    expect_false(s$showscale)
  }
})

expectation("twisted_not_plane",
            covers = "scatter_3d.twisted-surface",
            purpose = "Catches the interaction model's surface coming out flat, which would make the twisted picture meaningless.", {
  s <- Filter(function(t) identical(t$type, "surface"), plotly::plotly_build(twisted)$x$data)[[1]]
  z <- unname(s$z)
  corners <- z[1, 1] + z[25, 25] - z[1, 25] - z[25, 1]
  expect_gt(abs(corners), 0.1)
})
