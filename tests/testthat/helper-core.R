# THE CORE: what every line Applr draws must be.
#
# At each point along a line, y is the model's prediction for one row of
# data in which
#   1. the x-axis variable takes the value at that point (`at`),
#   2. every other predictor is held at one stated value (`held`), and
#   3. the prediction is put into the y-axis's units (`to_y`): exp() for a
#      log(y) model on a raw y-axis, identity when the axis is already on the
#      model's scale.
# A confidence or prediction ribbon follows the same rule for its lower and
# upper edges. A decreasing `to_y` (such as 1/y) swaps them, so ymin stays
# below ymax. scatter_3d's surface is the same rule with two axis variables
# in `at` and no ribbon.
#
# The prediction itself is stats::predict(), which Applr trusts and doesn't
# recompute. Applr's job is building the right row and mapping the result.
# So each numeric test chooses `at`, `held` and `to_y` itself, from the model
# and the plot's set-up, never from Applr's code, and compares what Applr
# drew with what this function returns.
#
# Not promised yet: an x-axis that combines several variables, such as
# aes(log(a) + b). Many rows land on one x, so the model has no single value
# between data points, and Applr joins the points it can compute
# (for_devs/known_issues.Rmd). Tests of those axes check the data points
# only. Promising more is a change to this rule, not a bug fix.
#
# Returns the expected y values, or with `interval` a data frame of y, ymin
# and ymax.
expected_line <- function(model, at, held = list(), to_y = identity, interval = NULL) {
  rows <- do.call(data.frame, c(as.list(at), held, stringsAsFactors = FALSE, check.names = FALSE))
  if (is.null(interval)) return(to_y(unname(stats::predict(model, rows))))
  p <- stats::predict(model, rows, interval = interval)
  lwr <- to_y(unname(p[, "lwr"]))
  upr <- to_y(unname(p[, "upr"]))
  data.frame(y = to_y(unname(p[, "fit"])), ymin = pmin(lwr, upr), ymax = pmax(lwr, upr))
}
