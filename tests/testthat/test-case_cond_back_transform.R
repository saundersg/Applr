# CASE: cond_back_transform
# What each engine says about a transformed response: geom_slice() announces
# the back-transformation and warns when it can't do one; slice_2d() is
# silent, except for an unformatted message when no inverse exists.

on_mpg <- function(model, y = quote(mpg), ...) function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(disp, !!y)) + geom_slice(model, ...)
}
base_slice <- function(...) function() {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  slice_2d(..., caption = FALSE)
}

raises("back_transformed", on_mpg(lm(log(mpg) ~ disp, mtcars)), "message",
       "were back-transformed to match the `mpg` axis",
       covers = c("shared.back-transform.message", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The most common case, log(y) on a raw axis: the message must say the line was changed and how to turn that off, since a student may not expect it.")
raises("back_transform_off", on_mpg(lm(log(mpg) ~ disp, mtcars), back_transform = FALSE), character(),
       covers = c("shared.back-transform.message", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Catches a message appearing even after the user turned back-transforming off with back_transform = FALSE.")
raises("y_axis_not_response", on_mpg(lm(mpg ~ disp, mtcars), quote(hp)), "warning",
       "does not match the model's response",
       covers = c("shared.back-transform.y-mismatch", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A y-axis that isn't the response usually means the line is meaningless there; the warning must say so while still drawing.")
raises("no_inverse", on_mpg(lm(abs(mpg) ~ disp, mtcars)), "warning",
       "Could not infer the inverse of the model's response `abs(mpg)`",
       covers = c("shared.back-transform.no-inverse", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A transform with no known inverse, abs(y): the warning must say the line stays on the transformed scale and point to back_transform =.")
raises("identity_wrapper", on_mpg(lm(I(1 / mpg) ~ disp, mtcars)), "warning",
       "Could not infer the inverse",
       covers = c("shared.back-transform.identity-wrapper", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Pins a gap: I(1 / mpg) should invert like 1 / mpg but warns like no_inverse. When it's fixed this output changes; its hint (exp) is also wrong for 1/y.")
# Only the x-axis choice is announced; the back-transformation is silent.
raises("slice_2d_back_transform_silent", base_slice(lm(log(mpg) ~ disp, mtcars)), "message",
       "X-axis not specified",
       covers = c("shared.back-transform.engines-differ", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Shows that slice_2d() back-transforms with no message, unlike back_transformed in geom_slice(); you can judge whether that difference is wanted.")
raises("slice_2d_no_inverse", base_slice(lm(abs(mpg) ~ disp, mtcars), x_axis = "disp"), "message",
       "Error creating inverse", format = FALSE,
       covers = c("shared.back-transform.engines-differ", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "slice_2d()'s counterpart to no_inverse lets an internal, unformatted message through; you can judge whether it should match geom_slice()'s warning.")
