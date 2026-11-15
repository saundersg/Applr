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

covers("shared.back-transform.message", "shared.back-transform.y-mismatch",
       "shared.back-transform.no-inverse", "shared.back-transform.identity-wrapper",
       "shared.back-transform.engines-differ")

raises("back_transformed", on_mpg(lm(log(mpg) ~ disp, mtcars)), "message",
       "were back-transformed to match the `mpg` axis",
       purpose = "A log(mpg) model on a raw mpg axis is back-transformed, and geom_slice() says so with the way to turn it off.")
raises("back_transform_off", on_mpg(lm(log(mpg) ~ disp, mtcars), back_transform = FALSE), character(),
       purpose = "With back_transform = FALSE there is nothing to announce, so the same log(mpg) plot raises no conditions.")
raises("y_axis_not_response", on_mpg(lm(mpg ~ disp, mtcars), quote(hp)), "warning",
       "does not match the model's response",
       purpose = "A y-axis mapped to a variable that is not the model's response warns that the line may not belong there.")
raises("no_inverse", on_mpg(lm(abs(mpg) ~ disp, mtcars)), "warning",
       "Could not infer the inverse of the model's response `abs(mpg)`",
       purpose = "A response transform with no known inverse, abs(mpg), warns and plots the predictions on the transformed scale.")
raises("identity_wrapper", on_mpg(lm(I(1 / mpg) ~ disp, mtcars)), "warning",
       "Could not infer the inverse",
       purpose = "A response wrapped in I(), I(1 / mpg), should invert like the unwrapped one but today warns that no inverse is known.")
# Only the x-axis choice is announced; the back-transformation is silent.
raises("slice_2d_back_transform_silent", base_slice(lm(log(mpg) ~ disp, mtcars)), "message",
       "X-axis not specified",
       purpose = "slice_2d() back-transforms log(mpg) without a word, unlike geom_slice(); only its x-axis choice is announced.")
raises("slice_2d_no_inverse", base_slice(lm(abs(mpg) ~ disp, mtcars), x_axis = "disp"), "message",
       "Error creating inverse", format = FALSE,
       purpose = "slice_2d() given a response with no inverse lets an unformatted 'Error creating inverse' message through.")
