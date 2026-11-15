# CASE: look_base
# Base graphics: slice_2d()'s plot, title, axis labels and caption, a styled
# line with its band, overlaid add_slice_2d() slices, and diagnose()'s
# three panels.

logged <- lm(log(mpg) ~ disp + hp, data = mtcars)
quietly <- function(code) function() suppressMessages(code())

judged_output("slice_2d_default", quietly(function() slice_2d(logged)), type = "base_graphics",
              covers = c("slice_2d.plot-labels", "slice_2d.caption"),
              purpose = "slice_2d() with every default on a log(mpg) model: the back-transformed curve and a caption of filled-in values, both absent from base_slice_2d/plot.")
judged_output("slice_2d_styled", quietly(function() {
  slice_2d(logged, x_axis = "hp", disp = 250, n = 150, col = "blue", lwd = 2, interval = "prediction")
}), type = "base_graphics",
              covers = c("slice_2d.ylim-covers-band", "slice_2d.graphical-params"),
              purpose = "The only base plot with a band and custom style: the band must be a translucent version of the line's colour, and the y-axis must make room for it.")
judged_output("add_slice_2d_overlay", quietly(function() {
  plot(mpg ~ disp, data = mtcars)
  add_slice_2d(logged, hp = min(mtcars$hp), col = "blue")
  add_slice_2d(logged, hp = max(mtcars$hp), col = "red", lty = 2, interval = "confidence")
}), type = "base_graphics",
              covers = c("add_slice_2d.adds-to-plot", "add_slice_2d.caption-off"),
              purpose = "add_slice_2d() on the user's own plot: two slices must overlay without redrawing it, each in its own style, and no caption may be painted over.")
judged_output("diagnose", function() diagnose(lm(dist ~ speed, data = cars)), type = "base_graphics",
              covers = "diagnose.three-panels",
              purpose = "The only picture of diagnose(): three residual panels in one row, with the largest residuals labelled and nothing overlapping.")

expectation("ylim_covers_band",
            covers = "slice_2d.ylim-covers-band",
            purpose = "Catches the y-axis being sized to the data only, which would cut the prediction band off at the top or bottom.", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  suppressMessages(slice_2d(logged, x_axis = "hp", disp = 250, interval = "prediction"))
  prep <- prepare_slice_data(logged, "hp", 100, TRUE, "prediction", list(disp = 250))
  usr <- graphics::par("usr")
  expect_lte(usr[3], min(prep$line$lwr))
  expect_gte(usr[4], max(prep$line$upr))
})
