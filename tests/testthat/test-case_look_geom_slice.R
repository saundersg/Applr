# CASE: look_geom_slice
# How geom_slice() lines and ribbons look: the default skyblue line, fixed
# aesthetics, ribbons in their own line's colour, a fill that overrides it,
# and the confidence and prediction ribbons side by side in width.

mpg <- ggplot2::mpg
iris_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
quad <- lm(hwy ~ displ + I(displ^2), data = mpg)
grouped <- lm(hwy ~ displ + I(displ^2) + displ:drv, data = mpg)
points <- function(data, mapping) ggplot2::ggplot(data, mapping) + ggplot2::geom_point()

default <- points(iris, ggplot2::aes(Petal.Length, Sepal.Length)) + geom_slice(iris_model) +
  ggplot2::labs(title = "Default line")
styled <- points(iris, ggplot2::aes(Petal.Length, Sepal.Length)) +
  geom_slice(iris_model, colour = "firebrick", linewidth = 2, linetype = "dashed") +
  ggplot2::labs(title = "colour, linewidth and linetype set")
confidence <- points(mpg, ggplot2::aes(displ, hwy)) + geom_slice(quad, interval = "confidence") +
  ggplot2::labs(title = "Confidence interval")
prediction <- points(mpg, ggplot2::aes(displ, hwy)) + geom_slice(quad, interval = "prediction") +
  ggplot2::labs(title = "Prediction interval")
ribbon_per_group <- points(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
  geom_slice(grouped, interval = "confidence") + ggplot2::labs(title = "Ribbons follow their line's colour")
fill_set <- points(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
  geom_slice(grouped, interval = "confidence", fill = "grey50") + ggplot2::labs(title = "fill = \"grey50\" wins")
full_range <- points(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(grouped, full_range = TRUE) +
  ggplot2::labs(title = "full_range = TRUE")

judged_output("default", default,
              covers = "geom_slice.default-look",
              purpose = "geom_slice() with no styling: the default skyblue line must sit in the user's plot and theme without changing anything else.")
judged_output("styled", styled,
              covers = "geom_slice.fixed-aesthetics",
              purpose = "Colour, linewidth and linetype set together; catches one of them not reaching the line, which default can't show.")
judged_output("confidence", confidence,
              covers = "shared.intervals.look",
              purpose = "A confidence ribbon by itself, so you can judge its translucency and shape (narrow where data are dense).")
judged_output("prediction", prediction,
              covers = "shared.intervals.look",
              purpose = "The same slice with a prediction ribbon; next to confidence, it must look clearly wider and cover most points.")
judged_output("ribbon_per_group", ribbon_per_group,
              covers = "geom_slice.ribbon-follows-line",
              purpose = "Ribbons on coloured groups must each take their own line's colour; a single-colour plot couldn't show this.")
judged_output("fill_set", fill_set,
              covers = "geom_slice.ribbon-follows-line",
              purpose = "Unlike ribbon_per_group, a fixed fill must win for every ribbon while the lines keep their colours.")
judged_output("full_range", full_range,
              covers = "geom_slice.full-range",
              purpose = "What full_range = TRUE looks like: every group's line across the whole panel, past its own data. A test checks the numbers; you judge whether it reads as extrapolation.")

expectation("ribbon_colour",
            covers = c("geom_slice.ribbon-follows-line", "geom_slice.default-look"),
            purpose = "Catches ribbons not taking their line's colour, a fixed fill not winning, or the default colour changing from skyblue.", {
  # The swap happens as the ribbon is drawn
  d <- slice_data(ribbon_per_group)
  expect_equal(slice_ribbon_fill(d, GeomSlice$default_aes$fill), d$colour)
  expect_equal(unique(slice_data(fill_set)$fill), "grey50")
  expect_equal(unique(slice_data(default)$colour), "skyblue")
})
