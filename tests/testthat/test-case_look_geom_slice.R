# CASE: look_geom_slice
# How geom_slice() lines and ribbons look: the default skyblue line, fixed
# aesthetics, ribbons in their own line's colour, a fill that overrides it,
# and the confidence and prediction ribbons side by side in width.

iris_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
quad <- lm(hwy ~ displ + I(displ^2), data = mpg)
grouped <- lm(hwy ~ displ + I(displ^2) + displ:drv, data = mpg)

default <- ggplot(iris, aes(Petal.Length, Sepal.Length)) + geom_point() + geom_slice(iris_model) +
  labs(title = "Default line")
styled <- ggplot(iris, aes(Petal.Length, Sepal.Length)) + geom_point() +
  geom_slice(iris_model, colour = "firebrick", linewidth = 2, linetype = "dashed") +
  labs(title = "colour, linewidth and linetype set")
confidence <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_slice(quad, interval = "confidence") +
  labs(title = "Confidence interval")
prediction <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_slice(quad, interval = "prediction") +
  labs(title = "Prediction interval")
ribbon_per_group <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() +
  geom_slice(grouped, interval = "confidence") + labs(title = "Ribbons follow their line's colour")
fill_set <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() +
  geom_slice(grouped, interval = "confidence", fill = "grey50") + labs(title = "fill = \"grey50\" wins")
full_range <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() + geom_slice(grouped, full_range = TRUE) +
  labs(title = "full_range = TRUE")

covers("geom_slice.default-look", "geom_slice.fixed-aesthetics", "geom_slice.ribbon-follows-line",
       "shared.intervals.look")
judged_output("default", default,
              title = "Default skyblue line",
              purpose = "geom_slice() with no styling draws one skyblue line in the user's plot and theme, adding nothing else.")
judged_output("styled", styled,
              title = "Fixed line styling",
              purpose = "Fixed colour, linewidth and linetype all reach the line.")
judged_output("confidence", confidence,
              title = "Confidence ribbon",
              purpose = "A confidence ribbon on a quadratic slice, narrow where the data are dense and widening where they thin out.")
judged_output("prediction", prediction,
              title = "Prediction ribbon",
              purpose = "A prediction ribbon on the same slice, far wider than the confidence one and covering most points.")
judged_output("ribbon_per_group", ribbon_per_group,
              title = "Ribbons per group",
              purpose = "Ribbons on lines coloured by group take their own line's colour, each over its group's range.")
judged_output("fill_set", fill_set,
              title = "Fixed ribbon fill",
              purpose = "A fixed fill overrides the line colours for every ribbon while the lines keep theirs.")
judged_output("full_range", full_range,
              title = "Full-range lines",
              purpose = "full_range = TRUE extends every group's line over the whole panel rather than its own data range.")

test_that("ribbons take the line's colour unless fill is set", {
  # The swap happens as the ribbon is drawn
  d <- slice_data(ribbon_per_group)
  expect_equal(slice_ribbon_fill(d, GeomSlice$default_aes$fill), d$colour)
  expect_equal(unique(slice_data(fill_set)$fill), "grey50")
  expect_equal(unique(slice_data(default)$colour), "skyblue")
})
