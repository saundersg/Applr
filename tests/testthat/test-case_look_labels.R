# CASE: look_labels
# How geom_slice_text() labels and the subtitle and caption sit on the plot:
# colours, the legend key and its headroom, placement and offset, the room
# made for labels, and a wrapped subtitle and caption.

set.seed(123)
d <- data.frame(x = runif(80, -10, 10), x2 = runif(80, 0, 4),
                g = factor(sample(c("A", "B"), 80, replace = TRUE)))
d$y <- d$x * ifelse(d$g == "A", 0.5, 2) + ifelse(d$g == "A", 5, -5) + 4 * d$x2 + rnorm(80)
model <- lm(y ~ x * g + x2, data = d)
iris_model <- lm(Petal.Length ~ Sepal.Length + I(Sepal.Length^2):Species + Sepal.Width, iris)
build <- function(p) suppressMessages(p)
on_d <- function(mapping = ggplot2::aes(x, y)) ggplot2::ggplot(d, mapping) + ggplot2::geom_point(alpha = 0.5)

colour_by_group <- build(on_d(ggplot2::aes(x, y, colour = g)) +
  geom_slice(model, predict_vars = list(x2 = c(0, 2, 4))) + geom_slice_text())
legend_style <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 2, 4), g = c("A", "B"))) +
  geom_slice_text(style = "legend"))
legend_one_colour <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4)), colour = "darkgreen") +
  geom_slice_text(style = "legend"))
left_offset <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) +
  geom_slice_text(location = "left", offset = 15, color = "black"))
manual_hjust <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) +
  geom_slice_text(hjust = 0.5, vjust = -0.5))
no_expand <- build(on_d() + geom_slice(model, predict_vars = list(x2 = c(0, 4))) + geom_slice_text(expand = FALSE))
subtitle <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
  ggplot2::geom_point() + geom_slice(iris_model, interval = "confidence") + geom_slice_subtitle(wrap = 6))
caption <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
  ggplot2::geom_point() + geom_slice(iris_model, interval = "confidence") + geom_slice_caption(wrap = 6))
subtitle_narrow <- build(ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
  ggplot2::geom_point() + geom_slice(iris_model) + geom_slice_subtitle(wrap = 3.5) +
  ggplot2::theme(plot.margin = ggplot2::margin(5, 250, 5, 5)))

covers("geom_slice_text.colour-follows-line", "geom_slice_text.legend-key-colour",
       "geom_slice_text.legend-headroom", "geom_slice_text.offset-points",
       "geom_slice_text.expand-fits-labels", "geom_slice_subtitle.readable",
       "geom_slice_caption.no-indent-right-aligned", "shared.annotation-wrap.hanging-indent",
       "shared.annotation-wrap.narrow-and-wide")
judged_output("colour_by_group", colour_by_group,
              purpose = "Labels on lines coloured by group take their own line's colour and fit inside the widened panel.")
judged_output("legend_style", legend_style,
              purpose = "The legend style labels six same-coloured lines with bare values and a key naming the variables, with headroom so the key clears the top label.")
judged_output("legend_one_colour", legend_one_colour,
              purpose = "With a fixed line colour the legend key takes that colour rather than grey.")
judged_output("left_offset", left_offset,
              purpose = "Labels at the left ends, offset 15pt and in a fixed colour, widen the panel on the left.")
judged_output("manual_hjust", manual_hjust,
              purpose = "A user's hjust and vjust replace the automatic offset, centring labels on the line ends.")
judged_output("no_expand", no_expand,
              purpose = "expand = FALSE leaves the scales alone, so labels run off the panel: the documented trade-off.")
judged_output("subtitle", subtitle,
              purpose = "A long subtitle wrapped at 6 inches hangs its continuation lines under the equation's right-hand side, clear of the panel.")
judged_output("caption", caption,
              purpose = "The same text as a caption sits below the plot, right-aligned and wrapped without the hanging indent.")
judged_output("subtitle_narrow", subtitle_narrow,
              purpose = "A subtitle wrapped at 3.5 inches beside a squeezed panel still fits, term by term.")

test_that("labels take their line's colour, and the key is grey for mixed colours", {
  t <- slice_text_data(colour_by_group)
  expect_equal(sort(unique(t$colour)), sort(unique(slice_data(colour_by_group)$colour)))
  key <- Filter(function(l) inherits(l$geom, "GeomText") && !inherits(l$geom, "GeomSliceText"),
                legend_one_colour$layers)[[1]]
  expect_equal(unname(key$aes_params$colour), "darkgreen")
})
