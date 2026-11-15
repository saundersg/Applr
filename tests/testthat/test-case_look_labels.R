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

judged_output("colour_by_group", colour_by_group,
              covers = c("geom_slice_text.colour-follows-line", "geom_slice_text.expand-fits-labels"),
              purpose = "Labels on coloured groups: each must match its line's colour, and the panel must widen just enough that no label is clipped.")
judged_output("legend_style", legend_style,
              covers = c("geom_slice_text.legend-key-colour", "geom_slice_text.legend-headroom"),
              purpose = "The legend style's corner key, the only output showing it with six lines: the key must name the variables and clear the topmost label.")
judged_output("legend_one_colour", legend_one_colour,
              covers = "geom_slice_text.legend-key-colour",
              purpose = "Unlike legend_style, the lines share one fixed colour, so the key must take that colour instead of grey.")
judged_output("left_offset", left_offset,
              covers = c("geom_slice_text.offset-points", "geom_slice_text.expand-fits-labels"),
              purpose = "The only output with labels on the left: the gap must look like 15pt and the panel must widen on the left, not the right.")
judged_output("manual_hjust", manual_hjust,
              covers = "geom_slice_text.offset-points",
              purpose = "A user's hjust and vjust must take over from the automatic offset; catches Applr's offset fighting the user's placement.")
judged_output("no_expand", no_expand,
              covers = "geom_slice_text.expand-fits-labels",
              purpose = "Shows the documented trade-off of expand = FALSE, labels running off the panel, so you can judge it looks deliberate rather than broken.")
judged_output("subtitle", subtitle,
              covers = c("geom_slice_subtitle.readable", "shared.annotation-wrap.hanging-indent"),
              purpose = "A long subtitle at the usual width: it must read as an equation, with continuation lines hanging under the right-hand side and clear of the panel.")
judged_output("caption", caption,
              covers = "geom_slice_caption.no-indent-right-aligned",
              purpose = "The same text as subtitle, as a caption: right-aligned with no hanging indent, which would look odd on the right.")
judged_output("subtitle_narrow", subtitle_narrow,
              covers = "shared.annotation-wrap.narrow-and-wide",
              purpose = "The narrow half of narrow-and-wide (subtitle is the wide half): squeezed to 3.5 inches, the subtitle must still fit inside the plot.")

expectation("label_and_key_colours",
            covers = c("geom_slice_text.colour-follows-line", "geom_slice_text.legend-key-colour"),
            purpose = "Catches labels not matching their lines' colours, or a one-colour plot's legend key turning grey. Note its old name said 'grey for mixed colours', which it doesn't check.", {
  t <- slice_text_data(colour_by_group)
  expect_equal(sort(unique(t$colour)), sort(unique(slice_data(colour_by_group)$colour)))
  key <- Filter(function(l) inherits(l$geom, "GeomText") && !inherits(l$geom, "GeomSliceText"),
                legend_one_colour$layers)[[1]]
  expect_equal(unname(key$aes_params$colour), "darkgreen")
})
