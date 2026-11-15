# CASE: num_band
# A projection band's edges are the predictions at the banded variable's two
# values, which come from predict_vars, the data range, or band = TRUE.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl, data = mpg)
coloured <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv))

edges <- function(line, drv, lo, hi) {
  a <- predicted(model, data.frame(displ = line$x, drv = drv, cyl = lo))
  b <- predicted(model, data.frame(displ = line$x, drv = drv, cyl = hi))
  list(ymin = pmin(a, b), ymax = pmax(a, b), y = (a + b) / 2)
}

expectation("named_band_edges",
            covers = c("shared.band.edges-are-predictions", "shared.band.range"),
            purpose = "Catches band edges that aren't the predictions at the two predict_vars values, a line that isn't their midpoint, or edges swapped when given high-then-low, in every colour group.", {
  d <- slice_data(coloured + geom_slice(model, n = 10, band = "cyl", predict_vars = list(cyl = c(7, 5))))
  lv <- levels(factor(mpg$drv))
  for (g in unique(d$group)) {
    line <- d[d$group == g, ]
    e <- edges(line, lv[g], 5, 7)
    expect_equal(line$ymin, e$ymin)
    expect_equal(line$ymax, e$ymax)
    expect_equal(line$y, e$y)
  }
})

expectation("data_range_and_true",
            covers = c("shared.band.range", "shared.band.infer-true"),
            purpose = "Catches a band without predict_vars not spanning the data range, or band = TRUE choosing a different variable than naming it would.", {
  named <- slice_data(coloured + geom_slice(model, n = 10, band = "cyl"))
  inferred <- slice_data(coloured + geom_slice(model, n = 10, band = TRUE))
  expect_equal(inferred, named)
  e <- edges(named[named$group == 1, ], "4", min(mpg$cyl), max(mpg$cyl))
  expect_equal(named$ymin[named$group == 1], e$ymin)
})

expectation("true_picks_multi_value",
            covers = "shared.band.infer-true",
            purpose = "Unlike data_range_and_true, the predict_vars entry with several values must win over the variable the plot doesn't show.", {
  m <- lm(hwy ~ displ + cyl + year, data = mpg)
  d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
                    geom_slice(m, n = 10, band = TRUE, predict_vars = list(year = c(1999, 2008), cyl = 6)))
  a <- predicted(m, data.frame(displ = d$x, cyl = 6, year = 1999))
  b <- predicted(m, data.frame(displ = d$x, cyl = 6, year = 2008))
  expect_equal(d$ymin, pmin(a, b))
  expect_equal(d$ymax, pmax(a, b))
})

expectation("text_labels_edges",
            covers = "geom_slice_text.band-edges",
            purpose = "Catches geom_slice_text() putting a band edge's label on the wrong edge: each label's position must be the prediction for the value it names.", {
  p <- suppressMessages(coloured + geom_slice(model, band = "cyl", predict_vars = list(cyl = c(5, 7))) +
                          geom_slice_text())
  t <- slice_text_data(p)
  expect_equal(sort(unique(t$label)), c("cyl: 5", "cyl: 7"))
  for (i in seq_len(nrow(t))) {
    cyl <- as.numeric(sub("cyl: ", "", t$label[i]))
    drv <- levels(factor(mpg$drv))[t$colour[i] == unique(t$colour)]
    expect_equal(t$y[i], predicted(model, data.frame(displ = t$x[i], drv = drv, cyl = cyl)))
  }
})
