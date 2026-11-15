# CASE: num_held_values
# Predictors the user names are held at the values given; several values
# give one line per value, and several multi-value variables are crossed.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ + cyl + drv, data = mpg)
base <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy))

expectation("held_single",
            covers = "shared.held-values.single",
            purpose = "Catches a predictor named in predict_vars being imputed or varied instead of held at the value given.", {
  d <- slice_data(base + geom_slice(model, n = 10, predict_vars = list(cyl = 6, drv = "r")))
  expect_equal(d$y, predicted(model, data.frame(displ = d$x, cyl = 6, drv = "r")))
})

expectation("crossed",
            covers = "shared.held-values.multi-crossed",
            purpose = "Catches several multi-value entries being paired up instead of crossed (3 x 2 must give 6 lines), or two lines sharing a group and being joined.", {
  d <- slice_data(base + geom_slice(model, n = 10,
                                    predict_vars = list(cyl = c(4, 6, 8), drv = c("f", "r"))))
  expect_equal(length(unique(d$group)), 6)
  combos <- expand.grid(cyl = c(4, 6, 8), drv = c("f", "r"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(combos))) {
    line <- d[d$group == sort(unique(d$group))[i], ]
    expect_equal(line$y, predicted(model, data.frame(displ = line$x, cyl = combos$cyl[i], drv = combos$drv[i])))
  }
})

expectation("coerced",
            covers = "shared.held-values.coerced",
            purpose = "Catches a value of the wrong type (a string for a factor, \"3\" for a number) being passed to predict() as is, where it fails or predicts at the wrong level.", {
  as_given <- slice_data(base + geom_slice(model, n = 10, predict_vars = list(cyl = "6", drv = "r")))
  typed <- slice_data(base + geom_slice(model, n = 10,
                                        predict_vars = list(cyl = 6, drv = factor("r"))))
  expect_equal(as_given$y, typed$y)
})

expectation("beats_pinning",
            covers = "shared.held-values.beats-pinning",
            purpose = "Catches a colour or facet's pinned value overriding what the user explicitly asked to hold.", {
  coloured <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
    geom_slice(model, n = 10, predict_vars = list(drv = "4", cyl = 4))
  d <- slice_data(coloured)
  expect_equal(d$y[d$group == 1], predicted(model, data.frame(displ = d$x[d$group == 1], cyl = 4, drv = "4")))
  faceted <- base + geom_slice(model, n = 10, predict_vars = list(drv = "4", cyl = 4)) +
    ggplot2::facet_wrap(~drv)
  d <- slice_data(faceted)
  for (panel in unique(d$PANEL)) {
    line <- d[d$PANEL == panel, ]
    expect_equal(line$y, predicted(model, data.frame(displ = line$x, cyl = 4, drv = "4")))
  }
})

expectation("slice_2d_factor_string",
            covers = "shared.held-values.base-not-coerced",
            purpose = "The slice_2d() path, which passes ... to predict() without coercing: a factor level given as a string must still predict at that level.", {
  prep <- suppressMessages(prepare_slice_data(model, "displ", 10, TRUE, "none",
                                              list(cyl = 6, drv = "r")))
  expect_equal(prep$line$fit, predicted(model, data.frame(displ = prep$line$x, cyl = 6, drv = "r")))
})

expectation("held_named_x",
            covers = "slice_2d.held-named-x",
            purpose = "Catches R's partial argument matching taking a predictor literally named x as slice_2d()'s x_axis.", {
  d <- data.frame(w = 1:10, x = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9))
  d$y <- d$w + 2 * d$x
  m <- lm(y ~ w + x, data = d)
  prep <- prepare_slice_data(m, "w", 10, TRUE, "none", list(x = 2))
  expect_equal(prep$line$fit, predicted(m, data.frame(w = prep$line$x, x = 2)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(slice_2d(m, x_axis = "w", x = 2))
})
