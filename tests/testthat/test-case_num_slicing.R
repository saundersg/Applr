# CASE: num_slicing
# Grouping aesthetics and facets pin their predictor to each group's or
# panel's own value; a facet the model doesn't use pins nothing.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl, data = mpg)
cyl_mean <- mean(mpg$cyl)

expect_line <- function(line, ...) {
  expect_equal(line$y, predicted(model, data.frame(displ = line$x, ...)))
}

expectation("colour_group_pins",
            covers = "shared.slicing.group-pins",
            purpose = "Catches each colour group's line being predicted at an imputed value instead of that group's own value, the central idea of slicing by aesthetic.", {
  d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(model, n = 10))
  lv <- levels(factor(mpg$drv))
  for (g in unique(d$group)) expect_line(d[d$group == g, ], drv = lv[g], cyl = cyl_mean)
  d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, linetype = drv)) + geom_slice(model, n = 10))
  for (g in unique(d$group)) expect_line(d[d$group == g, ], drv = lv[g], cyl = cyl_mean)
})

expectation("facet_pins",
            covers = "shared.slicing.facet-pins",
            purpose = "Catches facets failing to pin their predictor, for facet_wrap and facet_grid, which store their variables differently.", {
  lv <- levels(factor(mpg$drv))
  for (facet in list(ggplot2::facet_wrap(~drv), ggplot2::facet_grid(rows = ggplot2::vars(drv)))) {
    d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + geom_slice(model, n = 10) + facet)
    for (panel in unique(d$PANEL)) {
      expect_line(d[d$PANEL == panel, ], drv = lv[as.integer(panel)], cyl = cyl_mean)
    }
  }
})

expectation("factor_wrapped_colour",
            covers = "shared.slicing.factor-wrapped-aes",
            purpose = "Catches colour = factor(cyl) going unrecognised as cyl, so each group's line uses the mean cyl instead of its own number.", {
  d <- slice_data(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = factor(cyl))) +
                    geom_slice(model, n = 10, predict_vars = list(drv = "f")))
  cyls <- sort(unique(mpg$cyl))
  for (g in unique(d$group)) expect_line(d[d$group == g, ], drv = "f", cyl = cyls[g])
})

expectation("facet_not_predictor",
            covers = "shared.slicing.facet-not-predictor",
            purpose = "Catches a facet on a variable outside the model pinning something by mistake, which would draw different lines in panels that should match.", {
  m <- lm(mpg ~ wt, mtcars)
  d <- slice_data(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(m, n = 10) +
                    ggplot2::facet_wrap(~gear))
  for (panel in unique(d$PANEL)) {
    line <- d[d$PANEL == panel, ]
    expect_equal(line$y, predicted(m, data.frame(wt = line$x)))
  }
})

expectation("facet_after_autoplot",
            covers = "autoplot.user-facet-after",
            purpose = "Catches the README's autoplot(model) + facet_wrap(~drv) drawing the same line in every panel because the facet came after autoplot().", {
  m <- lm(hwy ~ displ + drv + cyl, data = mpg)
  p <- suppressMessages(autoplot(m, ggplot2::aes(colour = factor(cyl)), summary = FALSE)) +
    ggplot2::facet_wrap(~drv)
  d <- slice_data(p)
  lv <- levels(factor(mpg$drv))
  cyls <- sort(unique(mpg$cyl))
  for (k in seq_len(nrow(unique(d[c("PANEL", "group")])))) {
    key <- unique(d[c("PANEL", "group")])[k, ]
    line <- d[d$PANEL == key$PANEL & d$group == key$group, ]
    expect_equal(line$y, predicted(m, data.frame(displ = line$x, drv = lv[as.integer(key$PANEL)],
                                                 cyl = cyls[key$group])))
  }
})
