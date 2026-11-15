# CASE: num_slice_text
# Where geom_slice_text() puts its labels, one per line, and what it does to
# the plot's scales to make room.

iris_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
base <- ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length))
three <- function(...) suppressMessages(
  base + geom_slice(iris_model, predict_vars = list(Sepal.Width = c(2, 3, 4))) + geom_slice_text(...))

expectation("label_at_end",
            covers = "geom_slice_text.label-per-line",
            purpose = "Catches a line getting no label, two labels, or a label away from its line's right-hand endpoint.", {
  p <- three()
  t <- slice_text_data(p)
  d <- slice_data(p)
  expect_equal(nrow(t), 3)
  for (w in c(2, 3, 4)) {
    label <- t[t$label == paste0("Sepal.Width: ", w), ]
    expect_equal(label$x, max(iris$Petal.Length))
    expect_equal(label$y, predicted(iris_model, data.frame(Petal.Length = label$x, Sepal.Width = w)))
    expect_equal(label$hjust, 0)
  }
})

expectation("left_location",
            covers = "geom_slice_text.location",
            purpose = "Catches location = \"left\" being ignored, or its labels left-justified so they run over the line.", {
  t <- slice_text_data(three(location = "left"))
  expect_equal(unique(t$x), min(iris$Petal.Length))
  expect_equal(unique(t$hjust), 1)
})

expectation("two_layers_combined",
            covers = "geom_slice_text.one-layer-per-slice",
            purpose = "Catches labels being built from only one of two slice layers of the same model, so some lines go unlabelled or wrongly labelled.", {
  p <- suppressMessages(base +
    geom_slice(iris_model, predict_vars = list(Sepal.Width = 2)) +
    geom_slice(iris_model, predict_vars = list(Sepal.Width = 4), colour = "red") +
    geom_slice_text())
  t <- slice_text_data(p)
  expect_equal(sort(t$label), c("Sepal.Width: 2", "Sepal.Width: 4"))
  expect_equal(t$colour[t$label == "Sepal.Width: 4"], "red")
})

expectation("keeps_user_scale",
            covers = "geom_slice_text.expand-keeps-scale",
            purpose = "Catches the room made for labels replacing the user's own x scale (limits, breaks, name) or printing ggplot2's 'Scale for x is already present'.", {
  p <- base + geom_slice(iris_model, predict_vars = list(Sepal.Width = c(2, 4))) +
    ggplot2::scale_x_continuous("Petal length", limits = c(0, 8), breaks = c(0, 4, 8),
                                expand = ggplot2::expansion(mult = c(0.2, 0.05)))
  msgs <- character()
  p <- withCallingHandlers(p + geom_slice_text(),
                           message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("already present", msgs)))
  sc <- p$scales$get_scales("x")
  expect_equal(sc$name, "Petal length")
  expect_equal(sc$limits, c(0, 8))
  expect_equal(sc$breaks, c(0, 4, 8))
  expect_equal(sc$expand[1], 0.2)
  expect_gt(sc$expand[3], 0.05)
  expect_equal(geom_slice_text(expand = FALSE)$expand, FALSE)
  untouched <- suppressMessages(base + geom_slice(iris_model) + geom_slice_text(expand = FALSE))
  expect_null(untouched$scales$get_scales("x"))
})

expectation("log_scale_kept",
            covers = "geom_slice_text.date-or-log-axis",
            purpose = "Catches widening for labels quietly turning a log x axis into a linear one.", {
  m <- lm(mpg ~ disp + hp, mtcars)
  p <- suppressMessages(ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) + ggplot2::scale_x_log10() +
                          geom_slice(m, predict_vars = list(hp = c(100, 200))) + geom_slice_text())
  expect_equal(p$scales$get_scales("x")$get_transformation()$name, "log-10")
})
