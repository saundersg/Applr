# CASE: num_companions
# What geom_slice_subtitle() leaves out, when it adds nothing, that the
# caption holds the same text, and that adding companions touches no device.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)
coloured <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv))
quiet <- function(p) suppressMessages(p)

expectation("skips_shown",
            covers = c("geom_slice_subtitle.skips-shown", "geom_slice_subtitle.held-line"),
            purpose = "Catches the subtitle's held line repeating what the plot already shows (the x variable, a legend, a facet, a text label), which would tell the reader something false or redundant.", {
  held <- function(p) strsplit(annotation_of(p), "\n", fixed = TRUE)[[1]][2]
  s <- held(quiet(coloured + geom_slice(model) + geom_slice_subtitle(wrap = FALSE)))
  expect_match(s, "^held at: cyl = 5.889; year = 2004$")
  s <- held(quiet(coloured + ggplot2::facet_wrap(~cyl) + geom_slice(model) + geom_slice_subtitle(wrap = FALSE)))
  expect_match(s, "^held at: year = 2004$")
  s <- held(quiet(coloured + geom_slice(model, predict_vars = list(year = c(1999, 2008))) +
                    geom_slice_text() + geom_slice_subtitle(wrap = FALSE)))
  expect_match(s, "^held at: cyl = 5.889$")
  s <- held(quiet(coloured + geom_slice(model, predict_vars = list(cyl = 6)) + geom_slice_subtitle(wrap = FALSE)))
  expect_match(s, "^held at: cyl = 6; year = 2004$")
})

expectation("nothing_to_say",
            covers = "geom_slice_subtitle.nothing-to-say",
            purpose = "Catches an empty or placeholder subtitle being set when there is no equation and nothing held.", {
  p <- quiet(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + geom_slice(lm(mpg ~ wt, mtcars)))
  q <- quiet(p + geom_slice_subtitle(model = FALSE))
  expect_null(annotation_of(q))
  expect_identical(q$labels, p$labels)
})

expectation("caption_same_text",
            covers = "geom_slice_caption.same-text-in-caption",
            purpose = "Catches geom_slice_caption() drifting from geom_slice_subtitle(): it must hold exactly the same text, only without the indent.", {
  p <- quiet(coloured + geom_slice(model))
  expect_identical(annotation_of(quiet(p + geom_slice_caption(wrap = FALSE)), "caption"),
                   annotation_of(quiet(p + geom_slice_subtitle(wrap = FALSE))))
  lines_of <- function(q, what) strsplit(annotation_of(q, what), "\n", fixed = TRUE)[[1]]
  caption <- lines_of(quiet(p + geom_slice_caption(wrap = 5)), "caption")
  subtitle <- lines_of(quiet(p + geom_slice_subtitle(wrap = 5)), "subtitle")
  starts_blank <- function(x) substr(x, 1, 1) %in% c(" ", "\u2002", "\u2009", "\u200a")
  expect_gt(length(caption), 2)
  expect_false(any(starts_blank(caption)))
  expect_true(any(starts_blank(subtitle[-1])))
})

expectation("no_device",
            covers = "shared.companions.no-device-side-effect",
            purpose = "Catches companions opening a graphics device or drawing a page while they measure text, which would leave blank windows or extra pages in a user's output.", {
  grDevices::graphics.off()
  quiet(coloured + geom_slice(model) + geom_slice_subtitle() + geom_slice_caption())
  expect_equal(grDevices::dev.cur(), c("null device" = 1L))
  # KNOWN BUG (for_devs/known_issues.Rmd): geom_slice_text() calls dev.size()
  # with no device open, which opens one (Rplots.pdf under Rscript, a window
  # in RStudio). When this fails, the bug is fixed: expect the null device.
  old <- setwd(tempdir())
  quiet(coloured + geom_slice(model, predict_vars = list(cyl = c(4, 8))) + geom_slice_text())
  expect_false(grDevices::dev.cur() == 1L)
  grDevices::graphics.off()
  setwd(old)
  # With a device already open, nothing is drawn on it: its display list
  # stays empty. (svg() needs cairo, which not every platform has.)
  grDevices::pdf(NULL)
  grDevices::dev.control("enable")
  quiet(coloured + geom_slice(model, predict_vars = list(cyl = c(4, 8))) + geom_slice_text() +
          geom_slice_subtitle())
  expect_length(grDevices::recordPlot()[[1]], 0)
  grDevices::dev.off()
})
