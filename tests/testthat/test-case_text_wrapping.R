# CASE: text_wrapping
# The same long subtitle wrapped to fixed widths, so the result doesn't
# depend on the device: how it breaks, indents and shortens factor names.

iris_model <- lm(Sepal.Length ~ Sepal.Width * Species + Petal.Length, data = iris)
on_iris <- function(...) {
  suppressMessages(ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length, colour = Species)) +
                     geom_slice(iris_model) + geom_slice_subtitle(...))
}
shared_level <- data.frame(
  x = rep(1:6, 2), a = factor(rep(c("Low", "High"), 6), levels = c("Low", "High")),
  b = factor(rep(c("Mid", "Mid", "High"), 4), levels = c("Mid", "High")),
  y = c(2, 5, 4, 8, 6, 9, 3, 6, 5, 9, 7, 11)
)
ambiguous <- lm(y ~ x + a + b, data = shared_level)

examples <- c(
  unwrapped = annotation_of(on_iris(wrap = FALSE)),
  wide_6in = annotation_of(on_iris(wrap = 6)),
  brackets_3.5in = annotation_of(on_iris(wrap = 3.5)),
  narrow_2in = annotation_of(on_iris(wrap = 2)),
  prepend_4in = annotation_of(on_iris(wrap = 4, prepend = "Fitted: ")),
  overlong_1in = annotation_of(on_iris(wrap = 1, model = TRUE)),
  shared_level_1.5in = annotation_of(suppressMessages(
    ggplot2::ggplot(shared_level, ggplot2::aes(x, y)) + geom_slice(ambiguous) + geom_slice_subtitle(wrap = 1.5)))
)

judged_output("wrapped", examples, type = "returned_text",
              covers = c("shared.annotation-wrap.ladder", "shared.annotation-wrap.between-terms", "shared.annotation-wrap.prepend-equals", "shared.annotation-wrap.overlong-term", "shared.annotation-wrap.ambiguous-brackets"),
              purpose = "One long subtitle at fixed widths, so you judge the breaks themselves independent of any device: where it breaks, the indent, when [level] appears and when nothing fits.")

lines <- function(name) strsplit(examples[[name]], "\n", fixed = TRUE)[[1]]
blank <- "^[    ]+"

expectation("breaks_between_terms",
            covers = c("shared.annotation-wrap.width-or-off", "shared.annotation-wrap.between-terms", "shared.annotation-wrap.fits"),
            purpose = "Catches a break inside a term, an operator stranded at a line end, text lost or reordered when wrapping, or wrap = FALSE still wrapping.", {
  expect_length(lines("unwrapped"), 2)
  wrapped <- lines("wide_6in")
  expect_gt(length(wrapped), 2)
  rejoined <- paste(sub(blank, "", wrapped[-length(wrapped)]), collapse = " ")
  expect_equal(rejoined, lines("unwrapped")[1])
  expect_false(any(grepl("[+-]$", wrapped)))
})

expectation("hanging_indent",
            covers = "shared.annotation-wrap.prepend-equals",
            purpose = "Catches continuation lines not indented, or indented under a prefix like 'Fitted: ' instead of under the equation.", {
  wrapped <- lines("wide_6in")
  expect_true(all(grepl(blank, wrapped[2:(length(wrapped) - 1)])))
  pre <- lines("prepend_4in")
  expect_match(pre[1], "^Fitted: Sepal.Length = ")
  expect_true(grepl(blank, pre[2]))
})

expectation("brackets_only_when_needed",
            covers = c("shared.annotation-wrap.ladder", "shared.annotation-wrap.ambiguous-brackets"),
            purpose = "Catches [level] shortening when full names fit, failing to shorten when they don't, or shortening two factors with a shared level to the same label.", {
  expect_false(grepl("[", examples[["wide_6in"]], fixed = TRUE))
  expect_true(grepl("[virginica]", examples[["brackets_3.5in"]], fixed = TRUE))
  expect_false(grepl("[High]", examples[["shared_level_1.5in"]], fixed = TRUE))
})

expectation("overlong_term",
            covers = "shared.annotation-wrap.overlong-term",
            purpose = "Catches a term too wide for the line being split in the middle instead of placed on its own line.", {
  over <- lines("overlong_1in")
  expect_true('- 0.286*Sepal.Width:(Species="virginica")' %in% over)
})

expectation("held_value_split_known_issue",
            covers = "shared.annotation-wrap.between-terms",
            purpose = "Pins a known issue ('Sepal.Width' and '= 3.057' on separate lines) so its fix is noticed: when this fails, the pair stays together.", {
  # KNOWN ISSUE (for_devs/known_issues.Rmd): with one held value the line is
  # broken at any space, so "Sepal.Width = 3.057" can split. When this
  # fails, the pair stays together.
  over <- lines("overlong_1in")
  expect_equal(tail(over, 2), c("held at: Sepal.Width", "= 3.057"))
})
