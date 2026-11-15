# CASE: text_subtitles
# Representative geom_slice_subtitle() texts, unwrapped so the wording is
# judged apart from the plot size, plus slice_2d()'s caption.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)
coloured <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv))
subtitle <- function(p) annotation_of(suppressMessages(p))

examples <- c(
  imputed = subtitle(coloured + geom_slice(model) + geom_slice_subtitle(wrap = FALSE)),
  held_values = subtitle(coloured + geom_slice(model, predict_vars = list(cyl = c(4, 8), year = 2008)) +
                           geom_slice_subtitle(wrap = FALSE)),
  band = subtitle(coloured + geom_slice(model, band = "cyl", predict_vars = list(year = 1999)) +
                    geom_slice_subtitle(wrap = FALSE)),
  no_equation = subtitle(coloured + geom_slice(model) + geom_slice_subtitle(model = FALSE)),
  prepend_append = subtitle(coloured + geom_slice(model) +
                              geom_slice_subtitle(prepend = "Fitted: ", append = " (mpg data)", wrap = FALSE)),
  style_brackets = subtitle(coloured + geom_slice(model) + geom_slice_subtitle(style = "brackets", wrap = FALSE)),
  string_value = subtitle(ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
                            geom_slice(lm(hwy ~ displ + drv, mpg), predict_vars = list(drv = "r")) +
                            geom_slice_subtitle(wrap = FALSE)),
  slice_2d_caption = suppressMessages(prepare_slice_data(model, "displ", 10, TRUE, "none",
                                                         list(drv = "f")))$caption_text,
  slice_2d_caption_empty = prepare_slice_data(lm(hwy ~ displ, mpg), "displ", 10, TRUE, "none", list())$caption_text
)

judged_output("subtitles", examples, type = "returned_text",
              covers = c("geom_slice_subtitle.equation-line", "geom_slice_subtitle.held-line", "geom_slice_subtitle.band-line", "geom_slice_subtitle.prepend-append", "slice_2d.caption", "slice_2d.caption-nothing-held"),
              purpose = "Every kind of subtitle and slice_2d() caption text, unwrapped so you judge wording, not layout (look_labels judges layout). Note the two engines word held values differently (= vs :).")

lines <- function(name) strsplit(examples[[name]], "\n", fixed = TRUE)[[1]]

expectation("equation_then_held",
            covers = c("geom_slice_subtitle.equation-line", "geom_slice_subtitle.held-line"),
            purpose = "Catches the lines coming out of order, held values formatted unlike geom_slice()'s messages, several values not joined with ', ', or a string value losing its quotes.", {
  expect_equal(lines("imputed")[1], lm_equation(model))
  expect_equal(lines("imputed")[2], "held at: cyl = 5.889; year = 2004")
  expect_equal(lines("held_values")[2], "held at: cyl = 4, 8; year = 2008")
  expect_equal(examples[["no_equation"]], "held at: cyl = 5.889; year = 2004")
  expect_equal(lines("string_value")[2], 'held at: drv = "r"')
})

expectation("band_and_options",
            covers = c("geom_slice_subtitle.band-line", "geom_slice_subtitle.prepend-append", "geom_slice_subtitle.dots-to-equation"),
            purpose = "Catches a banded variable reported as held instead of on its own line, prepend or append in the wrong place, or style not reaching the equation.", {
  expect_equal(lines("band")[2:3], c("held at: year = 1999", "projection band: cyl spanning from 4 to 8"))
  expect_match(examples[["prepend_append"]], "^Fitted: hwy = .* \\(mpg data\\)$")
  expect_equal(lines("style_brackets")[1], lm_equation(model, style = "brackets"))
})

expectation("slice_2d_caption",
            covers = c("slice_2d.caption", "slice_2d.caption-nothing-held"),
            purpose = "Catches slice_2d()'s caption rounding to other than 1 decimal, missing a held predictor, or leaving a stray separator when nothing is held.", {
  expect_equal(examples[["slice_2d_caption"]], "drv: f; cyl: 5.9; year: 2003.5")
  expect_equal(examples[["slice_2d_caption_empty"]], "")
})
