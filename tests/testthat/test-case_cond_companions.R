# CASE: cond_companions
# Conditions from geom_slice_text(), geom_slice_subtitle() and
# geom_slice_caption(): their arguments, no slice to describe, different
# models, labels too wide, and shortened factor names.

model <- lm(mpg ~ wt + hp, data = mtcars)
other <- lm(mpg ~ wt + qsec, data = mtcars)
base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
two_models <- function(companion) function() {
  base + geom_slice(model, predict_vars = list(hp = 100)) +
    geom_slice(other, predict_vars = list(qsec = 18)) + companion()
}

raises("text_style", function() geom_slice_text(style = "fancy"), "error", "`style` must be one of",
       covers = c("geom_slice_text.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "geom_slice_text()'s style names (variable, value, legend) differ from the subtitle's; the stop must list its own three.")
raises("text_location", function() geom_slice_text(location = "top"), "error",
       '`location` must be "right" or "left".',
       covers = c("geom_slice_text.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "\"top\" is a natural guess for where labels go; the stop must say only right or left exist.")
raises("text_offset", function() geom_slice_text(offset = "5pt"), "error", "`offset` must be a single number",
       covers = c("geom_slice_text.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "ggplot users write sizes as \"5pt\" or unit(5, \"pt\"); the stop must say offset is a plain number of points.")
raises("text_expand", function() geom_slice_text(expand = NA), "error", "`expand` must be TRUE or FALSE.",
       covers = c("geom_slice_text.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "NA slips through many TRUE/FALSE checks; this is the one confirming expand refuses it.")
raises("subtitle_model", function() geom_slice_subtitle(model = "yes"), "error", "`model` must be TRUE or FALSE.",
       covers = c("geom_slice_subtitle.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Here `model` is a TRUE/FALSE switch, not a fitted model as everywhere else in Applr; the hint must make clear what FALSE does.")
raises("subtitle_prepend", function() geom_slice_subtitle(prepend = 1), "error", "`prepend` must be a single string.",
       covers = c("geom_slice_subtitle.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A number given as prepend must stop rather than be pasted in; the hint shows a typical prefix.")
raises("subtitle_append", function() geom_slice_caption(append = c("a", "b")), "error",
       "`append` must be a single string.",
       covers = c("geom_slice_subtitle.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Called through geom_slice_caption(), which shares the subtitle's checks: catches the caption skipping them. Two strings must stop.")
raises("subtitle_wrap", function() geom_slice_subtitle(wrap = -1), "error", "`wrap` must be TRUE, FALSE",
       covers = c("geom_slice_subtitle.arg-validation", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "wrap takes TRUE, FALSE or a width in inches, an unusual mix; the stop must list all three and give an example width.")
raises("text_no_slice", function() base + ggplot2::geom_point() + geom_slice_text(), "warning",
       "geom_slice_text() found no geom_slice() layer",
       covers = c("shared.companions.no-slice-layer", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "geom_slice_text() before or without geom_slice() is an easy ordering slip; the warning must say what's missing and show the right order.")
raises("subtitle_no_slice", function() base + geom_slice_subtitle(), "warning",
       "geom_slice_subtitle() found no geom_slice() layer",
       covers = c("shared.companions.no-slice-layer", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The subtitle's version of text_no_slice, with its own wording; catches the companions drifting apart.")
raises("caption_no_slice", function() base + geom_slice_caption(), "warning",
       "so no caption was added",
       covers = c("shared.companions.no-slice-layer", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The caption's version, the third companion; caught separately because each has its own message.")
raises("text_two_models", two_models(geom_slice_text), "warning", "different models (`model`, `other`)",
       covers = c("shared.companions.different-models", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Labels from two different models would be mixed on one plot; the warning must name both models (as the user called them) and say how to fix it.")
raises("subtitle_two_models", two_models(geom_slice_subtitle), "warning", "so no subtitle was added",
       covers = c("shared.companions.different-models", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The subtitle's version: no single equation fits two models, so nothing is added; catches it picking one silently.")
raises("text_too_wide", function() {
  d <- data.frame(x = 1:10, y = c(2, 4, 3, 6, 5, 8, 7, 9, 11, 10),
                  a_long_variable_name_for_labels = c(1, 3, 2, 5, 4, 7, 6, 8, 10, 9))
  m <- lm(y ~ x + a_long_variable_name_for_labels, d)
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    geom_slice(m, predict_vars = list(a_long_variable_name_for_labels = c(1.23456, 98765.4321))) +
    geom_slice_text(offset = 400)
}, "warning", "cannot fit its labels",
       covers = c("geom_slice_text.too-wide", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Labels too wide would squeeze the data out; the warning must say they're clipped and offer the shorter label styles.")
raises("brackets_used", function() {
  m <- lm(Sepal.Length ~ Sepal.Width * Species + Petal.Length, iris)
  ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Sepal.Length, colour = Species)) +
    geom_slice(m) + geom_slice_subtitle(wrap = 3.5)
}, c("message", "message"), "Factor terms were shortened to fit the subtitle",
       covers = c("shared.annotation-wrap.brackets-message", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Shortened factor terms ([versicolor]) could confuse a reader; the message must show one term both ways and how to keep full names.")
raises("subtitle_bad_style", function() {
  base + geom_slice(model, predict_vars = list(hp = 100)) + geom_slice_subtitle(style = "fancy")
}, "warning", '`style` must be one of "prettier", "brackets", "raw"',
       covers = c("shared.annotation-wrap.invalid-style", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "An unknown style is only caught at build time, so it warns instead of stopping like text_style; you can judge whether that inconsistency is acceptable.")
