# CASE: cond_imputation
# Imputation messages: one per imputed predictor, once per layer however
# many panels and groups, in each engine's own wording.

mpg <- ggplot2::mpg
d <- data.frame(x = 1:9, f = factor(c("a", "b", "b", "c", "c", "c", "a", "b", "c")),
                s = c("p", "q", "q", "q", "p", "q", "p", "q", "q"), y = c(3, 5, 4, 8, 9, 12, 10, 13, 16))

raises("factor_and_character", function() {
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_slice(lm(y ~ x + f + s, d))
}, c("message", "message"), c('Used most common: "c"', 'Used most common: "q"'),
       covers = c("shared.imputation.message", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "The imputation message for a factor and a character predictor, the two non-numeric kinds; condition_imputed_values/wording has the numeric one.")
raises("once_per_layer", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) +
    geom_slice(lm(hwy ~ displ * drv + cyl + year, mpg)) + ggplot2::facet_wrap(~fl)
}, c("message", "message"), c("`cyl`", "`year`"),
       covers = c("shared.imputation.once-per-layer", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Catches the message repeating per colour group or facet panel (21 here), which would flood the console.")
raises("companions_stay_quiet", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
    geom_slice(lm(hwy ~ displ + cyl + year, mpg), predict_vars = list(year = c(1999, 2008))) +
    geom_slice_text() + geom_slice_subtitle() + geom_slice_caption()
}, "message", "Value for `cyl` not specified",
       covers = c("geom_slice_text.silent-rebuild", "geom_slice_subtitle.no-repeat-messages", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "Catches text, subtitle or caption layers repeating geom_slice()'s message when they re-resolve the slice: only one message must appear.")
raises("slice_2d_wording", function() {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  slice_2d(lm(y ~ x + f, d), x_axis = "x")
}, "message", "Value for `f` not specified - Used first value: a", format = FALSE,
       covers = c("shared.imputation.engines-differ", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "slice_2d()'s different wording and choice (first level) side by side with factor_and_character; you can judge the difference. Its hint 'f = a' lacks quotes.")
raises("slice_2d_character", function() slice_2d(lm(y ~ x + s, d), x_axis = "x"),
       "error", 'unsupported class "character"',
       covers = c("shared.imputation.engines-differ", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "slice_2d() stops on a character predictor that geom_slice() imputes happily; the stop must name the class and what slice_2d() supports.")
