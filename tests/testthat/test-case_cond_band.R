# CASE: cond_band
# A band that can't be resolved warns and the line is drawn without it; a
# band over the data range says which range it used.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)
coloured <- function(...) function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)
}

raises("band_range_message", coloured(lm(hwy ~ displ * drv + cyl, mpg), band = "cyl"),
       "message", "Band range for `cyl` not specified",
       covers = c("shared.band.range", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A band named without values quietly uses the data range; the only message that says which range was used and how to choose another.")
raises("band_true_several_multi", coloured(model, band = TRUE,
                                           predict_vars = list(cyl = c(4, 8), year = c(1999, 2008))),
       "warning", "band = TRUE is ambiguous: several",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "band = TRUE with two multi-value predict_vars entries can't pick; unlike band_true_several_candidates, the ambiguity comes from predict_vars, so the wording differs.")
raises("band_true_no_candidate", coloured(lm(hwy ~ displ * drv, mpg), band = TRUE),
       "warning", "no variable is available",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "band = TRUE when every predictor is already on the plot: nothing can band, so the hint has to explain what a band needs rather than list options.")
raises("band_true_several_candidates", coloured(model, band = TRUE),
       c("warning", "message", "message"), "band = TRUE is ambiguous: any of",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "band = TRUE with two free predictors (cyl, year): the warning must list them and show how to name one.")
raises("band_not_predictor", coloured(model, band = "cty"),
       c("warning", "message", "message"), '"cty" is not a predictor',
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A band named for a column outside the model (cty), likely a typo or mix-up: the warning lists the predictors that could band.")
raises("band_on_x", coloured(model, band = "displ"),
       c("warning", "message", "message"), "is on the x-axis",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A band named for the x variable is a conceptual slip (it already varies along the line); the warning must explain that, not just refuse.")
raises("band_pinned", coloured(model, band = "drv"),
       c("warning", "message", "message"), "used by the plot's grouping or faceting",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A band named for the colour grouping's variable: each line is already pinned to its own level, which the warning must explain.")
raises("band_not_numeric", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + geom_slice(lm(hwy ~ displ + drv, mpg), band = "drv")
}, c("warning", "message"), "is not numeric, so it has no range",
       covers = c("shared.band.unresolvable", "shared.conditions.two-part-format", "shared.conditions.severity"),
       purpose = "A band named for a factor has no range; unlike the other band warnings, the hint suggests a grouping aesthetic instead of another band variable.")
