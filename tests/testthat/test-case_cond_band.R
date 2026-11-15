# CASE: cond_band
# A band that can't be resolved warns and the line is drawn without it; a
# band over the data range says which range it used.

mpg <- ggplot2::mpg
model <- lm(hwy ~ displ * drv + cyl + year, data = mpg)
coloured <- function(...) function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = drv)) + geom_slice(...)
}

covers("shared.band.unresolvable", "shared.band.range")

raises("band_range_message", coloured(lm(hwy ~ displ * drv + cyl, mpg), band = "cyl"),
       "message", "Band range for `cyl` not specified",
       purpose = "A band named without values spans the data range, and the message says which range it used.")
raises("band_true_several_multi", coloured(model, band = TRUE,
                                           predict_vars = list(cyl = c(4, 8), year = c(1999, 2008))),
       "warning", "band = TRUE is ambiguous: several",
       purpose = "band = TRUE with two multi-value predict_vars entries can't tell which to band, so it warns and draws without a band.")
raises("band_true_no_candidate", coloured(lm(hwy ~ displ * drv, mpg), band = TRUE),
       "warning", "no variable is available",
       purpose = "band = TRUE on a model with no numeric predictor left to band warns that nothing is available.")
raises("band_true_several_candidates", coloured(model, band = TRUE),
       c("warning", "message", "message"), "band = TRUE is ambiguous: any of",
       purpose = "band = TRUE with two numeric predictors free to band (cyl, year) warns that the choice is ambiguous and lists them.")
raises("band_not_predictor", coloured(model, band = "cty"),
       c("warning", "message", "message"), '"cty" is not a predictor',
       purpose = "A band named for a variable outside the model warns and lists the predictors that could band.")
raises("band_on_x", coloured(model, band = "displ"),
       c("warning", "message", "message"), "is on the x-axis",
       purpose = "A band named for the x-axis variable warns, since it already varies along the line.")
raises("band_pinned", coloured(model, band = "drv"),
       c("warning", "message", "message"), "used by the plot's grouping or faceting",
       purpose = "A band named for the colour grouping's variable warns, since each line is pinned to one of its levels.")
raises("band_not_numeric", function() {
  ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + geom_slice(lm(hwy ~ displ + drv, mpg), band = "drv")
}, c("warning", "message"), "is not numeric, so it has no range",
       purpose = "A band named for a factor warns that a factor has no range, suggesting grouping instead.")
