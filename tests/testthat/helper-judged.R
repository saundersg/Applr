# Cases build judged outputs the way a user would type them, because the
# review page shows that code beside the output. So ggplot2 is attached, and
# no case-local helpers or suppressMessages() wrap what a user wouldn't write.
# Messages those calls raise therefore show in the test log; that's expected.
# label_value is left out because Applr has an internal one of that name.
library(ggplot2, exclude = "label_value")

# Declares a judged output from inside a case. `type` names a registered
# output type, or is worked out by the suite when the object alone says it
# (a ggplot, a plotly widget). `states` are the case's declared states, such
# as camera presets. `title` (about three words) and `purpose` (one sentence
# on what it shows) are string literals the review page reads from the case
# file, so they play no part at run time. A no-op unless the dev-only suite
# runner (tests/suite/) has installed a registry, so it is harmless under
# R CMD check.
judged_output <- function(name, object, type = NULL, states = NULL, title = NULL, purpose = NULL) {
  registry <- getOption("applr.judged_registry")
  case <- getOption("applr.judged_case")
  if (is.environment(registry) && !is.null(case)) {
    assign(paste0(case, "/", name), list(object = object, type = type, states = states),
           envir = registry)
  }
  invisible(object)
}

# Names the behaviour inventory entries a case covers, as string literals.
# The review page reads them from the case file; running the case does nothing.
covers <- function(...) invisible(NULL)

# Readers for returned text: each takes a plot as a user built it and returns
# the text they'd see on it. The review page shows the plot code with the
# reader's name as a label, so keep these to one plain job each.
subtitle_of <- function(p) p$labels$subtitle
labels_of <- function(p) paste(sort(slice_text_data(p)$label), collapse = "
")
legend_key_of <- function(p) {
  layer <- Filter(function(l) inherits(l$geom, "GeomText") && !inherits(l$geom, "GeomSliceText"), p$layers)
  if (length(layer)) layer[[1]]$aes_params$label else ""
}
