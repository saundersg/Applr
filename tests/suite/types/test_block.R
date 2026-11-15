# A test from a case, declared with expectation(), so the reviewer sees what
# it checks and sees it again whenever that changes. The object is the test's
# code, unevaluated. Its own part lists the expect_*() calls it makes, one per
# line; the core adds the code as written (the `source` part), since an
# expectation() always cites the inventory entries it checks.

expect_calls <- function(code) {
  found <- character()
  walk <- function(x) {
    if (!is.call(x)) return()
    if (is.name(x[[1]]) && startsWith(as.character(x[[1]]), "expect_")) {
      found <<- c(found, paste(trimws(deparse(x, width.cutoff = 500L)), collapse = " "))
      return()
    }
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
  walk(code)
  if (length(found)) found else "(no expect_*() calls)"
}

register_output_type(
  "test_block",
  states = function(declared) list(code = NULL),
  serialise = function(code, states) {
    stopifnot(is.language(code))
    form <- expect_calls(code)
    list(parts = list(checks = form), view = form)
  },
  render = function(form, state, file, live) {
    writeLines(c("<!doctype html><meta charset='utf-8'><pre>",
                 htmltools::htmlEscape(form), "</pre>"), file, useBytes = TRUE)
  },
  view = "text",
  format = "html",
  demo = "base_slice_2d",
  mutations = list(
    compared = function(x) { x$object <- do.call(substitute, list(x$object, list(grid = quote(other)))); x },
    added = function(x) { x$object <- call("{", x$object, quote(expect_true(TRUE))); x },
    dropped = function(x) { x$object <- quote({ NULL }); x }
  )
)
