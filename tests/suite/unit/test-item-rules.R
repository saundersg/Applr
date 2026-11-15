# Outputs and tests that cite their own inventory entries (`covers =`): what
# the case reader says of them, and the `source` part that sends them back
# for review when what they check changes.

fingerprints <- function(dir) {
  r <- run_case_file(file.path(dir, "test-case_base_slice_2d.R"))
  expect_true(r$passed)
  vapply(r$outputs, function(e) serialise_output(e)$fingerprint, "")
}

test_that("a test's checks part lists its expect_*() calls, even past an empty argument", {
  code <- quote({ x <- d[d$g == 1, ]; for (i in 1:2) expect_equal(x[i, ], 1); expect_true(TRUE) })
  expect_equal(expect_calls(code), c("expect_equal(x[i, ], 1)", "expect_true(TRUE)"))
  expect_equal(expect_calls(quote({ NULL })), "(no expect_*() calls)")
})

test_that("each output and test lists only its own entries, and says which it is", {
  o <- case_outputs(case_path("base_slice_2d"))
  expect_equal(unlist(o$plot$covers), c("slice_2d.plot-labels", "slice_2d.caption"))
  expect_equal(o$plot$kind, "output")
  expect_equal(unlist(o$line_is_prediction$covers), "slice_2d.line-is-prediction")
  expect_equal(o$line_is_prediction$kind, "test")
  expect_true(all(c("slice_2d.caption", "slice_2d.line-is-prediction") %in% case_covers(case_path("base_slice_2d"))))
})

test_that("a raises() call says in words what its test checks", {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    'raises("a", f, "error", "bad value", covers = "x.y")',
    'raises("b", f, c("warning", "message"), c("one", "two"), format = FALSE, covers = c("x.y", "x.z"))'
  ), f)
  o <- case_outputs(f)
  expect_equal(unlist(o$a$checks), c("Raises, in order: error", "A message contains 'bad value'",
                                     "That message is in the two-part format"))
  expect_equal(unlist(o$b$checks), c("Raises, in order: warning, message", "A message contains 'one'",
                                     "A message contains 'two'", "The call still finishes and builds its plot"))
  expect_equal(unlist(o$b$covers), c("x.y", "x.z"))
})

test_that("an expectation() is a judged output and still a test that can fail", {
  dir <- case_copy("base_slice_2d")
  r <- run_case_file(file.path(dir, "test-case_base_slice_2d.R"))
  expect_true(r$passed)
  expect_true("base_slice_2d/line_is_prediction" %in% names(r$outputs))
  s <- serialise_output(r$outputs[["base_slice_2d/line_is_prediction"]])
  expect_equal(names(s$part_hashes), c("checks", "source"))

  broken <- case_copy("base_slice_2d", edit = c("hp = 150))))", "hp = 100))))"))
  expect_false(run_case_file(file.path(broken, "test-case_base_slice_2d.R"))$passed)
})

test_that("changing what a test checks, or an item's purpose, changes its fingerprint", {
  before <- fingerprints(case_copy("base_slice_2d"))
  setup <- fingerprints(case_copy("base_slice_2d", edit = c("length.out = 100)", "length.out = 100) + 0")))
  expect_false(identical(before[["base_slice_2d/line_is_prediction"]], setup[["base_slice_2d/line_is_prediction"]]))
  expect_identical(before[["base_slice_2d/plot"]], setup[["base_slice_2d/plot"]])

  why <- fingerprints(case_copy("base_slice_2d", edit = c("Mainly the base_graphics", "Chiefly the base_graphics")))
  expect_false(identical(before[["base_slice_2d/plot"]], why[["base_slice_2d/plot"]]))
})

test_that("an output that cites no entries of its own keeps its old fingerprint", {
  dir <- case_copy(edit = c('covers = "geom_slice.user-theme",', ""))
  before <- run_case_file(file.path(dir, "test-case_single_predictor_line.R"))
  s <- serialise_output(before$outputs[[id]])
  expect_false("source" %in% names(s$part_hashes))
})
