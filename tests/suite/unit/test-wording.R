# The wording page's backend: call sites read from R/ source, and the
# maintainer's ticks, which count only for the wording they were given on.

pkg_with <- function(lines) {
  dir <- tempfile("R")
  dir.create(dir)
  writeLines(lines, file.path(dir, "f.R"))
  dir
}

two_messages <- c(
  "f <- function(x) {",
  "  if (x) slice_warn(paste0(\"Value `\", x, \"` is odd.\"), \"Use another.\")",
  "  stop(\"Plain \", x)",
  "}",
  "g <- function() slice_inform(\"Done.\")"
)

test_that("each call site is read as a template, keyed by its function and position", {
  s <- wording_sites(pkg_with(two_messages))
  expect_equal(vapply(s, `[[`, "", "key"), c("f.R:f#1", "f.R:f#2", "f.R:g#1"))
  expect_equal(vapply(s, `[[`, "", "kind"), c("warning", "error", "message"))
  expect_equal(s[[1]]$what, "Value `{x}` is odd.")
  expect_equal(s[[1]]$hint, "Use another.")
  expect_equal(s[[2]]$what, "Plain {x}")
  expect_equal(s[[1]]$line, 2)
})

test_that("a tick counts until the wording changes, which shows what was ticked", {
  paths <- temp_paths()
  dir <- pkg_with(two_messages)
  s <- wording_sites(dir)
  wording_mark("f.R:f#1", s[[1]]$fingerprint, "ok", paths, dir)
  rows <- wording_with_marks(wording_sites(dir), paths)
  expect_equal(vapply(rows, `[[`, "", "mark"), c("ok", "", ""))
  expect_equal(vapply(rows, `[[`, "", "status"), c("", "", ""))
  expect_length(wording_unticked(paths, dir), 2)

  writeLines(sub("is odd", "is strange", two_messages), file.path(dir, "f.R"))
  rows <- wording_with_marks(wording_sites(dir), paths)
  expect_equal(rows[[1]]$mark, "")
  expect_equal(rows[[1]]$status, "changed")
  expect_match(rows[[1]]$was, "is odd")
})

test_that("a message added after the last tick is new", {
  paths <- temp_paths()
  dir <- pkg_with(two_messages)
  wording_mark("f.R:g#1", wording_sites(dir)[[3]]$fingerprint, "bad", paths, dir)
  writeLines(c(two_messages, "h <- function() slice_abort(\"Nope.\")"), file.path(dir, "f.R"))
  rows <- wording_with_marks(wording_sites(dir), paths)
  expect_equal(vapply(rows, `[[`, "", "status"), c("", "", "", "new"))
  expect_equal(rows[[3]]$mark, "bad")
})

test_that("a tick on stale wording, an unknown site or an unknown mark is refused", {
  paths <- temp_paths()
  dir <- pkg_with(two_messages)
  expect_error(wording_mark("f.R:f#1", "000000000000", "ok", paths, dir), "changed since the page loaded")
  expect_error(wording_mark("f.R:z#1", "x", "ok", paths, dir), "No such message")
  expect_error(wording_mark("f.R:f#1", wording_sites(dir)[[1]]$fingerprint, "maybe", paths, dir), "A mark is")
  expect_false(file.exists(paths$wording))
})

test_that("every message in the package is read, each with a unique key", {
  s <- wording_sites(file.path(root, "R"))
  keys <- vapply(s, `[[`, "", "key")
  expect_gt(length(s), 50)
  expect_false(anyDuplicated(keys) > 0)
})

test_that("the numeric cases give a titled situation list", {
  groups <- situation_list(case_dir)
  expect_gt(length(groups), 10)
  tests <- unlist(lapply(groups, `[[`, "tests"), recursive = FALSE)
  expect_true(all(nzchar(vapply(tests, `[[`, "", "title"))))
  expect_true(all(nzchar(vapply(groups, `[[`, "", "heading"))))
})
