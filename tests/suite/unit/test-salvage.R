# TEMPORARY (ticket 4): the salvage audit is read with each addition's code,
# a broken audit says what's wrong, and a verdict reaches salvage_review.json
# only as JSON from the page.

audit_file <- function(lines) {
  f <- tempfile(fileext = ".yml")
  writeLines(lines, f)
  f
}
good_audit <- function() audit_file(c(
  "additions:",
  "  - id: line-check",
  "    case: single_predictor_line",
  "    tests: [\"plot\", \"the plotted line equals the model's prediction on the expected grid\"]",
  "    entries: [geom_slice.line-is-prediction]",
  "    why: Lines were judged by eye.",
  "old:",
  "  - {id: gs_01, verdict: added, added: [line-check], note: One line.}",
  "  - {id: gs_02, verdict: covered, by: [single_predictor_line], note: Covered.}",
  "  - {id: gs_03, verdict: discarded, note: Old infrastructure.}"
))
salvage_app <- function(file = good_audit()) {
  paths <- temp_paths()
  dir.create(dirname(paths$state), recursive = TRUE, showWarnings = FALSE)
  list(paths = paths, app = review_app(paths, page_dir = file.path(suite_dir, "page"),
                                       inventory_file = inventory_path(root), case_dir = case_copy(),
                                       salvage_file = file))
}
verdict_body <- function(...) jsonlite::toJSON(list(...), auto_unbox = TRUE)

test_that("the audit is served with each addition's code and the old lines it came from", {
  s <- salvage_app()
  data <- json_body(call_app(s$app, "/api/salvage"))
  a <- data$additions[[1]]
  expect_equal(a$from, list("gs_01"))
  expect_match(a$tests[[1]]$code, '^judged_output\\("plot", p\\)')
  expect_match(a$tests[[2]]$code, "^test_that\\(")
  expect_length(data$old, 3)
  expect_length(data$reviews, 0)
})

test_that("an audit naming a missing case, test or addition, or a bad verdict, is reported", {
  broken <- audit_file(c(
    "additions:",
    "  - {id: a, case: no_such_case, tests: [x]}",
    "  - {id: b, case: single_predictor_line, tests: [\"no such test\"]}",
    "old:",
    "  - {id: o1, verdict: kept}",
    "  - {id: o2, verdict: covered, by: [nope]}",
    "  - {id: o3, verdict: added, added: [zzz]}",
    "  - {id: o3, verdict: discarded}"
  ))
  err <- json_body(call_app(salvage_app(broken)$app, "/api/salvage"))$error
  for (p in c("'no_such_case'", "'no such test'", "verdict 'kept'", "'nope', which isn't a case",
              "addition 'zzz'", "'o3' is used more than once")) {
    expect_match(err, p, fixed = TRUE)
  }
})

test_that("a verdict from the page is written; disagreeing needs a note; the latest wins", {
  s <- salvage_app()
  post <- function(...) call_app(s$app, "/api/salvage", method = "POST", body = verdict_body(...))
  expect_equal(post(id = "gs_03", verdict = "disagree", note = "")$status, 409L)
  expect_equal(post(id = "gs_99", verdict = "agree")$status, 409L)
  expect_false(file.exists(salvage_review_path(s$paths)))
  expect_equal(post(id = "line-check", verdict = "agree")$status, 200L)
  expect_equal(post(id = "gs_03", verdict = "disagree", note = "Keep this one.")$status, 200L)
  expect_equal(post(id = "gs_03", verdict = "agree")$status, 200L)
  reviews <- json_body(call_app(s$app, "/api/salvage"))$reviews
  expect_equal(reviews$`line-check`$verdict, "agree")
  expect_equal(reviews$gs_03$verdict, "agree")
  expect_length(read_salvage_reviews(s$paths), 3)
})

test_that("only JSON from the page's own origin can record a verdict", {
  s <- salvage_app()
  body <- verdict_body(id = "gs_03", verdict = "agree")
  expect_equal(call_app(s$app, "/api/salvage", method = "POST", body = body,
                        type = "application/x-www-form-urlencoded")$status, 415L)
  expect_equal(call_app(s$app, "/api/salvage", method = "POST", body = body,
                        origin = "http://evil.example")$status, 403L)
  expect_false(file.exists(salvage_review_path(s$paths)))
})
