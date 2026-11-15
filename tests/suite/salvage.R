# TEMPORARY (ticket 4): the salvage audit and the maintainer's verdicts on
# it. The audit (for_devs/salvage_audit.yml) is written by an agent; the
# verdicts (salvage_review.json, next to the review state) only by the review
# server. Both go at the ticket's clean-up, with the page's Salvage audit view.

salvage_path <- function(root = ".") file.path(root, "for_devs", "salvage_audit.yml")
salvage_review_path <- function(paths) file.path(dirname(paths$state), "salvage_review.json")

# Top-level calls of a case file keyed by their first argument when it is a
# string: test_that() names, judged_output() and raises() output names.
case_blocks <- function(file) {
  exprs <- parse(file, keep.source = TRUE)
  src <- attr(exprs, "srcref")
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (is.call(e) && length(e) > 1 && is.character(e[[2]]) && length(e[[2]]) == 1) {
      out[[e[[2]]]] <- paste(as.character(src[[i]]), collapse = "\n")
    }
  }
  out
}

# The audit, with each addition's code read from its case file. Stops, listing
# every problem, when a line names a case, addition or test that doesn't exist,
# an ID repeats, or a verdict is unknown.
read_salvage <- function(file, case_dir) {
  raw <- yaml::read_yaml(file)
  cases <- sub("^test-case_(.*)\\.R$", "\\1", list.files(case_dir, "^test-case_.*\\.R$"))
  add_ids <- vapply(raw$additions, function(a) a$id %||% "", "")
  old_ids <- vapply(raw$old, function(o) o$id %||% "", "")
  problems <- c(sprintf("'%s' is used more than once", unique(c(add_ids, old_ids)[duplicated(c(add_ids, old_ids))])))

  additions <- lapply(raw$additions, function(a) {
    file <- file.path(case_dir, paste0("test-case_", a$case, ".R"))
    if (!file.exists(file)) {
      problems <<- c(problems, sprintf("addition '%s' names case '%s', which doesn't exist", a$id, a$case))
      blocks <- list()
    } else {
      blocks <- case_blocks(file)
    }
    tests <- lapply(unlist(a$tests), function(t) {
      if (file.exists(file) && is.null(blocks[[t]])) {
        problems <<- c(problems, sprintf("addition '%s' names '%s', which isn't in %s", a$id, t, basename(file)))
      }
      list(name = t, code = blocks[[t]] %||% "")
    })
    from <- old_ids[vapply(raw$old, function(o) a$id %in% unlist(o$added), TRUE)]
    list(id = a$id, case = a$case, why = a$why %||% "", entries = as.list(unlist(a$entries)),
         tests = tests, from = as.list(from))
  })

  old <- lapply(raw$old, function(o) {
    if (!o$verdict %in% c("covered", "added", "discarded")) {
      problems <<- c(problems, sprintf("'%s' has verdict '%s'", o$id, o$verdict))
    }
    unknown_cases <- setdiff(unlist(o$by), cases)
    unknown_adds <- setdiff(unlist(o$added), add_ids)
    problems <<- c(problems,
      sprintf("'%s' is covered by '%s', which isn't a case", o$id, unknown_cases),
      sprintf("'%s' names addition '%s', which isn't one", o$id, unknown_adds))
    list(id = o$id, verdict = o$verdict, by = as.list(unlist(o$by)), added = as.list(unlist(o$added)),
         note = o$note %||% "")
  })

  if (length(problems)) {
    stop("Salvage audit ", file, ":\n", paste0("  - ", problems, collapse = "\n"), call. = FALSE)
  }
  list(additions = additions, old = old)
}

read_salvage_reviews <- function(paths) {
  f <- salvage_review_path(paths)
  if (file.exists(f)) jsonlite::read_json(f) else list()
}

# The maintainer's verdict on one addition or one old line, appended to the
# reviews file. Disagreeing needs a note, since an agent acts on it.
salvage_review <- function(id, verdict = c("agree", "disagree"), note = "", audit, paths) {
  verdict <- match.arg(verdict)
  known <- c(vapply(audit$additions, `[[`, "", "id"), vapply(audit$old, `[[`, "", "id"))
  if (!id %in% known) stop("Not in the salvage audit: ", id, call. = FALSE)
  note <- trimws(note %||% "")
  if (verdict == "disagree" && !nzchar(note)) stop("Disagreeing needs a note for the agent.", call. = FALSE)
  reviews <- read_salvage_reviews(paths)
  reviews[[length(reviews) + 1]] <- list(id = id, verdict = verdict, note = note, when = now_stamp())
  jsonlite::write_json(reviews, salvage_review_path(paths), pretty = TRUE, auto_unbox = TRUE)
  invisible(reviews[[length(reviews)]])
}

# The latest verdict per ID.
latest_salvage_reviews <- function(paths) {
  out <- list()
  for (r in read_salvage_reviews(paths)) out[[r$id]] <- r
  out
}
