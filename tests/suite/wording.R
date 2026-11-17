# The wording page: every message the package can print, one row per call
# site in R/, read from the source without running it. The maintainer ticks
# each one through the review page; the review server is the only writer of
# wording_review.json. A tick counts only while the call site's wording is
# the wording that was ticked, and the release check needs every site ticked.

wording_functions <- c(slice_abort = "error", slice_warn = "warning", slice_inform = "message",
                       stop = "error", warning = "warning", message = "message")

# A message argument as a template: literal text kept, anything computed
# shown as {expression}. paste() and paste0() are followed into.
wording_template <- function(e) {
  if (is.null(e)) return(NULL)
  if (is.character(e)) return(paste(e, collapse = ""))
  if (is.call(e) && as.character(e[[1]])[1] %in% c("paste0", "paste")) {
    a <- as.list(e)[-1]
    sep <- if (as.character(e[[1]]) == "paste") a$sep %||% " " else ""
    if (!is.null(names(a))) a <- a[!names(a) %in% c("sep", "collapse")]
    return(paste(vapply(a, function(x) wording_template(x) %||% "", ""), collapse = sep))
  }
  paste0("{", paste(deparse(e, width.cutoff = 200), collapse = " "), "}")
}

# One row per call to a wording function in `dir`: its kind, file and line,
# the enclosing top-level function, its template (`what`, `hint`), its code,
# and a `key` that survives lines moving (file, enclosing function and the
# call's position within it). `fingerprint` changes when the wording does.
wording_sites <- function(dir = "R") {
  rows <- list()
  for (f in sort(list.files(dir, "\\.R$", full.names = TRUE))) {
    parsed <- parse(f, keep.source = TRUE)
    pd <- utils::getParseData(parsed)
    top <- pd[pd$parent == 0, ]
    owner <- function(line) {
      hit <- top[top$line1 <= line & top$line2 >= line, ]
      if (!nrow(hit)) return("")
      e <- tryCatch(str2lang(utils::getParseText(pd, hit$id[1])), error = function(err) NULL)
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) as.character(e[[2]]) else ""
    }
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% names(wording_functions), ]
    calls <- calls[order(calls$line1, calls$col1), ]
    seen <- list()
    for (i in seq_len(nrow(calls))) {
      fn <- calls$text[i]
      if (basename(f) == "conditions.R" && fn %in% c("stop", "warning", "message")) next
      id <- pd$parent[pd$id == calls$parent[i]]
      code <- utils::getParseText(pd, id)
      e <- tryCatch(str2lang(code), error = function(err) NULL)
      if (is.null(e)) next
      a <- as.list(e)[-1]
      nm <- names(a) %||% rep("", length(a))
      if (startsWith(fn, "slice_")) {
        what <- if ("what" %in% nm) a$what else a[[which(nm == "")[1]]]
        hint <- if ("hint" %in% nm) a$hint else if (sum(nm == "") > 1) a[[which(nm == "")[2]]] else NULL
      } else {
        what <- as.call(c(as.name("paste0"), a[!nm %in% c("call.", "domain", "immediate.", "appendLF")]))
        hint <- NULL
      }
      fun <- owner(calls$line1[i])
      if (!nzchar(fun)) fun <- "(top level)"
      seen[[fun]] <- (seen[[fun]] %||% 0) + 1
      what <- wording_template(what)
      hint <- wording_template(hint)
      rows[[length(rows) + 1]] <- list(
        key = sprintf("%s:%s#%d", basename(f), fun, seen[[fun]]),
        fn = fn, kind = unname(wording_functions[fn]), file = paste0("R/", basename(f)),
        line = calls$line1[i], what = what, hint = hint, code = code,
        fingerprint = substr(digest::digest(paste(what, hint %||% "", sep = "\n"), algo = "sha256", serialize = FALSE), 1, 12)
      )
    }
  }
  rows
}

# wording_review.json: every mark in order, and `known`, the call sites that
# existed at the last mark, so a site added since shows as new.
read_wording_review <- function(paths) {
  if (!file.exists(paths$wording)) return(list(known = character(), marks = list()))
  r <- jsonlite::read_json(paths$wording)
  list(known = unlist(r$known) %||% character(), marks = r$marks %||% list())
}

# The latest mark for each key, as recorded by the review server.
latest_wording_marks <- function(paths) {
  out <- list()
  for (m in read_wording_review(paths)$marks) out[[m$key]] <- m
  out
}

# Records the maintainer's mark on one call site: "ok" (reads well), "bad"
# (needs work) or "" (cleared). `fingerprint` is the one the page showed, so
# a mark on wording that changed since is refused.
wording_mark <- function(key, fingerprint, mark, paths = suite_paths(), dir = "R") {
  if (!mark %in% c("ok", "bad", "")) stop("A mark is \"ok\", \"bad\" or \"\".", call. = FALSE)
  sites <- wording_sites(dir)
  site <- Find(function(s) identical(s$key, key), sites)
  if (is.null(site)) stop("No such message: ", key, call. = FALSE)
  if (!identical(site$fingerprint, fingerprint)) {
    stop("This message's wording changed since the page loaded. Reload and look again.", call. = FALSE)
  }
  review <- read_wording_review(paths)
  review$marks[[length(review$marks) + 1]] <- list(key = key, fingerprint = fingerprint, mark = mark,
                                                   what = site$what, hint = site$hint, when = now_stamp())
  review$known <- vapply(sites, `[[`, "", "key")
  dir.create(dirname(paths$wording), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(review, paths$wording, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(review$marks[[length(review$marks)]])
}

# Each site with its standing: `mark` while its wording is the marked one,
# and `status`: "changed" (marked, but the wording has changed since; `was`
# holds what was marked), "new" (added since the last mark) or "".
wording_with_marks <- function(sites, paths) {
  review <- read_wording_review(paths)
  marks <- latest_wording_marks(paths)
  lapply(sites, function(s) {
    m <- marks[[s$key]]
    current <- !is.null(m) && identical(m$fingerprint, s$fingerprint)
    s$mark <- if (current) m$mark else ""
    s$status <- if (!is.null(m) && !current) "changed"
      else if (length(review$known) && !s$key %in% review$known) "new" else ""
    s$was <- if (!is.null(m) && !current) paste(c(m$what, m$hint), collapse = "\n    ")
    s
  })
}

# Sites the release check refuses: not ticked as reading well for their
# current wording.
wording_unticked <- function(paths = suite_paths(), dir = "R") {
  Filter(function(s) !identical(s$mark, "ok"), wording_with_marks(wording_sites(dir), paths))
}

# What each filled-in expression stands for, so the page's scenarios can fill
# it: a small hand-kept table (expression, role), roles being column, value,
# fn, model, response or cls. An expression not in it stays as written.
read_wording_roles <- function(file = "tests/suite/wording_roles.csv") {
  if (!file.exists(file)) return(list())
  t <- utils::read.csv(file, stringsAsFactors = FALSE, strip.white = TRUE)
  as.list(stats::setNames(t$role, t$expression))
}
