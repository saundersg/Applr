# The behaviour inventory (spec: "Write, then salvage") and which cases cover
# it. The format is described in tests/suite/README.md, "Behaviour inventory".
# Cases cite entries with covers("<id>", ...), read here from the case file
# without running it.

inventory_path <- function(root = ".") file.path(root, "for_devs", "behaviour_inventory.yml")

# Flattens the inventory into one record per entry. Stops, listing every
# problem, when an entry has no ID or principle, or an ID is used twice.
read_inventory <- function(file) {
  raw <- yaml::read_yaml(file)
  groups <- list()
  entries <- list()
  for (section in c("functions", "shared")) {
    for (name in names(raw[[section]])) {
      g <- raw[[section]][[name]]
      family <- if (section == "shared") "shared behaviours" else g$family %||% name
      groups[[length(groups) + 1]] <- list(
        name = name, shared = section == "shared", family = family,
        purpose = g$purpose %||% "", functions = as.list(unlist(g$functions))
      )
      for (kind in c("behaviours", "edge_cases")) {
        for (e in g[[kind]]) {
          entries[[length(entries) + 1]] <- list(
            id = e$id %||% "", group = name, shared = section == "shared", family = family,
            kind = if (kind == "behaviours") "behaviour" else "edge case",
            principle = e$principle %||% "", source = e$source %||% "",
            options = as.list(unlist(e$options)), deferred = e$deferred %||% ""
          )
        }
      }
    }
  }
  ids <- vapply(entries, `[[`, "", "id")
  problems <- c(
    sprintf("an entry in '%s' has no valid id (lower case letters, digits, '.', '_', '-')",
            vapply(entries, `[[`, "", "group")[!grepl("^[a-z0-9][a-z0-9_.-]*$", ids)]),
    sprintf("'%s' has no principle", ids[!nzchar(trimws(vapply(entries, `[[`, "", "principle")))]),
    sprintf("'%s' is used more than once", unique(ids[duplicated(ids)]))
  )
  if (length(problems)) {
    stop("Behaviour inventory ", file, ":\n", paste0("  - ", problems, collapse = "\n"), call. = FALSE)
  }
  list(groups = groups, entries = entries)
}

# The inventory IDs a case file cites, in order: through covers(), and the
# `covers =` of each declaring call.
case_covers <- function(file) {
  found <- character()
  walk <- function(x) {
    if (!is.call(x)) return()
    args <- as.list(x)[-1]
    if (identical(x[[1]], as.name("covers"))) {
      found <<- c(found, unlist(Filter(is.character, args)))
    }
    if (is.name(x[[1]]) && as.character(x[[1]]) %in% declaring) found <<- c(found, cited(x$covers))
    for (i in seq_along(args)) {
      if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
    }
  }
  for (e in parse(file, keep.source = FALSE)) walk(e)
  unique(found)
}

# Each judged output a case file declares, read without running it, keyed by
# name: its `purpose`, the top-level call that declares it, and the top-level
# lines before that call it depends on. Those are the definitions of the
# names it uses, followed back, and the plain calls (set.seed()) among them.
# An output that names its own inventory entries (`covers =`) also gets
# `kind` ("test" for an expectation(), else "output") and, for raises(), the
# `checks` its hidden test makes, in words.
declaring <- c("judged_output", "raises", "expectation")

cited <- function(x) {
  if (is.character(x)) return(x)
  if (is.call(x) && identical(x[[1]], as.name("c"))) return(unlist(Filter(is.character, as.list(x)[-1])))
  character()
}

# raises()'s signature (tests/testthat/helper-slice.R), to read its arguments
# by name, and what its test checks given them.
raises_args <- function(name, call, kinds, has = character(), format = TRUE, purpose = NULL, covers = NULL) NULL

raises_checks <- function(e) {
  a <- as.list(match.call(raises_args, e))
  lit <- function(x, default) if (is.null(x)) default else if (is.call(x)) eval(x, baseenv()) else x
  kinds <- lit(a$kinds, character())
  format <- isTRUE(lit(a$format, TRUE))
  c(paste0("Raises, in order: ", paste(kinds, collapse = ", ")),
    unlist(lapply(lit(a$has, character()), function(h) c(
      paste0("A message contains '", h, "'"),
      if (format) "That message is in the two-part format"))),
    if (!"error" %in% kinds) "The call still finishes and builds its plot")
}

case_outputs <- function(file) {
  parsed <- parse(file, keep.source = TRUE)
  exprs <- as.list(parsed)
  src <- vapply(attr(parsed, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
  head_of <- function(e) if (is.call(e) && is.name(e[[1]])) as.character(e[[1]]) else ""
  assigns <- vapply(exprs, function(e) head_of(e) %in% c("<-", "="), TRUE)
  defines <- lapply(seq_along(exprs), function(i) {
    if (!assigns[i]) return(character())
    lhs <- exprs[[i]][[2]]
    while (is.call(lhs)) lhs <- lhs[[2]]
    if (is.name(lhs)) as.character(lhs) else character()
  })
  uses <- lapply(seq_along(exprs), function(i) {
    e <- exprs[[i]]
    all.names(if (assigns[i] && is.name(e[[2]])) e[[3]] else e)
  })
  plain <- !assigns & !vapply(exprs, head_of, "") %in% c(declaring, "covers", "test_that")

  out <- list()
  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    if (!head_of(e) %in% declaring || length(e) < 2 || !is.character(e[[2]])) next
    need <- uses[[k]]
    keep <- logical(k - 1)
    for (i in rev(seq_len(k - 1))) {
      if (length(intersect(defines[[i]], need))) {
        keep[i] <- TRUE
        need <- union(need, uses[[i]])
      }
    }
    if (any(keep)) keep <- keep | (plain[seq_len(k - 1)] & seq_len(k - 1) < max(which(keep)))
    rec <- list(purpose = if (is.character(e$purpose)) e$purpose else "",
                setup = as.list(src[which(keep)]), code = src[k])
    covers <- cited(e$covers)
    if (length(covers)) {
      rec$covers <- as.list(covers)
      rec$kind <- if (head_of(e) == "expectation") "test" else "output"
      if (head_of(e) == "raises") rec$checks <- as.list(raises_checks(e))
    }
    out[[e[[2]]]] <- rec
  }
  out
}

# One record per case file: the IDs it cites, any the inventory doesn't
# know, its family, taken from the first function entry it cites (else the
# first shared one), and what case_outputs() reads of each output.
case_index <- function(case_dir, inventory) {
  ids <- vapply(inventory$entries, `[[`, "", "id")
  shared <- vapply(inventory$entries, `[[`, TRUE, "shared")
  family <- vapply(inventory$entries, `[[`, "", "family")
  files <- list.files(case_dir, pattern = "^test-case_.*\\.R$", full.names = TRUE)
  lapply(files, function(f) {
    cites <- case_covers(f)
    known <- match(cites[cites %in% ids], ids)
    first <- c(known[!shared[known]], known)[1]
    list(name = case_name(f), file = basename(f), covers = as.list(cites),
         unknown = as.list(setdiff(cites, ids)),
         family = if (is.na(first)) "not in the inventory" else family[first],
         outputs = case_outputs(f))
  })
}
