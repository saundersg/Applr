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

# The inventory IDs a case file cites through covers(), in order.
case_covers <- function(file) {
  found <- character()
  walk <- function(x) {
    if (!is.call(x)) return()
    args <- as.list(x)[-1]
    if (identical(x[[1]], as.name("covers"))) {
      found <<- c(found, unlist(Filter(is.character, args)))
    }
    for (i in seq_along(args)) {
      if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
    }
  }
  for (e in parse(file, keep.source = FALSE)) walk(e)
  unique(found)
}

# Each judged output a case file declares, read without running it, keyed by
# name: its `title` and `purpose`, the top-level call that declares it
# (`code`), and the top-level lines before that call it depends on (`setup`).
# Those are the definitions of the names it uses, followed back, and the plain
# calls (set.seed()) among them.
#
# `user` is what a reviewer is shown: the code a user would type to get the
# output, from the object's assignment or inline in the declaring call, with
# a function() wrapper dropped, and only the setup that code needs. Returned
# text built as c(name = ..., ...) gives one `examples` entry per element
# instead, each with its own setup; a reader around one (subtitle_of(), ...)
# is named apart from the plot code it reads.
declaring <- c("judged_output", "raises")
text_readers <- c("subtitle_of", "labels_of", "legend_key_of")

case_outputs <- function(file) {
  parsed <- parse(file, keep.source = TRUE)
  exprs <- as.list(parsed)
  src <- vapply(attr(parsed, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
  all_pd <- utils::getParseData(parsed)
  top <- all_pd[all_pd$parent == 0, ]
  top <- top$id[order(top$line1, top$col1)]
  pd <- all_pd[all_pd$token != "COMMENT", ]
  pd <- pd[order(pd$line1, pd$col1), ]
  by_parent <- split(seq_len(nrow(pd)), pd$parent)
  kids <- function(id) pd[by_parent[[as.character(id)]] %||% integer(), ]
  text_of <- function(id) utils::getParseText(all_pd, id)
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

  setup_for <- function(need, k) {
    keep <- logical(k - 1)
    for (i in rev(seq_len(k - 1))) {
      if (length(intersect(defines[[i]], need))) {
        keep[i] <- TRUE
        need <- union(need, uses[[i]])
      }
    }
    if (any(keep)) keep <- keep | (plain[seq_len(k - 1)] & seq_len(k - 1) < max(which(keep)))
    as.list(src[which(keep)])
  }
  # An expression's source as a user would read it: continuation lines lose
  # the indentation they had inside the case's own call.
  tidy <- function(text) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    if (length(lines) > 1) {
      rest <- lines[-1]
      cut <- min(nchar(sub("\\S.*", "",rest[nzchar(trimws(rest))])))
      lines[-1] <- paste0("  ", substring(rest, cut + 1))
    }
    paste(lines, collapse = "\n")
  }
  # The code of an expression, unwrapping function() { ... } to its body.
  code_of <- function(id) {
    k <- kids(id)
    if (nrow(k) && k$token[1] == "FUNCTION") {
      body <- k$id[nrow(k)]
      bk <- kids(body)
      if (bk$token[1] == "'{'") {
        inner <- bk$id[bk$token %in% c("expr", "equal_assign", "expr_or_assign_or_help")]
        return(paste(vapply(inner, function(i) tidy(text_of(i)), ""), collapse = "\n"))
      }
      return(tidy(text_of(body)))
    }
    tidy(text_of(id))
  }
  # The second positional argument of a call: the object of judged_output()
  # and raises().
  object_of <- function(id) {
    k <- kids(id)
    args <- k$id[k$token == "expr" & seq_len(nrow(k)) > 1 &
                   !c(FALSE, head(k$token, -1) == "EQ_SUB")]
    if (length(args) >= 2) args[[2]] else NA
  }
  names_in <- function(code) unique(unlist(lapply(parse(text = code, keep.source = FALSE), all.names)))
  call_name <- function(id) {
    k <- kids(id)
    if (!nrow(k) || k$token[1] != "expr") return("")
    f <- kids(k$id[1])
    if (nrow(f) == 1 && f$token == "SYMBOL_FUNCTION_CALL") f$text else ""
  }
  examples_of <- function(id, k) {
    a <- kids(id)
    subs <- which(a$token == "SYMBOL_SUB")
    lapply(stats::setNames(subs, a$text[subs]), function(j) {
      value <- a$id[j + 2]
      reader <- call_name(value)
      if (reader %in% text_readers) value <- object_of_first(value) else reader <- ""
      code <- code_of(value)
      list(reader = reader, code = code, setup = setup_for(names_in(code), k))
    })
  }
  object_of_first <- function(id) {
    k <- kids(id)
    k$id[k$token == "expr"][2]
  }
  user_view <- function(k) {
    obj <- object_of(top[k])
    if (is.na(obj)) return(list(setup = list(), code = ""))
    d <- k
    ok <- kids(obj)
    if (nrow(ok) == 1 && ok$token == "SYMBOL") {
      d <- Find(function(i) ok$text %in% defines[[i]] && assigns[i] && is.name(exprs[[i]][[2]]),
                rev(seq_len(k - 1)))
      if (is.null(d)) return(list(setup = list(), code = ok$text))
      obj <- kids(top[d])$id[3]
    }
    if (call_name(obj) == "c") return(list(examples = examples_of(obj, d)))
    code <- code_of(obj)
    list(setup = setup_for(names_in(code), d), code = code)
  }

  out <- list()
  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    if (!head_of(e) %in% declaring || length(e) < 2 || !is.character(e[[2]])) next
    out[[e[[2]]]] <- list(title = if (is.character(e$title)) e$title else "",
                          purpose = if (is.character(e$purpose)) e$purpose else "",
                          setup = setup_for(uses[[k]], k), code = src[k], user = user_view(k))
  }
  out
}

# The numeric cases as a plain-language list: per file, its top comment, and
# per test its `# Title:` line, its test_that() description as the sentence,
# and its code.
case_situations <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  head <- sub("^#\\s?", "", lines[seq_len(match(FALSE, startsWith(lines, "#")) - 1)])
  head <- head[!startsWith(head, "CASE:")]
  parsed <- parse(file, keep.source = TRUE)
  tests <- list()
  for (i in seq_along(parsed)) {
    e <- parsed[[i]]
    if (!is.call(e) || !identical(e[[1]], as.name("test_that"))) next
    first <- attr(parsed, "srcref")[[i]][1]
    above <- if (first > 1) lines[first - 1] else ""
    tests[[length(tests) + 1]] <- list(
      title = if (startsWith(above, "# Title: ")) substring(above, 10) else "",
      sentence = if (is.character(e[[2]])) e[[2]] else "",
      code = paste(as.character(attr(parsed, "srcref")[[i]]), collapse = "\n"))
  }
  list(name = case_name(file), file = basename(file), heading = paste(head, collapse = " "), tests = tests)
}

situation_list <- function(case_dir, pattern = "^test-case_num_.*\\.R$") {
  lapply(list.files(case_dir, pattern = pattern, full.names = TRUE), case_situations)
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
