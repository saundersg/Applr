# The release check: run before a CRAN submission, on the machine the
# approvals were made on. From the package root:
#   Rscript tests/suite/release.R
# Fails while any inbox output is Unreviewed, Rejected or Agent approved, or
# its fingerprint here differs from the recorded one (run suite.R run first),
# or while any message on the wording page isn't ticked as reading well.
# Then runs the release audit: every state is rendered again and compared
# with the view kept at its human approval. Needs chromote with Chrome for
# interactive states, and png.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
source("tests/suite/load.R")
load_suite("tests/suite")

rows <- check_run()
bad <- Filter(function(r) release_verdict(r$status_here, r$case_failed, r$inbox) == "fail" ||
                (r$inbox && !r$match), rows)
for (r in bad) {
  cat(sprintf("  %-45s %s\n", r$id,
              if (r$case_failed) "case expectations fail"
              else if (!r$match) "fingerprint differs from the recorded one; run suite.R run"
              else r$status_here))
}

unticked <- wording_unticked()
for (s in unticked) cat(sprintf("  %-45s wording %s\n", paste0(s$file, ":", s$line),
                                if (identical(s$mark, "bad")) "marked as needing work" else "not ticked"))

audit <- release_audit(Filter(function(r) r$inbox, rows))
changed <- audit[audit != "same"]
for (id in names(changed)) cat(sprintf("  %-45s audit: %s\n", id, changed[[id]]))

cat(sprintf("\n%d outputs: %d not Human approved, %d failing the audit; %d messages not ticked\n",
            length(rows), length(bad), length(changed), length(unticked)))
if (length(bad) || length(changed) || length(unticked)) {
  cat("Release check FAILED. Don't submit to CRAN.\n")
  quit(status = 1)
}
cat("Release check passed.\n")
