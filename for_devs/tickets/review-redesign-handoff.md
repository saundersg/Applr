# Handoff: review page redesign, built

**Status (2026-11-17):** Built on `test-suite-revision`, waiting for the maintainer to look through it, and then for a review by a fresh session. Not part of the numbered tickets: it came out of a design conversation on 2026-11-16 about why the first draft of the inbox was harder to review than the old suite.

Read first: the spec's *What the maintainer reviews*, *Review page* and *The core* sections (`for_devs/test_suite_spec.md`), and `tests/suite/README.md` (*Writing cases*, *The review page*).

## Why

The first draft of the inbox had far more to read than the old suite. That was for three reasons:
- Every condition message became an item needing Human approved: 100 of 146.
- The pilot (`review-inbox-pilot` branch, now abandoned) put all 110 numeric tests in the inbox as code to audit.
- Each page stacked six blocks of text above the output.

The maintainer decided the following:
- **What they judge.** User experience and style: what a user types, and what they see. Not numeric facts.
- **Trusting the numbers.** That comes from one readable rule, *the core*, not from reading every test.
- **Wording.** It gets its own lower-priority page, edited in place, with no Send back.
- **Numeric tests.** They're reviewed as a plain-language list.

The UI layouts were picked from mockups. Those are on branch `claude/review-ui-prototypes-da94cc` under `for_devs/prototypes/PROTOTYPE_review_ui/`, with the picks in `DECISIONS.md` there. That branch isn't merged here.

## What was built

| What | Where | Commit |
|---|---|---|
| **The core.** `expected_line(model, at, held, to_y, interval)`, with the rule written in plain words above it. Every numeric expectation compares with it. A multi-variable x-axis is covered only at the data points, and the comment says so. | `tests/testthat/helper-core.R` | 5ec3071 |
| **Cases written as user code.** Plain `ggplot()` calls, with no helpers or `suppressMessages()`, and a `title =` on each judged output. Readers for returned text are in `helper-judged.R`. | `tests/testthat/test-case_{look,text,widget}_*.R` and others | 356b510 |
| **Reading a case file.** `case_outputs()` now also returns `title` and `user`: the code a user would type, with `function()` dropped and only the setup it needs, or one entry per example for `c(name = ...)` text. | `tests/suite/inventory.R` | 356b510 |
| **Titles for numeric tests.** A `# Title:` line above each of the 74 `test_that()`s; the description serves as the sentence. Read by `situation_list()`. | `tests/testthat/test-case_num_*.R`, `inventory.R` | 356b510 |
| **The inbox item page ("Stage").** Title, behaviour line, the orange "Why it was made/changed" box, the output on the left, the user's code on the right (kept in view) with Approve and Send back, and details behind buttons. "Reject" is now "Send back". | `tests/suite/page/app.js` (`caseView` and the code above it), `style.css` | 356b510 |
| **The wording page and the numeric tests page** | `tests/suite/page/pages.js`, `style.css` | 356b510 |
| **The wording page's data.** One row per call site with a stable key (file, enclosing function, position). Ticks are saved with the wording's fingerprint, and the set of known sites is recorded so that new rows show as New. | `tests/suite/wording.R`, `server.R` (`/api/wording`, `condition_forms()`) | 356b510 |
| **What each filled-in variable stands for.** Hand-kept; 46 of 59 were guessed, and 13 are blank for the maintainer. | `tests/suite/wording_roles.csv` | 356b510 |
| **`inbox = FALSE` on the conditions type.** It replaces the `blocks_ci` from 5ec3071. CI and the release check ignore wording's review status; the release check reads the wording ticks instead. | `types.R`, `types/conditions.R`, `check.R`, `release.R`, `store.R` | 356b510 |
| **Hook and deny rule for the wording ticks file.** It's protected like the human-decisions file. | `.claude/hooks/protect_human_decisions.R`, `.claude/settings.json` | 356b510 |
| **Unit tests** | `tests/suite/unit/test-wording.R`, plus additions to `test-check.R`, `test-inventory.R` and `test-hook.R` | 356b510 |
| **Five mutation breaks.** Two fixed because the line they target changed, and three new ones for the wording ticks. | `tests/suite/mutate.R` | this handoff's commit |

## Verified

- `devtools::test()`: every case passes.
- `Rscript tests/suite/test.R`: all pass, except `test-salvage.R:36`. That test was already failing at `80d0f12` (a `purpose =` line was added to `single_predictor_line` without updating the salvage test's regex) and is left for ticket 4.
- `Rscript tests/suite/ci.R` locally: 146 outputs, with the 100 wording ones passing and the 46 inbox ones warning (Agent approved). No variants.
- **Plot fingerprints.** Every plot, widget and text output kept its fingerprint through the rewrite, except `text_subtitles/subtitles`, which dropped two `slice_2d()` caption examples that only internal code could produce. Those are now a plain expectation. It's agent-approved, with a note.
- **The page.** All three pages were checked in a browser: the inbox (a plot, base graphics, a widget, a text grid), wording (Custom fill-ins, and a tick end to end, whose file was then deleted) and numeric tests.
- **Mutation check.** Run on Actions after this handoff's commit. The run started before the `mutate.R` fix, 44134626468, can't apply two breaks and should be ignored.

## Known gaps and open items

- **Noisier test log.** It now shows the messages and the printed LaTeX that the user-level case code produces. That's expected, since nothing wraps them any more.
- **Missing scenario roles.** 13 variables in `wording_roles.csv` have no role, so scenario buttons leave them as their R expression.
- **Read ticks and Custom values aren't shared.** The numeric tests page's Read ticks are saved in the browser (`localStorage`), and the Custom fill-ins last for the browser session only. Neither is shared or committed.
- **Load time.** `/api/data` takes about 2 seconds, because it parses `R/` and every case file on each load.
- **The behaviour inventory.** The per-output "Judge it against" button from the mockup was left out, because it needs per-output `covers =`, which only the abandoned pilot had.
- **Parked:** merging the two line engines (`compute_slice_group`, `prepare_slice_data`) into one deep module, and what the core should promise for a derived x-axis. Both are the maintainer's decisions.

## For the reviewing session

Review `git diff 80d0f12..HEAD`. Things worth checking:
1. **`case_outputs()`.** Its parse-data walking (`object_of`, `examples_of`, `code_of`, `tidy`) on case shapes the unit test doesn't cover.
2. **The wording template.** `wording_sites()` keys and templates; `paste()` and `sprintf()`-built messages show up as one `{…}` placeholder.
3. **Example matching.** The regex in `pages.js` `wordingRows()` that finds each message's real example in the condition forms.
4. **`inbox = FALSE` everywhere.** That nothing in `ci.R`, `release.R`, `suite.R list` or the page still treats wording as an inbox item.
5. **Docs.** That the spec, `tests/suite/README.md` and `page/guide.html` agree.

Don't write the wording ticks or human decisions files: only the review server does, and the hook blocks it. A shell command that merely names either file is blocked too, so use the Edit tool for docs that mention them. Run the mutation check on Actions (`gh workflow run mutation-check.yaml --ref test-suite-revision`), not locally, unless the maintainer agrees.
