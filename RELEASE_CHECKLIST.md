# r4lineups 2.3.0 release checklist

This is the sole CRAN and JOSS publication candidate. Versions 2.0.0, 2.1.0
and 2.2.0 were never submitted and must not be; their archives in
`cran_submission_2026-10-27/` and earlier are historical.

2.3.0 contains the fixes from three independent reviews (`codex_qa_1.md`,
28 October; `codex_review_2.md`, 29 October; `codex_review_3.md`, 30 October
2026) and an independent statistical audit (`notes/statistical-qa/`, 31
October 2026), together with a convergence fix in `fit_max_sdt()` found by CI.
None of these files is in the build. See NEWS.md.

## Completed locally

- [x] Version is 2.3.0, NEWS is dated 2026-10-29, and the public surface is
  163 exports and 66 registered S3 methods.
- [x] Roxygen documentation and `NAMESPACE` were regenerated with no
  subsequent drift.
- [x] All 36 test files pass: 1,334 passing expectations, no failures, and
  five opt-in skips (three live-pyWitness tests, which pass locally with 24
  expectations, the MediaPipe face-landmark test and the installed-app
  headless-Chrome test).
- [x] Ordinary examples, `\donttest{}` examples, installation and all 16
  vignettes pass from the source tarball.
- [x] `R CMD check --as-cran` with network access passes on macOS
  27.0.1/R 4.6.1 with 0 errors, 0 warnings and 0 notes, including the CRAN
  incoming feasibility and URL checks and the PDF manual.
- [x] The reviewers' reproductions give the corrected values, and each is a
  regression test.
- [x] The JOSS manuscript names 2.3.0 and renders cleanly in the Open
  Journals job.
- [x] Checked candidate: `cran_submission_2026-10-31b/r4lineups_2.3.0.tar.gz`,
  2,298,002 bytes, SHA-256
  `dfda255ead9fac6d149efd8c7adc937372a1ed277ee78535af370d3bdc2b5386`,
  built from the merge commit `7315cf3`, which fixes the two pretest NOTEs.
  Earlier archives (`cran_submission_2026-10-31/` and before) are superseded.

## Publication gates

- [x] MIT-relicensing consent recorded for both authors: Colin Tredoux
  confirmed on 2026-08-10 that Tamsyn Naylor agreed on 2026-08-09.
- [x] The R release/devel/oldrel-1 operating-system matrix passes on the
  resubmitted revision: GitHub Actions run `41242178346` (tree identical to
  `7315cf3`).
- [x] The TeX-equipped reference-manual job produces a PDF (same run).
- [x] The official JOSS/Open Journals job produces a clean `paper.pdf`.
- [x] The Linux/macOS/Windows pinned-pyWitness workflow passes on commit
  `28c8768` (run `40962145807`); the later `fit_max_sdt()` fix does not touch
  the pyWitness bridge.
- [x] r-universe built 2.3.0 from `1a4ed4e` for macOS, Windows, WebAssembly
  and Linux R-devel. Its Linux R-release check failed on the `fit_max_sdt()`
  convergence test that the release PR fixes.
- [x] The browser app at https://cgtza2.github.io/r4lineups/app/ returns 200
  and starts in headless Chrome with every tab present.
- [ ] After the release PR is merged, confirm that `main` passes the check
  matrix, that r-universe's Linux R-release check passes, and that the
  deployed app bundles r4lineups 2.3.0.
- [x] Submitted the 31 October archive (SHA-256 `2dd8679f…0468f`) to CRAN on
  31 October 2026 through the web form (submission ID 360999); the maintainer
  confirmation email was sent to colin.tredoux@uct.ac.za.
- [x] CRAN's automatic pretest returned 2 NOTEs on Windows and Debian (a
  flagged word in DESCRIPTION and slow examples); fixed in PR #12.
- [ ] Resubmit the `cran_submission_2026-10-31b/` archive through the web form
  (on Colin's go-ahead, planned for 1 November 2026).
- [ ] Maintainer clicks the confirmation link; CRAN pretest and review.
- [ ] After CRAN accepts the unchanged source, tag the matching commit
  `v2.3.0` and create the GitHub release.
- [ ] Submit the JOSS paper.
- [ ] At successful JOSS review, archive the accepted release, add the archive
  DOI to JOSS metadata, and verify that author lists agree.
