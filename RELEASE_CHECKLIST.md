# r4lineups 2.3.0 release checklist

This is the sole CRAN and JOSS publication candidate. Versions 2.0.0,
2.1.0 and 2.2.0 were never submitted and must not be; their archives in
`cran_submission_2026-10-27/` and earlier are historical.

2.3.0 contains the fixes from three independent reviews
(`codex_qa_1.md`, 28 October; `codex_review_2.md`, 29 October;
`codex_review_3.md`, 30 October 2026) and an independent statistical
audit (`notes/statistical-qa/`, 31 October 2026), together with a
convergence fix in
[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
found by CI. None of these files is in the build. See NEWS.md.

## Completed locally

Version is 2.3.0, NEWS is dated 2026-10-29, and the public surface is
163 exports and 66 registered S3 methods.

Roxygen documentation and `NAMESPACE` were regenerated with no
subsequent drift.

All 36 test files pass: 1,334 passing expectations, no failures, and
five opt-in skips (three live-pyWitness tests, which pass locally with
24 expectations, the MediaPipe face-landmark test and the installed-app
headless-Chrome test).

Ordinary examples, `\donttest{}` examples, installation and all 16
vignettes pass from the source tarball.

`R CMD check --as-cran` with network access passes on macOS 27.0.1/R
4.6.1 with 0 errors, 0 warnings and 0 notes, including the CRAN incoming
feasibility and URL checks and the PDF manual.

The reviewers’ reproductions give the corrected values, and each is a
regression test.

The JOSS manuscript names 2.3.0 and renders cleanly in the Open Journals
job.

Checked candidate: `cran_submission_2026-11-02/r4lineups_2.3.0.tar.gz`,
2,347,222 bytes, SHA-256
`441cabddae64f4841629659f75a45ed609d4eafb603436cc7e337049b65d1273`,
built from the merge commit `94fcb14`, which cuts the check time.
Earlier archives (`cran_submission_2026-10-31b/` and before) are
superseded.

## Publication gates

MIT-relicensing consent recorded for both authors: Colin Tredoux
confirmed on 2026-08-10 that Tamsyn Naylor agreed on 2026-08-09.

The R release/devel/oldrel-1 operating-system matrix passes on the
resubmitted revision: GitHub Actions run `41420279002` on `94fcb14`.

The TeX-equipped reference-manual job produces a PDF (same run).

The official JOSS/Open Journals job produces a clean `paper.pdf`.

The Linux/macOS/Windows pinned-pyWitness workflow passes on commit
`28c8768` (run `40962145807`); the later
[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
fix does not touch the pyWitness bridge.

r-universe built 2.3.0 from `1a4ed4e` for macOS, Windows, WebAssembly
and Linux R-devel. Its Linux R-release check failed on the
[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
convergence test that the release PR fixes.

The browser app at <https://cgtza2.github.io/r4lineups/app/> returns 200
and starts in headless Chrome with every tab present.

After the release PR is merged, confirm that `main` passes the check
matrix, that r-universe’s Linux R-release check passes, and that the
deployed app bundles r4lineups 2.3.0.

Submitted the 31 October archive (SHA-256 `2dd8679f…0468f`) to CRAN on
31 October 2026 through the web form (submission ID 360999); the
maintainer confirmation email was sent to <colin.tredoux@uct.ac.za>.

CRAN’s automatic pretest returned 2 NOTEs on Windows and Debian (a
flagged word in DESCRIPTION and slow examples); fixed in PR \#12.

Resubmitted the `cran_submission_2026-10-31b/` archive (SHA-256
`dfda255e…2b5386`) on 1 November 2026 through the web form (submission
ID 361029); the maintainer confirmation email was sent to
<colin.tredoux@uct.ac.za>.

Maintainer confirmed; both pretests were OK, but CRAN (Uwe Ligges, 1
November 2026) returned 361029 because the whole check took 14 minutes
on Windows (limit 10). Slow tests now skip on CRAN and the slowest
vignette chunks load precomputed results (PR \#13).

Rebuilt from `main` (`94fcb14`); local `R CMD check --as-cran` with
network: Status OK (tests 14 s, vignettes 159 s including the locally
installed pyWitness vignette).

win-builder R-devel (Windows, 2026-11-01 r90825) on the same archive:
Status OK, check time 400 s (tests 62 s, vignettes 116 s; previously 297
s and 328 s on CRAN’s Windows pretest).

Resubmitted the `cran_submission_2026-11-02/` archive (SHA-256
`441cabdd…1273`) on 2 November 2026 through the web form (submission ID
361198); the maintainer confirmation email was sent.

Maintainer confirmed 361198; CRAN accepted 2.3.0 on 2 November 2026
(“package r4lineups_2.3.0.tar.gz is on its way to CRAN”).

Tagged the accepted commit `94fcb14` as `v2.3.0`.

Created the GitHub release for `v2.3.0` with the accepted archive
(<https://github.com/CGTZA2/r4lineups/releases/tag/v2.3.0>).

Submit the JOSS paper.

At successful JOSS review, archive the accepted release, add the archive
DOI to JOSS metadata, and verify that author lists agree.
