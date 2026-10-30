# r4lineups 2.3.0 release checklist

This is the sole CRAN and JOSS publication candidate. Versions 2.0.0,
2.1.0 and 2.2.0 were never submitted and must not be; their archives in
`cran_submission_2026-10-27/` and earlier are historical.

2.3.0 contains the fixes from two independent statistical reviews
(`codex_qa_1.md`, 28 October 2026, and `codex_review_2.md`, 29 October
2026; neither file is in the build) and a convergence fix in
[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
found by CI after the second set of fixes was merged. See NEWS.md.

## Completed locally

Version is 2.3.0, NEWS is dated 2026-10-29, and the public surface is
163 exports and 66 registered S3 methods.

Roxygen documentation and `NAMESPACE` were regenerated with no
subsequent drift.

All 35 test files pass from the built source package: 1,282 passing
expectations, no failures, and five opt-in skips: three live-pyWitness
tests, covered by the pyWitness workflow below, and the MediaPipe
face-landmark and installed-app headless-Chrome tests, which passed in
the second review’s run on the 2.3.0 sources.

Ordinary examples, `\donttest{}` examples, installation and all 16
vignettes pass from the source tarball.

`R CMD check --as-cran --no-manual --run-donttest` with network access
passes on macOS 27.0.1/R 4.6.1 with 0 errors, 0 warnings and 0 notes,
including the CRAN incoming feasibility and URL checks.

The reviewers’ reproductions give the corrected values, and each is a
regression test.

The JOSS manuscript names 2.3.0 and renders cleanly in the Open Journals
job.

Checked candidate: `cran_submission_2026-10-29/r4lineups_2.3.0.tar.gz`,
2,080,831 bytes, SHA-256
`b44dbc9badea79cb0b5508c135f35fced55abadbe3b88640fdfb7819cf1227ec`,
built from commit `40de5bd` (package inputs identical to the release
merge).

## Publication gates

MIT-relicensing consent recorded for both authors: Colin Tredoux
confirmed on 2026-08-10 that Tamsyn Naylor agreed on 2026-08-09.

The R release/devel/oldrel-1 operating-system matrix passes on the
release revision: GitHub Actions run `40967586729`.

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

Submit the 29 October archive to CRAN. Do not submit any earlier
archive.

After CRAN accepts the unchanged source, tag the matching commit
`v2.3.0` and create the GitHub release.

Submit the JOSS paper.

At successful JOSS review, archive the accepted release, add the archive
DOI to JOSS metadata, and verify that author lists agree.
