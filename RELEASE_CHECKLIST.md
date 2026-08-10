# r4lineups 2.1.0 release checklist

This is the sole CRAN and JOSS publication candidate. Version 2.0.0 is
historical and must not be submitted.

## Completed locally

- [x] Version is 2.1.0 and the public surface is frozen at 159 exports and 54
  registered S3 methods.
- [x] Roxygen documentation and `NAMESPACE` were regenerated with no subsequent
  drift.
- [x] All 23 test files pass from the built source package: 815 passing
  expectations and four documented skips (one deliberately slow simulation and
  three live-pyWitness tests run separately).
- [x] Ordinary examples, `\donttest{}` examples, installation, and all 15
  vignettes pass from the source tarball.
- [x] The separate live suite passes against pinned pyWitness revision
  `e726dcfc09423d0e0ff7f46c8e3a711040293eba`.
- [x] `R CMD check --as-cran --no-manual` passes on macOS/R 4.6.1 with 0
  errors, 0 warnings, and 0 notes, including CRAN incoming feasibility checks.
- [x] `urlchecker::url_check()` returns zero failures.
- [x] The JOSS manuscript is 987 words, parses cleanly, and renders with Pandoc
  and citeproc with all citations resolved; all 16 bibliography DOIs resolve
  through `doi.org`.
- [x] Article PDFs, correspondence, raw manuscript materials, clean JOSS
  sources, development reports, caches, and Python objects are excluded from
  the CRAN archive.
- [x] Checked candidate: `r4lineups_2.1.0.tar.gz`, 2,155,732 bytes, SHA-256
  `b83bec26b8ab0aaca3180b874b76ee2e1aedc7f5b5ce2b040492d2194ac7689d`.

## Publication gates

- [x] MIT-relicensing consent recorded for both authors: Colin Tredoux
  confirmed on 2026-08-10 that Tamsyn Naylor agreed on 2026-08-09.
- [x] Push the exact finalized source revision without changing package inputs.
- [x] Require the R release/devel/oldrel-1 operating-system matrix to pass.
- [x] Require the Linux/macOS/Windows pinned-pyWitness workflow to pass.
- [x] Require the TeX-equipped reference-manual job to produce a PDF.
- [x] Require the official JOSS/Open Journals job to produce a clean `paper.pdf`.
- [x] Insert verified CI results in `cran-comments.md` and the audit report.
- [x] Confirm that the repository candidate tarball still matches the size and
  SHA-256 above; if package inputs changed, rebuild and repeat every check.
- [ ] Submit that exact archive to CRAN. Do not submit either stale historical
  archive.
- [ ] After CRAN accepts the unchanged source, tag the matching commit `v2.1.0`
  and create the GitHub release.
- [ ] Submit the synchronized JOSS paper.
- [ ] At successful JOSS review, archive the accepted release, add the archive
  DOI to JOSS metadata, and verify that author lists agree.
