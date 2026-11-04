test_that("calibration populations score responses as in the literature", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect", "filler", "reject"),
    confidence = 90, suspect_pos = 1
  )
  sus <- make_calibration_data(dat, confidence_scale = "0-100")
  expect_equal(sus$population, "suspect_ids")
  expect_equal(sus$n_total, 2)
  expect_equal(sus$overall_accuracy, 0.5)

  # Choosers (Brewer & Wells, 2006): suspect and filler IDs; filler IDs are errors.
  cho <- make_calibration_data(dat, confidence_scale = "0-100", population = "choosers")
  expect_equal(cho$n_total, 4)
  expect_equal(cho$overall_accuracy, 0.25)

  # Nonchoosers: a target-absent rejection is correct.
  non <- make_calibration_data(dat, confidence_scale = "0-100", population = "nonchoosers")
  expect_equal(non$n_total, 2)
  expect_equal(non$overall_accuracy, 0.5)

  # The deprecated FALSE maps to choosers and no longer scores correct
  # rejections as errors.
  old <- suppressWarnings(make_calibration_data(dat, choosers_only = FALSE,
                                                confidence_scale = "0-100"))
  expect_equal(old$population, "choosers")
  expect_error(make_calibration_data(dat, population = "everyone"))
})
