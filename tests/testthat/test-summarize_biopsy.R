test_that("Error messages work", {
  expect_error(summarize_biopsy(num.em = -1, 1, 0))
  expect_error(summarize_biopsy(num.em = 1.5, 1, 0))
  expect_error(summarize_biopsy(meio = 0.1, mito = -0.1))
  expect_error(summarize_biopsy(meio = 1.5, mito = 0.2))
})

test_that("Code runs without error", {
  expect_no_error(summarize_biopsy(num.em = 10, 0.5, 0.1))
  expect_no_error(summarize_biopsy(
    num.em = 10,
    0.5,
    0.1,
    dispersal = 0.5,
    hide.default.param = FALSE
  ))
})

sample <- data.frame(
  prop.aneu = 1,
  prob.meio = 1,
  prob.mito = 0,
  dispersal = 0,
  euploid = 0,
  mosaic = 0,
  aneuploid = 1
)
sample2 <- data.frame(
  prop.aneu = 1,
  prob.meio = 1,
  prob.mito = 0,
  dispersal = 0,
  euploid = 0,
  mosaic = 0,
  aneuploid = 1,
  num.cell = 200,
  num.chr = 1,
  concordance = 0
)
test.df <- summarize_biopsy(meio = 0.2, mito = 0.01)
test_that("Code returns desired output", {
  expect_equal(summarize_biopsy(num.em = 10, 1, 0), sample)
  expect_equal(summarize_biopsy(num.em = 10, 1, 0, hide.default.param = FALSE),
               sample2)
  expect_true(test.df$euploid > test.df$aneuploid)
  expect_true(test.df$euploid > test.df$mosaic)
})

test.df2 <- summarize_biopsy(meio = 0.4, mito = 0.5)
test_that("Code returns desired output", {
  expect_true(test.df2$euploid < test.df2$aneuploid)
  expect_true(test.df2$euploid <= test.df2$mosaic)
})
