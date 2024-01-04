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

sample <- c(
  1, 1, 0, 0, 0, 0, 1
)
sample2 <- c(
   1, 1, 0, 0, 0, 0, 1, 200, 1, 0
)
test.df <- summarize_biopsy(meio = 0.2, mito = 0.01)
test_that("Code returns desired output", {
  expect_equal(summarize_biopsy(num.em = 10, 1, 0), sample)
  expect_equal(summarize_biopsy(num.em = 10, 1, 0, hide.default.param = FALSE),
               sample2)
  expect_true(test.df[5] > test.df[7])
  expect_true(test.df[5] > test.df[6])
})

test.df2 <- summarize_biopsy(meio = 0.4, mito = 0.5)
test_that("Code returns desired output", {
  expect_true(test.df2[5] < test.df2[7])
  expect_true(test.df2[5] <= test.df2[6])
})
