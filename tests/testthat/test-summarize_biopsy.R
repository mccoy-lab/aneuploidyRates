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
test.df2 <- summarize_biopsy(num.em = 10, 1, 0)
test_that("Code returns desired output", {
  expect_equal(dim(test.df2), c(10, 7))
  expect_equal(test.df2[1,], sample)
  expect_equal(summarize_biopsy(num.em = 10, 1, 0, hide.default.param = FALSE)[1,],
               sample2)
  expect_true(test.df[1, 5] > test.df[1, 7])
  expect_true(test.df[1, 5] > test.df[1, 6])
})

test.df3 <- summarize_biopsy(meio = 0.4, mito = 0.5)
test_that("Code returns desired output", {
  expect_equal(dim(test.df3), c(100,7))
  expect_true(test.df3[1, 5] < test.df3[1, 7])
  expect_true(test.df3[1, 5] <= test.df3[1, 6])
})
