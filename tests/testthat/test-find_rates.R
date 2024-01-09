# library(EasyABC)

# Check the output
# test_that("Output is the correct form",{
#   expect_type(find_rates(), "vector")
# })

test_that("Error messages work",{
  expect_error(find_rates(meio.range = list(-1,1)))
  expect_error(find_rates(mito.range = list(0,3)))
  expect_error(find_rates(disp.range = list(0.6, 2)))
  expect_error(find_rates(tolerance = 2))
  expect_error(find_rates(num.trials = 5.5))
  expect_error(find_rates(expected = list(0.2, 0.2, 0.5)))
})
