test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Check the output
test_that("Output is the correct form",{
  expect_type(find_rates(), "data.frames")
})
