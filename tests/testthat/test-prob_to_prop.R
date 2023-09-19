# prob_to_prop method
test_that("error messages work",{
  expect_error(prob_to_prop(-0.1, 0.5))
  expect_error(prob_to_prop(0.1, 1.5))
  expect_error(prob_to_prop(0.1, 0.5, 1.5))
})

test_that("The code runs under normal inputs",{
  expect_no_error(prob_to_prop(0.3, 0.4, 4))
  expect_equal(prob_to_prop(1, 0.5), 1)
  expect_equal(prob_to_prop(0, 0), 0)
  expect_in(prob_to_prop(0.5, 0, 1), list(1, 0.5, 0))
  expect_in(prob_to_prop(0.5, 0, 2), list(1, 0.25, 0.5, 0.75, 0))
})

# mito_aneu_cells
test_that("error messages work",{
  expect_error(mito_aneu_cells(2, prob.affected = -2))
  expect_error(mito_aneu_cells(0, prob.affected = 2))
  expect_error(mito_aneu_cells(0.1, prob.affected = 0.5))
  expect_error(mito_aneu_cells(0, 0.5, 0.5))
})

test_that("The code runs under normal inputs",{
  expect_in(mito_aneu_cells(0, 1, 0.5), list(1, 2, 0))
  expect_equal(mito_aneu_cells(0, 8, 0.05, T), 2^8)
})