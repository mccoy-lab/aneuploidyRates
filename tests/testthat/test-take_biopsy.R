# library(tessera)

em = tessera::Embryo()
test_that("Error message works",{
  expect_error(take_biopsy(1))
  expect_error(take_biopsy(em, biop.size = 1000))
  expect_error(take_biopsy(em, biop.size = 0))
})

test_that("Biopsy types are correctly identified", {
  expect_equal(take_biopsy(Embryo(prop.aneuploid = 1)), 2)
  expect_equal(take_biopsy(Embryo(prop.aneuploid = 0)), 0)
  expect_equal(take_biopsy(Embryo(prop.aneuploid = 1), biop.size = 1), 2)
})
