context("test-mtcars")

test_that("multiplication works", {
  expect_equivalent(2 * 2, 4)
})


mtcars_a <- mtcars %>%
  dplyr::group_by(cyl, am) %>%
  dplyr::summarise(mpg_mean = mean(mpg))

mtcars_b <- nanor::nano_means(mtcars, mean_var = mpg, cyl, am)

test_that("mtcars example works", {
  expect_equal(mtcars_a, mtcars_b, tolerance = 0.1)
})
