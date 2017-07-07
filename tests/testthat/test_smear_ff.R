
library(ff)

context("Smear integers")

test_that("smearing integers works correctly", {
  
  x <- ff(rep(0,20))
  x[c(4,7,8,9,14,15)] <- c(1,2,3,2,1,1)
  
  smear_x <- function (...) {smear_ff(x, ...)[] }
  
  expect_that(smear_x(0,0),   is_identical_to(x[]))
  expect_that(smear_x(0,1),   is_identical_to(c(0, 0, 0, 1, 1, 0, 2, 5, 5, 2, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0)))
  expect_that(smear_x(-1,0),  is_identical_to(c(0, 0, 1, 1, 0, 2, 5, 5, 2, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0)))
  expect_that(smear_x(-1,1),  is_identical_to(c(0, 0, 1, 1, 1, 2, 5, 7, 5, 2, 0, 0, 1, 2, 2, 1, 0, 0, 0, 0)))
  expect_that(smear_x(-3,3),  is_identical_to(c(1, 1, 1, 3, 6, 8, 8, 7, 7, 7, 6, 4, 2, 2, 2, 2, 2, 1, 0, 0)))
  expect_that(smear_x(-1,-2), is_identical_to(c(0, 1, 1, 0, 2, 5, 5, 2, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0)))
  expect_that(smear_x(1,2),   is_identical_to(c(0, 0, 0, 0, 1, 1, 0, 2, 5, 5, 2, 0, 0, 0, 1, 2, 1, 0, 0, 0)))
})


test_that("smearing integers is independent of whether from or to is larger", {
  
  x <- ff(rep(0,20))
  x[c(4,7,8,9,14,15)] <- c(1,2,3,2,1,1)
  
  smear_x <- function (...) {smear_ff(x, ...)[] }
  
  expect_that(smear_x(0,1),   is_identical_to(smear_x(1,0)))
  expect_that(smear_x(-1,0),  is_identical_to(smear_x(0,-1)))
  expect_that(smear_x(-1,-2), is_identical_to(smear_x(-2,-1)))
  expect_that(smear_x(1,2),   is_identical_to(smear_x(2,1)))
})


context("Smear numeric")


test_that("smearing numeric works correctly", {
  
  x <- ff(rep(0,20))
  x[c(4,7,8,9,14,15)] <- c(1.2,-2.5,3.4,-2.8,1,1)
  
  smear_x <- function (...) {smear_ff(x, ...)[] }
  
  expect_that(smear_x(0,0),   is_identical_to(x[]))
  expect_that(smear_x(0,1),   is_equivalent_to(c(0, 0, 0, 1.2, 1.2, 0, -2.5, 0.9, 0.6, -2.8, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0)))
  expect_that(smear_x(-1,0),  is_equivalent_to(c(0, 0, 1.2, 1.2, 0, -2.5, 0.9, 0.6, -2.8, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0)))
  expect_that(smear_x(-1,1),  is_equivalent_to(c(0, 0, 1.2, 1.2, 1.2, -2.5, 0.9, -1.9, 0.6, -2.8, 0, 0, 1, 2, 2, 1, 0, 0, 0, 0)))
  expect_that(smear_x(-3,3),  is_equivalent_to(c(1.2, 1.2, 1.2, -1.3, 2.1, -0.7, -0.7, -1.9, -1.9, -1.9, 1.6, -0.8, 2, 2, 2, 2, 2, 1, 0, 0)))
  expect_that(smear_x(-1,-2), is_equivalent_to(c(0, 1.2, 1.2, 0, -2.5, 0.9, 0.6, -2.8, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0)))
  expect_that(smear_x(1,2),   is_equivalent_to(c(0, 0, 0, 0, 1.2, 1.2, 0, -2.5, 0.9, 0.6, -2.8, 0, 0, 0, 1, 2, 1, 0, 0, 0)))
})


test_that("smearing numeric is independent of whether from or to is larger", {
  
  x <- ff(rep(0,20))
  x[c(4,7,8,9,14,15)] <- c(1.2,2.5,3.4,2.8,1,1)
  
  smear_x <- function (...) {smear_ff(x, ...)[] }
  
  expect_that(smear_x(0,1),   is_identical_to(smear_x(1,0)))
  expect_that(smear_x(-1,0),  is_identical_to(smear_x(0,-1)))
  expect_that(smear_x(-1,-2), is_identical_to(smear_x(-2,-1)))
  expect_that(smear_x(1,2),   is_identical_to(smear_x(2,1)))
})



context("smear_ff vs smear_ff_simple")

test_that("smear_ff and smear_ff_simple return the same results", {
  
  set.seed(42)
  x <- ff(runif(10000))
  
  smear_x <- function (...) {smear_ff(x, ...)[]}
  smear_x_simple <- function (...) {smear_ff_simple(x, ...)[]}
  
  expect_that(smear_x(0,1),    is_identical_to(smear_x_simple(0,1)))
  expect_that(smear_x(-10,10), is_identical_to(smear_x_simple(-10,10)))
  expect_that(smear_x(10,11),  is_identical_to(smear_x_simple(10,11)))
  expect_that(smear_x(-73,73), is_identical_to(smear_x_simple(-73,73)))
})









