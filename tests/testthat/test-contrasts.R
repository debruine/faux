test_that("deviation_code", {
  x <- factor(1:2, labels = LETTERS[1:2])
  x1 <- deviation_code(x, base = 1)
  x2 <- deviation_code(x, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- deviation_code(x, base = 1)
  x2 <- deviation_code(x, base = 2)
  x3 <- deviation_code(x, base = 3)
  mat1 <- matrix(c(-1/3,  2/3, -1/3, -1/3, -1/3, 2/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A")))
  mat2 <- matrix(c(2/3, -1/3, -1/3, -1/3, -1/3, 2/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".A-B", ".C-B")))
  mat3 <- matrix(c(2/3, -1/3, -1/3, -1/3, 2/3, -1/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".A-C", ".B-C")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
})

# deviation_code, base by name ----
test_that("deviation_code, base by name", {
  x <- factor(1:2, labels = LETTERS[1:2])
  xA <- deviation_code(x, base = "A")
  xB <- deviation_code(x, base = "B")
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(xA), mat1)
  expect_equal(contrasts(xB), mat2)
})

# deviation_code, specify levels ----
test_that("deviation_code, specify levels", {
  x <- 1:2
  lvls <- c("A", "B")
  x1 <- deviation_code(x, levels = lvls, base = 1)
  x2 <- deviation_code(x, levels = lvls, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  
  x <- c("A", "B")
  lvls <- c("B", "A")
  x1 <- deviation_code(x, levels = lvls, base = 1)
  x2 <- deviation_code(x, levels = lvls, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[2:1], ".A-B"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[2:1], ".B-A"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
})

# sum_code ----
test_that("sum_code", {
  x <- factor(1:2, labels = LETTERS[1:2])
  x0 <- sum_code(x)
  x1 <- sum_code(x, omit = 1)
  x2 <- sum_code(x, omit = 2)
  mat1 <- matrix(c(-1, 1), dimnames = list(LETTERS[1:2], ".B-intercept"))
  mat2 <- matrix(c(1, -1), dimnames = list(LETTERS[1:2], ".A-intercept"))
  expect_equal(contrasts(x0), mat2)
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- sum_code(x, omit = 1)
  x2 <- sum_code(x, omit = 2)
  x3 <- sum_code(x)
  
  expect_equal(contrasts(x1) %>% as.vector(), c(-1, 1, 0, -1, 0, 1))
  expect_equal(contrasts(x2) %>% as.vector(), c(1, -1, 0, 0, -1, 1))
  expect_equal(contrasts(x3) %>% as.vector(), c(1, 0, -1, 0, 1, -1))
  expect_equal(
    contrasts(x1) %>% dimnames() %>% `[[`(2), 
    c(".B-intercept", ".C-intercept")
  )
  expect_equal(
    contrasts(x2) %>% dimnames() %>% `[[`(2), 
    c(".A-intercept", ".C-intercept")
  )
  expect_equal(
    contrasts(x3) %>% dimnames() %>% `[[`(2), 
    c(".A-intercept", ".B-intercept")
  )
})


# treatment_code ----
test_that("treatment_code", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- treatment_code(x)
  x2 <- treatment_code(x, base = "B")
  x3 <- treatment_code(x, base = 3)
  mat1 <- matrix(c(0,1,0,0,0,1), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A")))
  mat2 <- matrix(c(1,0,0,0,0,1), 3, 
                 dimnames = list(LETTERS[1:3], c(".A-B", ".C-B")))
  mat3 <- matrix(c(1,0,0,0,1,0), 3, 
                 dimnames = list(LETTERS[1:3], c(".A-C", ".B-C")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
})


# helmert_code ----
test_that("helmert_code", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- helmert_code(x)
  x2 <- helmert_code(x, levels = c("C", "B", "A"))
  mat1 <- matrix(c(-.5, .5, 0, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A.B")))
  mat2 <- matrix(c(-.5, .5, 0, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[3:1], c(".B-C", ".A-C.B")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
})

# poly_code ----
test_that("poly_code", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- poly_code(x)
  x2 <- poly_code(x, levels = c("C", "B", "A"))
  mat1 <- matrix(c(-.7071, 0, .7071, .408, -.8165, .408), 3, 
                 dimnames = list(LETTERS[1:3], c("^1", "^2")))
  mat2 <- mat1
  dimnames(mat2)[[1]] <- c("C", "B", "A")
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
})

# sdif_code ----
test_that("sdif_code", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- sdif_code(x)
  x2 <- sdif_code(x, levels = c("C", "B", "A"))
  mat1 <- matrix(c(-2/3, 1/3, 1/3, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-B")))
  mat2 <- mat1
  dimnames(mat2) <- list(LETTERS[3:1], c(".B-C", ".A-B"))
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
})
