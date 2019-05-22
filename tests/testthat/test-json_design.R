context("test-json_design")

test_that("defaults", {
  des <- check_design(2,2)
  json <- json_design(des)
  txt <- '{"within":{"A":{"A1":"A1","A2":"A2"}},"between":{"B":{"B1":"B1","B2":"B2"}},"dv":{"y":"Score"},"id":{"id":"Subject ID"},"n":{"B1":100,"B2":100},"mu":{"B1":{"A1":0,"A2":0},"B2":{"A1":0,"A2":0}},"sd":{"B1":{"A1":1,"A2":1},"B2":{"A1":1,"A2":1}},"r":{"B1":[[1,0],[0,1]],"B2":[[1,0],[0,1]]}}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
})
