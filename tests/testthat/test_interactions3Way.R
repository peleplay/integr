context("Test 3-way interactions function")

#Test interactions3way() for validity of input parameters ----
test_that("interactions3way() throw an error if input parameters are invalid", {
  expect_error(interactions3Way())
  expect_error(interactions3Way(integr::golf))
  expect_error(interactions3Way(integr::golf, classAtt = "Play", speedUp = 1))
  expect_error(interactions3Way(integr::golf, classAtt = "Play", speedUp = "A"))
  expect_error(interactions3Way(integr::golf, classAtt = Play))
  expect_error(interactions3Way(integr::golf, classAtt = 123))
  expect_error(interactions3Way(integr::golf, classAtt = FALSE))
  expect_error(interactions3Way(integr::golf, classAtt = c("A", 123)))
})

#Test that interactions3way() return a list[data.frame, listInfoGains] ----
test_that("interactions3way() return a list[data.frame, listInfoGains]", {
  expect_type(interactions3Way(integr::golf, classAtt = "Play"), "list")
  expect_true(is.data.frame(interactions3Way(integr::golf, classAtt = "Play")[[1]]))
  expect_type(interactions3Way(integr::golf, classAtt = "Play")[[2]], "list")
})
