context("Test Helping Functions")

#Test helping functions for missing/incorrect input parameters ----
test_that("Helping functions throw error if a parameter is missing/incorrect", {

  #Test entropy()
  expect_error(entropy())
  expect_error(entropy(integr::golf))
  expect_error(entropy(integr::golf, classAtt = ""))
  expect_error(entropy(integr::golf, classAtt = 123))
  expect_error(entropy(integr::golf, classAtt = integr::golf))

  #test infoGain()
  expect_error(infoGain())
  expect_error(infoGain(integr::golf, inAtt = c("Outlook")))
  expect_error(infoGain(integr::golf, inAtt = c("Outlook", "Windy"), classAtt = ""))
  expect_error(infoGain(integr::golf, classAtt = ""))
  expect_error(infoGain(integr::golf, inAtt = "", classAtt = 123))
  expect_error(infoGain(integr::golf, inAtt = c("Outlook", "Windy"), classAtt = 123))
  expect_error(infoGain(integr::golf, inAtt = c("Outlook", "Windy"), classAtt = integr::golf))

  #test isDiscreteDataFrame()
  expect_error(isDiscreteDataFrame())
  expect_error(isDiscreteDataFrame("String"))
  expect_error(isDiscreteDataFrame(123))
  expect_error(isDiscreteDataFrame(c("A", 1)))

})

#Test helping functions for correctness of output ----

#Test entropy()
test_that("entropy() returns correct value", {
  expect_gte(entropy(integr::golf, classAtt = "Play"), 0)
  expect_lte(entropy(integr::golf, classAtt = "Play"), 1)
  expect_type(entropy(integr::golf, classAtt = "Play"), "double")
})

#Test infoGain()
test_that("infoGain() returns correct value", {
  expect_gte(infoGain(integr::golf, inAtt = "Windy", classAtt = "Play"), 0)
  expect_lte(infoGain(integr::golf, inAtt = "Windy", classAtt = "Play"), 1)
  expect_type(infoGain(integr::golf, inAtt = "Windy", classAtt = "Play"), "double")
})

#Test isDiscreteDataFrame()
test_that("isDiscreteDataFrame() returns correct value", {
  expect_is(isDiscreteDataFrame(integr::golf), "logical")
})
