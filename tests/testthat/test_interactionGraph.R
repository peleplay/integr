context("Test intreaction graphs function")

#Test interactionGraph() for validity of input parameters ----
test_that("interactionGraph() throws an error to invalid input parameters", {
  expect_error(interactionGraph())
  expect_error(interactionGraph(integr::golf))
  expect_error(interactionGraph(integr::golf, classAtt = 1))
  expect_error(interactionGraph(integr::golf, classAtt = TRUE))
  expect_error(interactionGraph(integr::golf, classAtt = Play))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", intNo = TRUE))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", intNo = "A"))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", intNo = A))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", speedUp = "A"))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", speedUp = 1))
  expect_error(interactionGraph(integr::golf, classAtt = "Play", speedUp = A))

  #Test if warnings are thrown for the intNo parameter outside of range
  expect_warning(interactionGraph(integr::golf, classAtt = "Play", intNo = -1))
  expect_warning(interactionGraph(integr::golf, classAtt = "Play", intNo = 0))
  expect_warning(interactionGraph(integr::golf, classAtt = "Play", intNo = 20))
  expect_warning(interactionGraph(integr::golf, classAtt = "Play", intNo = 25))
  expect_warning(interactionGraph(integr::golf, classAtt = "Play"))

})

#Test that interactionGraph() returns a string object with graphviz text  ----
test_that("interactionGraph() returns a string object with graphviz text", {
  expect_type(interactionGraph(integr::golf, classAtt = "Play", intNo = 10), "character")
  expect_match(interactionGraph(integr::golf, classAtt = "Play", intNo = 10), "InteractionGraph")
})
