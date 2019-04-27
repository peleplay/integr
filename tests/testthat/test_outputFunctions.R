context("Test output functions")

#Test output functions for missing/incorrect input parameters ----
test_that("Output functions throw error if a parameter is missing/incorrect", {

  #Test plotIntGraph()
  expect_error(plotIntGraph())
  expect_error(plotIntGraph("ABC"))
  expect_error(plotIntGraph(123))
  expect_error(plotIntGraph(c("ABC", 123)))

  #Test igToGrViz
  expect_error(igToGrViz())
  expect_error(igToGrViz("ABC"))
  expect_error(igToGrViz(123))
  expect_error(igToGrViz(interactionGraph(integr::golf, "Play"), intNo = 10, path = 123))
  expect_error(igToGrViz(interactionGraph(integr::golf, "Play"), intNo = 10, fName = 123))
  expect_error(igToGrViz(interactionGraph(integr::golf, "Play"), intNo = 10, fName = "A 1"))

  #Test igToSVG
  expect_error(igToGrSVG())
  expect_error(igToGrSVG("ABC"))
  expect_error(igToGrSVG(123))
  expect_error(igToGrSVG(interactionGraph(integr::golf, "Play"), path = 123))
  expect_error(igToGrSVG(interactionGraph(integr::golf, "Play"), fName = 123))
  expect_error(igToGrSVG(interactionGraph(integr::golf, "Play"), fName = "A 1"))
  expect_error(igToGrSVG(interactionGraph(integr::golf, "Play"), h = "ABC"))

  #Test igToPNG
  expect_error(igToGrPNG())
  expect_error(igToGrPNG("ABC"))
  expect_error(igToGrPNG(123))
  expect_error(igToGrPNG(interactionGraph(integr::golf, "Play"), path = 123))
  expect_error(igToGrPNG(interactionGraph(integr::golf, "Play"), fName = 123))
  expect_error(igToGrPNG(interactionGraph(integr::golf, "Play"), fName = "A 1"))
  expect_error(igToGrPNG(interactionGraph(integr::golf, "Play"), h = "ABC"))

  #Test igToPDF
  expect_error(igToGrPDF())
  expect_error(igToGrPDF("ABC"))
  expect_error(igToGrPDF(123))
  expect_error(igToGrPDF(interactionGraph(integr::golf, "Play"), path = 123))
  expect_error(igToGrPDF(interactionGraph(integr::golf, "Play"), fName = 123))
  expect_error(igToGrPDF(interactionGraph(integr::golf, "Play"), fName = "A 1"))
  expect_error(igToGrPDF(interactionGraph(integr::golf, "Play"), h = "ABC"))

  #Test igToPS
  expect_error(igToGrPS())
  expect_error(igToGrPS("ABC"))
  expect_error(igToGrPS(123))
  expect_error(igToGrPS(interactionGraph(integr::golf, "Play"), path = 123))
  expect_error(igToGrPS(interactionGraph(integr::golf, "Play"), fName = 123))
  expect_error(igToGrPS(interactionGraph(integr::golf, "Play"), fName = "A 1"))
  expect_error(igToGrPS(interactionGraph(integr::golf, "Play"), h = "ABC"))

})
