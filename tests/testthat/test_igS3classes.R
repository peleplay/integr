context("Test interaction graph S3 classes")

#Test validity of input parameters for Nodes, Edges, InteractionGraph ----
test_that("Interaction Graph (ig) S3 class throw error if improper input", {

  #Nodes
  expect_error(igNode())
  expect_error(igNode(1, 22.2))
  expect_error(igNode(1, "22.2"))
  expect_error(igNode("A", "B"))
  expect_error(igNode(TRUE, FALSE))

  #Edges
  expect_error(igEdge())
  expect_error(igEdge(1, 2, 222.222))
  expect_error(igEdge("1", 2, 222.222))
  expect_error(igEdge(1, "2", 222.222))
  expect_error(igEdge(1, 2, "222.222"))
  expect_error(igEdge("1", "2", "222.222"))
  expect_error(igEdge(1, 2, TRUE))

  #Interaction Graph
  expect_error(ig())
  expect_error(ig(igNode("A", 48.2)))
  expect_error(ig(igEdge("A", "B", 10.5)))

})

#Test that the generated classes are of proper type (i.e. S3 + node/edge/ig)
test_that("ig, igNode and igEdge are of proper S3 class type", {

  #Nodes
  expect_s3_class(igNode("A", 48.2), "igNode")

  #Edges
  expect_s3_class(igEdge("A", "B", 10.5), "igEdge")

  #Interaction Graph
  expect_s3_class(ig(igNode("A", 48.2), igEdge("A", "B", 10.5)), "ig")

})
