context("09-AttributeSpaces")

test_that("AttributeSpaces generator function", {
  # create data
  pu_coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(cs_pus)))
  sp1 <- AttributeSpace(
    planning.unit.points = PlanningUnitPoints(
      pu_coords,
      seq_len(nrow(cs_pus))
    ),
    demand.points = make.DemandPoints(
      randomPoints(cs_spp[[1]], n = 100, prob = TRUE)
    ),
    species = 1L
  )
  sp2 <- AttributeSpace(
    planning.unit.points = PlanningUnitPoints(
      pu_coords,
      seq_len(nrow(cs_pus))
    ),
    demand.points = make.DemandPoints(
      randomPoints(cs_spp[[2]], n = 100, prob = TRUE)
    ),
    species = 2L
  )
  # checks are internal
  sps1 <- AttributeSpaces(list(sp1, sp2), "test")
  suppressMessages(print(sps1))
  expect_true(methods::validObject(sps1, test = FALSE))
})
