# Initialization
## load packages
devtools::load_all()

## set seed
set.seed(500)

# Main processing
## simulate data
sim_pu <- sim.pus(100, xmn = -5, xmx = 5, ymn = -5, ymx = 5)
sim_spp <- terra::rast(list(
  sim.species(sim_pu, res = 1, n = 1, model = "uniform"),
  sim.species(sim_pu, res = 1, n = 1, model = "normal"),
  sim.species(sim_pu, res = 1, n = 1, model = "bimodal")
))
names(sim_spp) <- c("uniform", "normal", "bimodal")

## create attribute spaces
sim_spaces <- AttributeSpaces(
  name = "geographic",
  lapply(seq_len(terra::nlyr(sim_spp)), function(i) {
    # extract data
    d <- terra::as.data.frame(
      sim_spp[[i]], xy = TRUE, cells = TRUE, na.rm = TRUE
    )
    # construct attribute space
    AttributeSpace(
      PlanningUnitPoints(
        coords = as.matrix(d[, c("x", "y")]),
        ids = as.integer(d$cell)
      ),
      DemandPoints(
        coords = as.matrix(d[, c("x", "y")]),
        weights = d[[4]]
      ),
      species = i
    )
  })
)

## create spatial data
sim_polygons <- convert2PolySet(sim_pu)
sim_spp_probs <- calcSpeciesAverageInPus(sim_pu, sim_spp)
sim_bd <- calcBoundaryData(sim_pu)

## create rap data
sim_rd <- RapData(
  pu = sim_pu,
  species = data.frame(name = names(sim_spp)),
  target = data.frame(
    species = rep(c(1L, 2L, 3L), 2),
    target = rep(c(0L, 1L), each = 3),
    proportion = rep(c(0.2, 0.85), each = 3)
  ),
  pu.species.probabilities = sim_spp_probs,
  attribute.spaces = list(sim_spaces),
  polygons = sim_polygons,
  boundary = sim_bd
)

## create unsolved problem
sim_ru <- RapUnsolved(
  opts = RapUnreliableOpts(),
  data = sim_rd
)

## solve problem
sim_rs <- solve(sim_ru, GurobiOpts(NumberSolutions = 3L))

# Exports
usethis::use_data(sim_ru, overwrite = TRUE)
usethis::use_data(sim_rs, overwrite = TRUE)
