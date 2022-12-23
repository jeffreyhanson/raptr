## ---- include=FALSE-----------------------------------------------------------
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
knitr::opts_chunk$set(fig.align = "center", eval = !is_check,
                      root.dir = normalizePath("../.."))


## ---- include=FALSE-----------------------------------------------------------
devtools::load_all()


## ---- eval=FALSE--------------------------------------------------------------
## # load raptr R package
## library(raptr)
## 
## # show package overview
## ?raptr


## ---- message=FALSE, result='hide'--------------------------------------------
# load packages for tutorial
library(parallel)
library(plyr)
library(dplyr)
library(ggplot2)

# set seed for reproducibility
set.seed(500)

# set number of threads for computation
threads <- as.integer(max(1, detectCores() - 2))


## -----------------------------------------------------------------------------
is.GurobiInstalled(verbose = TRUE)


## -----------------------------------------------------------------------------
# make planning units
sim_pus <- sim.pus(100L)

# simulate species distributions
sim_spp <- lapply(
  c("uniform", "normal", "bimodal"), sim.species, n = 1, x = sim_pus, res = 1
)


## ---- fig.width=7, fig.height=2.5, fig.cap="_Distribution of three simulated species. Each square represents a planning unit. The color of each square denotes the probability that individuals from each species occupy it._"----
# plot species
plot(
  terra::rast(sim_spp),
  main = c("Uniform species", "Normal species", "Bimodal species")
)
terra::lines(as(sim_pus, "SpatVector"))


## -----------------------------------------------------------------------------
# generate coordinates for pus/demand points
pu_coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sim_pus)))

# create demand point objects
sim_dps <- lapply(sim_spp, function(x) {
  DemandPoints(
    pu_coords,
    weights = terra::extract(x, pu_coords)[[1]]
  )
})


## -----------------------------------------------------------------------------
## create RapUnreliableOpts object
# this stores parameters for the unreliable formulation problem (ie. BLM)
sim_ro <- RapUnreliableOpts()

## create RapData object
# create data.frame with species info
species <- data.frame(name = c("uniform", "normal", "bimodal"))

## create data.frame with species and space targets
# amount targets at 20% (denoted with target=0)
# space targets at 20% (denoted with target=1)
targets <- expand.grid(species = 1:3, target = 0:1, proportion = 0.2)

# calculate probability of each species in each pu
pu_probabilities <- calcSpeciesAverageInPus(sim_pus, terra::rast(sim_spp))

## create AttributeSpace object
# this stores the coordinates of the planning units in an attribute space
# and the coordinates and weights of demand points in the space
pu_points <- PlanningUnitPoints(
  coords = pu_coords,
  ids = seq_len(nrow(sim_pus))
)

attr_spaces <- AttributeSpaces(
  list(
    AttributeSpace(
      planning.unit.points = pu_points,
      demand.points = sim_dps[[1]],
      species = 1L
    ),
    AttributeSpace(
      planning.unit.points = pu_points,
      demand.points = sim_dps[[2]],
      species = 2L
    ),
    AttributeSpace(
      planning.unit.points = pu_points,
      demand.points = sim_dps[[3]],
      species = 3L
    )
  ),
  name = "geographic"
)

# generate boundary data information
boundary <- calcBoundaryData(sim_pus)

## create RapData object
# this stores all the input data for the prioritization
sim_rd <- RapData(
  sim_pus, species, targets, pu_probabilities,
  list(attr_spaces), boundary, convert2PolySet(sim_pus)
)

## create RapUnsolved object
# this stores all the input data and parameters needed to generate
# prioritizations
sim_ru <- RapUnsolved(sim_ro, sim_rd)


## -----------------------------------------------------------------------------
# create new object with just the uniform species
sim_ru_s1 <- spp.subset(sim_ru, "uniform")

# update amount targets to 20% and space targets to 0%
sim_ru_s1 <- update(
  sim_ru_s1, amount.target = 0.2, space.target = NA, solve = FALSE
)

# solve problem to identify prioritization
sim_rs_s1_amount <- solve(sim_ru_s1, Threads = threads)


## -----------------------------------------------------------------------------
## show summary
# note the format for this is similar to that used by Marxan
# see ?raptr::summary for details on this table
summary(sim_rs_s1_amount)

# show amount held
amount.held(sim_rs_s1_amount)

# show space held
space.held(sim_rs_s1_amount)


## ---- fig.height=3.5, fig.width=4.5, fig.align='center', fig.cap="_A prioritization for the uniformly distributed species generated using amount-based targets (20\\%). Sqaures represent planning units. Planning units with a green border are selected for prioritization, and their colour denotes the probability they are inhabited by the species._"----
# plot the prioritization and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main = "Uniform species")


## -----------------------------------------------------------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, "normal")


## -----------------------------------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(
  sim_ru_s2, amount.target = 0.2, space.target = NA,
  solve = TRUE, Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_amount)

# show amount held
amount.held(sim_rs_s2_amount)

# show space held
space.held(sim_rs_s2_amount)


## ---- fig.height=3.5, fig.width=4.5,fig.align="center", fig.cap="_A prioritization for the normally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions._"----
# plot the prioritization and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main = "Normal species")


## -----------------------------------------------------------------------------
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, "bimodal")


## -----------------------------------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(
  sim_ru_s3, amount.target = 0.2, space.target = NA, Threads = threads
)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A prioritization for the bimodally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions._"----
# plot the prioritization and the bimodal species' distribution
spp.plot(sim_rs_s3_amount, 1, main = "Bimodal species")


## -----------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_amount)

# show amount held
amount.held(sim_rs_s3_amount)

# show space held
space.held(sim_rs_s3_amount)


## -----------------------------------------------------------------------------
# make new prioritization
sim_rs_s1_space <- update(
  sim_rs_s1_amount, amount.target = 0.2, space.target = 0.85, Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_rs_s1_space)

# show amount held
amount.held(sim_rs_s1_space)

# show space held
space.held(sim_rs_s1_space)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A prioritization for the uniformly distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----
# plot the prioritization and the uniform species' distribution
spp.plot(sim_rs_s1_space, "uniform", main = "Uniform species")


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two prioritizations for the uniformly distributed species. Prioritisation $X$ was generated using just amount-based targets (20\\%), and prioritization $Y$ was generated using an additional space-based target (85\\%)._"----
# plot the difference between old and new prioritizations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1,
     main = "Difference between solutions")


## -----------------------------------------------------------------------------
# make new prioritization
sim_rs_s2_space <- update(
  sim_rs_s2_amount, amount.target = 0.2, space.target = 0.85, Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_space)

# show amount held
amount.held(sim_rs_s2_space)

# show space held
space.held(sim_rs_s2_space)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A prioritization for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----

# plot the prioritization and the normal species' distribution
spp.plot(sim_rs_s2_space, "normal", main = "Normal species")


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two prioritizations for the normally distributed species. See Figure 7 caption for conventions._"----
# plot the difference between old and new prioritizations
plot(
  sim_rs_s2_amount, sim_rs_s2_space, 1, 1,
  main = "Difference between solutions"
)


## -----------------------------------------------------------------------------
# make new prioritization
sim_rs_s3_space <- update(
  sim_rs_s3_amount, amount.target = 0.2,space.target = 0.85, Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_space)

# show amount held
amount.held(sim_rs_s3_space)

# show space held
space.held(sim_rs_s3_space)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A prioritization for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----
# plot the prioritization and the bimodal species' distribution
spp.plot(sim_rs_s3_space, 'bimodal', main='Bimodal species')


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two prioritizations for the bimodally distributed species. See Figure 7 caption for conventions._"----
# plot the difference between old and new prioritizations
plot(
  sim_rs_s3_amount, sim_rs_s3_space, 1, 1,
  main = "Difference between solutions"
)


## -----------------------------------------------------------------------------
# make prioritizations
sim_mrs_amount <- update(
  sim_ru, amount.target = c(0.2, 0.2, 0.2),
  space.target = c(0, 0, 0), Threads = threads
)

sim_mrs_space <- update(
  sim_ru, amount.target = c(0.2, 0.2, 0.2),
  space.target = c(0.85, 0.85, 0.85),
  Threads = threads
)


## -----------------------------------------------------------------------------
# show summaries
summary(sim_mrs_amount)
summary(sim_mrs_space)

# show amount held for each prioritization
amount.held(sim_mrs_amount)
amount.held(sim_mrs_space)

# show space held for each prioritization
space.held(sim_mrs_amount)
space.held(sim_mrs_space)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A multi-species prioritization for the uniformly, normally, and bimodally distributed species generated using just amount-based targets (20\\%). Squares represent planning units. Dark green planning units are selected for preservation._"----
# plot multi-species prioritization with amount-based targets
plot(sim_mrs_amount, 1, main = "Amount-based targets")


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A multi-species prioritization for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 12 caption for conventions._"----
# plot multi-species prioritization with amount- and space-based targets
plot(sim_mrs_space, 1, main = "Amount and space-based targets")


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two multi-species prioritizations. See Figure 7 caption for conventions.)"----
# difference between the two prioritizations
plot(sim_mrs_amount, sim_mrs_space, 1, 1, main = "Difference between solutions")


## ---- eval = TRUE, include = FALSE--------------------------------------------
if (exists("sim_mrs_amount")) {
  n_sim_mrs_amount <- sum(selections(sim_mrs_amount) == 1)
} else {
  n_sim_mrs_amount <- NA
}
if (exists("sim_mrs_space")) {
  n_sim_mrs_space <- sum(selections(sim_mrs_space) == 1)
} else {
  n_sim_mrs_space <- NA
}


## -----------------------------------------------------------------------------
# make new prioritization with probability threshold of 0.1 for each species
sim_mrs_space2 <- solve(
  prob.subset(sim_mrs_space, species = 1:3, threshold = c(0.1, 0.1, 0.1)),
  Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_mrs_space2)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A multi-species prioritization for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This prioritization was generated to be robust against low occupancy probabilities, by preventing planning units with low probabilities from being used to represent demand points. See Figure 12 caption for conventions._"----
# plot prioritization
plot(sim_mrs_space2, 1)


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two multi-species prioritizations. See Figure 7 caption for conventions._"----
# difference between prioritizations that use and do not use thresholds
plot(sim_mrs_space2, sim_mrs_space, 1, 1, main = "Difference between solutions")


## -----------------------------------------------------------------------------
# make new prioritization using reliable formulation
sim_mrs_space3 <- try(update(
  sim_mrs_space, formulation = "reliable",
  max.r.level = 1L, Threads = threads
))


## -----------------------------------------------------------------------------
# make new prioritization using reliable formulation and reduced targets
sim_mrs_space3 <- update(
  sim_mrs_space, formulation = "reliable",
  max.r.level = 1L, space.target = -1000,
  Threads = threads
)


## -----------------------------------------------------------------------------
# show summary
summary(sim_mrs_space3)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A multi-species prioritization for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This prioritization was generated to be robust against low occupancy probabilities, by explicitly using the probability of occupancy data when deriving a solution. See Figure 12 caption for conventions._"----
# plot prioritization
plot(sim_mrs_space3, 1)


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two multi-species prioritizations. See Figure 7 caption for conventions._"----
# difference between prioritizations based on unreliable and reliable formulation
plot(sim_mrs_space3, sim_mrs_space, 1, 1, main = "Difference between solutions")


## ---- eval = TRUE, include = FALSE--------------------------------------------
if (exists("sim_mrs_space3")) {
  n_sim_mrs_space3 <- sum(selections(sim_mrs_amount) == 1)
} else {
  n_sim_mrs_space3 <- NA
}


## -----------------------------------------------------------------------------
# update prioritization
sim_mrs_amount_blm <- update(sim_mrs_amount, BLM = 100, Threads = threads)


## -----------------------------------------------------------------------------
# show summary of prioritization
summary(sim_mrs_amount_blm)

# show amount held for each prioritization
amount.held(sim_mrs_amount_blm)

# show space held for each prioritization
space.held(sim_mrs_amount_blm)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_A multi-species prioritization for the uniformly, normally, and bimodally distributed species generated using only amount-based targets (20\\%). Additionally, this prioritization was specified to have high connectivity, by using a high $BLM$ parameter. See Figure 12 caption for conventions._"----

# plot prioritization
plot(sim_mrs_amount_blm, 1)


## ---- fig.height=3.5, fig.width=6.0, fig.align="center", fig.cap="_Difference between two multi-species prioritizations. See Figure 7 caption for conventions._"----
# difference between the two prioritizations
plot(
  sim_mrs_amount_blm, sim_mrs_amount, 1, 1,
  main = "Difference between solutions"
)


## ---- fig.height=4, fig.width=5, fig.align="center", fig.cap="_Distribution map for four Australian bird species. Pixel colours denote probability of occupancy._"----
# import data
cs_spp <- terra::rast(
  system.file("extdata", "cs_spp.tif", package = "raptr")
)

# remove areas with low occupancy (prob < 0.6)
cs_spp <- terra::clamp(cs_spp, lower = 0.6, values = FALSE)

# plot species' distributions
plot(
  cs_spp,
  main = c(
    "Blue-winged kookaburra", "Brown-backed honeyeater",
    "Brown falcon", "Pale-headed rosella"
  )
)


## ---- fig.height=3.5, fig.width=4.5, fig.align='center', fig.cap="_Planning units for the case-study examples. Polygons denote planning units. Yellow units have more than 50\\% of their area already in a reserve.)"----
# import data
cs_pus <- sf::read_sf(
  system.file("extdata", "cs_pus.gpkg", package = "raptr")
)

## plot planning units
# convert sf to PolySet for quick plotting
cs_pus2 <- convert2PolySet(cs_pus)

# create vector of colours for planning units
# + light green: units not already inside reserve
# + yellow: units already inside reserve
cs_pus_cols <- rep("#c7e9c0", nrow(cs_pus))
cs_pus_cols[which(cs_pus$status == 2)] <- "yellow"

# set plotting window
par(mar = c(0.1, 0.1, 4.1, 0.1))

# plot polygons
PBSmapping::plotPolys(
  cs_pus2, col = cs_pus_cols, border = "gray30",
  xlab = "", ylab = "", axes = FALSE,
  main = "Planning units", cex = 1.8
)

# reset plotting window
par(mar = c(5.1, 4.1, 4.1, 2.1))


## ---- fig.height=2.5, fig.width=7, fig.align='center', fig.cap="_Broad-scale environmental variation across Australia. The variable DC1 describes the transition from wet and cool to dry and hot conditions. The variable DC2 describes the transition from wet and hot to dry and cool conditions._"----
# import data
cs_space <- terra::rast(
  system.file("extdata", "cs_space.tif", package = "raptr")
)

# plot variables
plot(cs_space, main = c("DC1", "DC2"), legend = FALSE, axes = FALSE)


## ---- message=FALSE-----------------------------------------------------------
# make amount-based prioritization
# and ignore existing protected areas by discarding values in the
# status (third) column of the attribute table
cs_rs_amount <- rap(
  cs_pus[, -2], cs_spp, cs_space, amount.target = 0.2,
  space.target = NA, n.demand.points = 50L,
  include.geographic.space = TRUE, formulation = "unreliable",
  kernel.method = "ks", quantile = 0.7, solve = FALSE
)


## -----------------------------------------------------------------------------
# generate prioritization
cs_rs_amount <- solve(cs_rs_amount, MIPGap = 0.1, Threads = threads)


## ---- fig.height=3.5, fig.width=4.5, fig.align="center", fig.cap="_Multi-species prioritization generated for four bird species using amount-based targets (20\\%). See Figure 12 captions for conventions._"----
# show summary
summary(cs_rs_amount)

# plot prioritization
plot(cs_rs_amount, 1)


## ---- fig.height=4.5, fig.width=10, fig.align='center', fig.cap="_Distribution of amount-based prioritization in the geographic attribute space. Points denote combinations of environmental conditions. Green and grey points represent planning unit selected for and not selected for prioritization (respectively). Blue points denote demand points, and their size indicates their weighting._"----
# plot prioritization in geographic attribute space
p1 <- space.plot(cs_rs_amount, 1, 2, main = "Blue-winged\nkookaburra")
p2 <- space.plot(cs_rs_amount, 2, 2, main = "Brown-backed\nhoneyeater")
p3 <- space.plot(cs_rs_amount, 3, 2, main = "Brown falcon")
p4 <- space.plot(cs_rs_amount, 4, 2, main = "Pale-headed\nrosella")
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)


## ---- fig.height=4.5, fig.width=10,  fig.align='center', fig.cap="_Distribution of amount-based prioritization in the environmental attribute space. See Figure 28 caption for conventions._"----
# plot prioritization in environmental attribute space
p1 <- space.plot(cs_rs_amount, 1, 1, main = "Blue-winged\nkookaburra")
p2 <- space.plot(cs_rs_amount, 2, 1, main = "Brown-backed\nhoneyeater")
p3 <- space.plot(cs_rs_amount, 3, 1, main = "Brown falcon")
p4 <- space.plot(cs_rs_amount, 4, 1, main = "Pale-headed\nrosella")
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)


## -----------------------------------------------------------------------------
# make amount- and space-based prioritization
cs_rs_space <- update(cs_rs_amount, space.target = 0.75, Threads = threads)


## ---- fig.height=3.5, fig.width=4.5, fig.align='center', fig.caption='_Prioritisation generated using amount, environmental, and geographic targets._'----
# show summary
summary(cs_rs_space)

# plot prioritization
plot(cs_rs_space, 1)


## ---- fig.height=4.5, fig.width=10, fig.cap="_Distribution of the amount- and space-based prioritization in the geographic attribute space. See Figure 28 caption for conventions._"----
# plot prioritization in geographic attribute space
p1 <- space.plot(cs_rs_space, 1, 2, main = "Blue-winged\nkookaburra")
p2 <- space.plot(cs_rs_space, 2, 2, main = "Brown-backed\nhoneyeater")
p3 <- space.plot(cs_rs_space, 3, 2, main = "Brown falcon")
p4 <- space.plot(cs_rs_space, 4, 2, main = "Pale-headed\nrosella")
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)


## ---- fig.height=4.5, fig.width=10, fig.align='center', fig.cap="_Distribution of the amount- and space-based prioritization in the environmental attribute space. See Figure 28 caption for conventions._"----
# plot prioritization in environmental attribute space
p1 <- space.plot(cs_rs_space, 1, 1, main = "Blue-winged\nkookaburra")
p2 <- space.plot(cs_rs_space, 2, 1, main = "Brown-backed\nhoneyeater")
p3 <- space.plot(cs_rs_space, 3, 1, main = "Brown falcon")
p4 <- space.plot(cs_rs_space, 4, 1, main = "Pale-headed\nrosella")
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)


## -----------------------------------------------------------------------------
# generate vector with Australia's selections
aus_selections <- which(cs_pus$status > 0)

# create new object with Australia's network
cs_rs_aus <- update(cs_rs_amount, b = aus_selections)


## ---- fig.height=3.5, fig.width=8, fig.align='center', fig.cap="_Prioritizations were generated using amount-based targets (20\\%), and with additional space-based targets (85\\%). These are compared to the Queensland reserve network. Data represent means and standard errors for the four species in each prioritization._"----
# define standard error function
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# create a table to store the values for the 3 prioritizations
cs_results <- data.frame(
  name = factor(
    rep(rep(c(
      "Amount-based\nprioritization",
      "Amount & space-based\nprioritization",
      "Queensland reserve\nnetwork"), each = 4), 3),
    levels = c(
      "Amount-based\nprioritization",
      "Amount & space-based\nprioritization",
      "Queensland reserve\nnetwork"
    )
  ),
  variable = rep(
    c("Amount", "Geographic space", "Environmental space"),
    each = 12
  ),
  species = colnames(amount.held(cs_rs_amount)),
  value = c(
    amount.held(cs_rs_amount)[1, ],
    amount.held(cs_rs_space)[1, ],
    amount.held(cs_rs_aus)[1, ],
    space.held(cs_rs_amount, space = 2)[1, ],
    space.held(cs_rs_space, space = 2)[1, ],
    space.held(cs_rs_aus, space = 2)[1, ],
    space.held(cs_rs_amount, space = 1)[1, ],
    space.held(cs_rs_space, space = 1)[1, ],
    space.held(cs_rs_aus, space = 1)[1, ])
  ) %>%
  group_by(name, variable) %>%
  summarise(mean = mean(value), se = se(value))

# plot the performance metrics
ggplot(aes(x = variable, y = mean, fill = name), data = cs_results) +
  geom_bar(position = position_dodge(0.9), stat = "identity") +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(0.9),
    width = 0.2
  ) +
  xlab("Property of species") +
  ylab("Proportion held in\nselected planning units (%)") +
  scale_fill_discrete(name = "") +
  theme_classic() +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    axis.line.x = element_line(), axis.line.y = element_line()
  )


## ---- eval = TRUE, include = FALSE--------------------------------------------
if (exists("cs_rs_amount")) {
  n_cs_rs_amount <- sum(selections(cs_rs_amount) == 1)
} else {
  n_cs_rs_amount <- NA
}
if (exists("cs_rs_space")) {
  n_cs_rs_space <- sum(selections(cs_rs_space) == 1)
} else {
  n_cs_rs_space <- NA
}

