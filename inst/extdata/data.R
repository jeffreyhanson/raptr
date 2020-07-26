# Initialization
## load packages
library(sf)
library(sp)
library(raster)

## load data
load("inst/extdata/cs_pus.rda")
load("inst/extdata/cs_space.rda")
load("inst/extdata/cs_spp.rda")
load("inst/extdata/sim_rs.rda")
load("inst/extdata/sim_ru.rda")

# Main processing
## update case study data
cs_pus@proj4string <- as(st_crs(3577), "CRS")
crs(cs_space) <- as(st_crs(3577), "CRS")
crs(cs_spp) <- as(st_crs(3577), "CRS")

# Exports
save(cs_pus, file = "inst/extdata/cs_pus.rda", compress = "xz")
save(cs_space, file = "inst/extdata/cs_space.rda", compress = "xz")
save(cs_spp, file = "inst/extdata/cs_spp.rda", compress = "xz")
save(sim_rs, file = "inst/extdata/sim_rs.rda", compress = "xz")
save(sim_ru, file = "inst/extdata/sim_ru.rda", compress = "xz")
