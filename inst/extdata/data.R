# Initialization
## load packages
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
cs_pus@proj4string <- sp::CRS("EPSG:3577")
crs(cs_space) <- sp::CRS("EPSG:3577")
crs(cs_spp) <- sp::CRS("EPSG:3577")

# Exports
save(cs_pus, file = "data/cs_pus.rda", compress = "xz")
save(cs_space, file = "data/cs_space.rda", compress = "xz")
save(cs_spp, file = "data/cs_spp.rda", compress = "xz")
save(sim_rs, file = "data/sim_rs.rda", compress = "xz")
save(sim_ru, file = "data/sim_ru.rda", compress = "xz")
