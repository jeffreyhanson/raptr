# load case study data
cs_pus <- sf::read_sf(
  system.file("extdata", "cs_pus.gpkg", package = "raptr")
)

cs_spp <- terra::rast(
  system.file("extdata", "cs_spp.tif", package = "raptr")
)

cs_space <- terra::rast(
  system.file("extdata", "cs_space.tif", package = "raptr")
)
