library(tidyverse)
library(sf)

# https://github.com/rstudio/cheatsheets/blob/main/sf.pdf

# querying the data via WFS (from LINZ)
url <-"https://data.linz.govt.nz/services;key=9039cb14ccf4498594ac376cac84e093/wfs/layer-113764/?service=WFS&request=GetCapabilities"

nz_suburbs <- st_read(url)

# Getting the Auckland suburbs
auckland_suburbs <- nz_suburbs |>
  filter(grepl("Auckland", territorial_authority))

# Subsetting because I don't want sea boundaries in the map 
auckland_suburbs <- auckland_suburbs |>
  filter(type %in% c("Suburb", "Town", "Locality") |
           name == "Waiheke Island")

# Removing macrons from suburb names
auckland_suburbs <- auckland_suburbs %>%
  mutate(name = chartr("ĀĒĪŌŪāēīōū", "AEIOUaeiou", name))

# One suburb per cell!!!
# Used ChatGPT for this because I could not figure out how to split each suburb and make it into ind. row
auckland_suburbs <- auckland_suburbs |>
  mutate(additional_name = str_split(additional_name, ",\\s*")) |>
  unnest(additional_name)


### The data of the shape of the suburb to be "drawn" on to the map ###
# https://r-spatial.org/book/03-Geometries.html
# https://r-spatial.r-universe.dev/articles/sf/sf1.html

suburbs_geometric <- st_cast(auckland_suburbs, "GEOMETRYCOLLECTION") # to get the polygon
auckland_suburb_polygons <- st_collection_extract(suburbs_geometric, "POLYGON")

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
# st_crs(auckland_suburb_polygons) currently using coordinate reference system of NZGD2000 
# I will change it to WGS84 (EPSG: 4326), which is crs used by google earth, etc.

auckland_suburb_polygons <- st_transform(auckland_suburb_polygons, crs = 4326)

auckland_suburb_polygons <- auckland_suburb_polygons |>
  rename(Suburb = name)

saveRDS(auckland_suburb_polygons, "auckland_suburb_polygons.rds")