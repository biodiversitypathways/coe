library(tidyverse)
library(sf)
library(terra)

yeggrid <- read_csv("edmonton_grid_coordinates.csv") |> st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
wilded <- read_csv("wilded.csv") |> st_as_sf(coords = c("longitude","latitude"), crs = 4326)
hydro <- read_csv("hydro.csv") |> st_as_sf(wkt = "geometry_multipolygon", crs = 4326) |> st_make_valid()
parks <- read_csv("parks.csv") |> st_as_sf(wkt = "Multipolygon", crs = 4326) |> st_make_valid()

intersections <- st_intersects(yeggrid, hydro)
no_water_points <- yeggrid[!apply(intersections, 1, any), ]
intersections2 <- st_intersects(no_water_points, parks)
no_water_parks_points <- no_water_points[apply(intersections2, 1, any), ]

print(no_water_parks_points)

write.csv(no_water_parks_points,"yeg.csv")

intersections <- st_intersects(yeggrid, hydro)
no_water_points <- yeggrid[!apply(intersections, 1, any), ]

gridfill <- no_water_points |> filter(!Location %in% no_water_parks_points$Location)

sample_frac(0.1)

wilded_buffered <- st_buffer(wilded, dist = 500)

intersections <- st_intersects(gridfill, wilded_buffered)

filtered_points <- gridfill[apply(intersections, 1, any), ]

print(filtered_points)

write.csv(filtered_points,"yegfill.csv")

bikes <- read_csv("bikes.csv")

