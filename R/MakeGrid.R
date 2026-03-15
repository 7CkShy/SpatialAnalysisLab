library(sf)
library(dplyr)
library(terra)
library(mapview)
library(tmap)

species_point = read.csv('./data/species_points.csv') |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview_test_points = mapview(species_point, cex = 3, alpha = 0.7, popup = NULL)
mapview_test_points

# 基于数据点创建网格
area_fishnet_grid = st_make_grid(
  species_point,
  cellsize = 1,
  what = "polygons",
  square = TRUE
)

# 把网格转换为 sf 对象，并添加网格 ID
area_fishnet_grid |>
  st_as_sf() |>
  mutate(
    grid_id = c(1:length(area_fishnet_grid))
  ) -> fishnet_grid_sf

# 利用 st_intersects 计算每个网格内的点数量
fishnet_grid_sf$n_point = lengths(st_intersects(fishnet_grid_sf, species_point))

# 过滤掉没有点的网格
fish_count = fishnet_grid_sf |>
  filter(n_point != 0)

# view the result in the intaction map
tmap_mode("view")

map_fishnet = tm_shape(fish_count) +
  tm_fill(
    col = "n_point",
    palette = "Reds",
    style = "cont",
    title = "Number of species",
    id = "grid_id",
    alpha = .5,
    popup.vars = c(
      "Number of collisions:" = "n_point"
    ),
    popup.format = list(
      n_point = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = .7)

map_fishnet

area_honeycomb_grid = st_make_grid(
  species_point,
  c(1, 1),
  what = "polygons",
  square = FALSE
)

# To sf and add grid ID
honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
honeycomb_grid_sf$n_colli = lengths(st_intersects(
  honeycomb_grid_sf,
  species_point
))

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count = filter(honeycomb_grid_sf, n_colli > 0)

tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb

# making the full grid again

full_fishnet_grid = species_point |>
  st_make_grid(
    c(1, 1),
    what = "polygons",
    square = FALSE
  )

mapview(full_fishnet_grid)

full_fishnet_grid_sf = full_fishnet_grid |>
  st_as_sf() |>
  mutate(
    grid_id = c(1:length(full_fishnet_grid))
  )

full_fishnet_grid_sf$n_species = lengths(
  st_intersects(full_fishnet_grid_sf, species_point)
)

mapview(full_fishnet_grid_sf, zcol = "n_species")

species_point |>
  slice(1:10) |>
  st_make_grid(c(1, 1), what = "polygons", square = TRUE) -> test_grid

test_grid |>
  st_as_sf() |>
  mutate(
    grid_id = c(1:length(test_grid))
  ) -> test_grid_sf

st_intersects(test_grid, species_point |> slice(1:10)) |>
  lengths()

# the difference between length() and lengths()
# length() return the all length with one number
# lengths() return the interval length of every elements

# 利用等面积投影，通过单位m的方式创建网格
m_fishnet_grid = species_point |> 
  st_transform(32648) |> 
  st_geometry() |> 
  st_make_grid(cellsize = 100000, what = "polygons", square = TRUE) |> 
  st_as_sf() |> 
  st_transform(4326) |>
  st_filter(species_point,.predicate = st_intersects) |> 
  st_filter(species_point,.predicate = st_disjoint)

  mapview(area_fishnet_grid)
