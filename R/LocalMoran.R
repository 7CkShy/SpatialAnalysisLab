raw_path = system.file("extdata", "Guerry.shp", package = "rgeoda")
raw_data = sf::st_read(raw_path)

local_moran_plot = \(sff, col_name, w_t, per_n, cpu_n, cut_off_n) {
  if (w_t == "queen") {
    w = rgeoda::queen_weights(sff)
  } else if (w_t == "rook") {
    w = rgeoda::rook_weights(sff)
  }
  ds = sff |>
    sf::st_drop_geometry() |>
    dplyr::select(all_of(col_name))
  lisa = rgeoda::local_moran(
    w,
    ds,
    permutations = per_n,
    cpu_threads = cpu_n,
    significance_cutoff = cut_off_n
  )
  lisaLabes = rgeoda::lisa_labels(lisa)
  lisa_sf_df = sff |>
    dplyr::select(col_name) |>
    dplyr::mutate(
      lisaMoran = rgeoda::lisa_values(lisa),
      Pvalue = rgeoda::lisa_pvalues(lisa),
      lisaIndex = rgeoda::lisa_clusters(lisa),
      lisaLabIndex = factor(lisaLabes[lisaIndex + 1], levels = lisaLabes),
      .before = "geometry"
    )
  return(lisa_sf_df)
}

local_moran_plot(raw_data, "Crm_prp", "queen", 999, 8, 0.05) -> moran_df

# ggplot plot
library(ggplot2)

moran_df |>
  ggplot() +
  geom_sf(aes(fill = lisaLabIndex)) -> p_m

p_m +
  scale_fill_manual(
    values = c(
      "Not significant" = "#f2f2f2",
      "High-High" = "red",
      "Low-Low" = "blue",
      "Low-High" = "green"
    )
  ) +
  guides(
    fill = guide_legend(title = "LISA")
  ) +
  theme_bw()
