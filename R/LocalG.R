library(rgeoda)
library(sf)
library(dplyr)

raw_path = system.file("extdata", "Guerry.shp", package = "rgeoda")
raw_data = st_read(raw_path)

# 生成queen权重矩阵
queen_w = queen_weights(raw_data)

# 获取每一个对象的邻近对象的index
nbrs = get_neighbors(queen_w, idx = 1)
nbrs

# 计算空间滞后lag，需要指定数据列
lag_q = spatial_lag(queen_w, raw_data['Crm_prs'])

# What is the spatial lag
raw_data$Crm_prs[nbrs] |>
  mean()
lag_q$Spatial.Lag[1]

# Caculate the LISA
lisa_cm = local_moran(queen_w, raw_data["Crm_prp"])

cm_sf = raw_data |>
  select(geometry) |>
  bind_cols(
    data.frame("Lisa_cm" = lisa_cm$lisa_vals)
  )

localg_crmprp <- local_g(queen_w, raw_data["Crm_prp"])

splilabel = lisa_labels(localg_crmprp)
splisac = lisa_clusters(localg_crmprp)
splisac = factor(splilabel[splisac + 1], levels = splilabel)

local_g_plot = \(sf, col_name, w_type, cpu_n, cutoff, p_on) {
  if (w_type == "queen") {
    w = rgeoda::queen_weights(sf)
  } else if (w_type == "rook") {
    w = rgeoda::rook_weights(sf)
  }
  var_df = sf |>
    sf::st_drop_geometry() |>
    dplyr::select(all_of(col_name))
  tlisa = rgeoda::local_g(
    w,
    var_df,
    cpu_threads = cpu_n,
    significance_cutoff = cutoff
  )
  lisalabel = rgeoda::lisa_labels(tlisa)
  lisa_df = sf |>
    dplyr::select(col_name) |>
    mutate(
      lisaG = rgeoda::lisa_values(tlisa),
      GMean = mean(lisaG),
      Pvalue = rgeoda::lisa_pvalues(tlisa),
      lisaindex = rgeoda::lisa_clusters(tlisa),
      lisalabindex = factor(lisalabel[lisaindex + 1], levels = lisalabel),
      .before = "geometry"
    )
  return(lisa_df)
}

a = local_g_plot(raw_data, "Crm_prp", "queen", 6, 0.05, TRUE)

# ggplot2 practice
library(ggplot2)

a |>
  ggplot() +
  geom_sf(aes(fill = lisalabindex)) -> p

p +
  scale_fill_discrete(palette = c("#e6e6e6", "red", "blue")) +
  theme_bw() +
  guides(fill = guide_legend(title = "LISA"))
