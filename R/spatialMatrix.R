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
  
plot(cm_sf["Lisa_cm"])

library(ggplot2)

ggplot(cm_sf) +
  geom_sf(aes(fill = Lisa_cm)) +
  scale_fill_viridis_c() +
  theme_minimal()