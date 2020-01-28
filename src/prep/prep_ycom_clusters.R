# prep_ycom_clusters.R

# perceptions prep
ycom_full <- read_csv("data/format/climate_perceptions_ycom_pca.csv") %>% select(-GeoName,-GeoType)

ycom_pca <- ycom_full %>% select(PC1:PC6)
## get PC clusters:
clusters <- kmeans(ycom_pca,8)

ycom_pca_clusters <- ycom_full %>% bind_cols(cluster=as.factor(clusters$cluster))
write_csv(ycom_pca_clusters,"data/format/climate_perceptions_ycom_pca_with_clusters.csv")

ggplot(ycom_pca_clusters) + geom_point(aes(PC1,PC4,color=cluster)) +
  scale_color_brewer(type="qual",palette="Set1")

# map
us_counties_sf <- sf::st_read("spatial/shp/us_counties/us_counties_laea.shp")
us_states_sf <- sf::st_read("spatial/shp/us_states/us_states_laea.shp")

us_counties_ycom <- us_counties_sf %>% left_join(ycom_pca_clusters,by="fips")

p_pc2_4 <- ggplot() + geom_sf(data=us_counties_ycom %>% gather(PC,val,PC2:PC4),aes(fill=val),color=NA) +
  geom_sf(data=us_states_sf,aes(),fill=NA,color="white",size=0.25) +
  viridis::scale_fill_viridis() +
  facet_wrap(~PC)

p_pc1 <- ggplot() + geom_sf(data=us_counties_ycom,aes(fill=PC1),color=NA) +
  geom_sf(data=us_states_sf,aes(),fill=NA,color="white",size=0.25) +
  viridis::scale_fill_viridis()

egg::ggarrange(p_pc1,p_pc2_4,heights = c(0.67,0.33),ncol=1)

cols <- colors()
cols <- cols[!grepl("gr[ae]y",cols)]
ggplot() + geom_sf(data=us_counties_ycom,aes(fill=cluster),color=NA) +
  geom_sf(data=us_states_sf,aes(),fill=NA,color="white",size=0.25) +
  scale_fill_brewer(type="qual",palette="Set2")
  # scale_fill_manual(values=colorspace::heat_hcl(n=8,h=c(0,360),l=c(20,80)))

