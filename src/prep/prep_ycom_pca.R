# prep_YCOM_pca.R
#
# Apply PCA to YCOM dataset to understand major components of climate change perceptions.


library(tidyverse)

out_path <- ggp::fig_set_output("prep_YCOM_pca.R")

# 0: geography, 1: beliefs, 2: perceptions/worry, 3: policy (specific), 4: policy (do more), 5: behavior/media
meta <- read_csv("data/format/YCOM_2019_meta.csv")
ycom <- read_csv("data/orig/yale/YCOM_2019_Data.csv")

# table(ycom$GeoType)
# ycom_state <- ycom %>% filter(GeoType=="State") %>% 
#   mutate(state=state.abb[match(GeoName,state.name)],
#          state=ifelse(grepl("District of Columbia",GeoName),"D.C.",state),
#          state=factor(state,levels=state[order(human)]))
# ycom_state_long <- ycom_state %>%
#   gather(question,percent,-GeoType,-GEOID,-GeoName,-TotalPop,-state) %>%
#   mutate(response=factor(if_else(grepl("Oppose",question),"oppose","affirm"),levels=c("oppose","affirm")),
#          topic=gsub("Oppose","",question)) %>%
#   select(-question) %>% left_join(meta %>% select(Group,topic),by="topic")
# 
# ggplot(ycom_state_long) + geom_bar(aes(state,percent,fill=response),stat="identity") +
#   facet_wrap(~topic) + theme(axis.text.x=element_text(angle=90,size=4))
# 
# GGally::ggpairs(ycom_state[,names(ycom_state) %in% meta$topic[meta$Group==1] & !grepl("Oppose",names(ycom_state))],mapping=aes(alpha=0.01))

ycom_county <- ycom %>% filter(GeoType=="County") %>% select(-ends_with("Oppose"))

ycom_county_beliefs <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group==1]))
ycom_county_perceptions <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group==2]))
ycom_county_policy <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group %in% c(3,4)]))


GGally::ggpairs(ycom_county_beliefs,mapping=aes(alpha=0.01))
GGally::ggpairs(ycom_county_perceptions,mapping=aes(alpha=0.01))
GGally::ggpairs(ycom_county_policy,mapping=aes(alpha=0.01))


perceptions_pca <- prcomp(x=ycom_county_perceptions)

beliefs_PCs <- get_PCs(ycom_county_beliefs)
ggplot(beliefs_PCs %>% gather(PC,val,PC2:PC4)) + geom_point(aes(PC1,val),alpha=0.2) +
  facet_wrap(~PC,ncol=1,scales="free_y")

perceptions_PCs <- get_PCs(ycom_county_perceptions)
ggplot(perceptions_PCs %>% gather(PC,val,PC2:PC5)) + geom_point(aes(PC1,val),alpha=0.2) + #coord_equal() + 
  facet_wrap(~PC,ncol=1,scales="free_y")

policy_PCs <- get_PCs(ycom_county_policy)
ggplot(policy_PCs %>% gather(PC,val,PC2:PC5)) + geom_point(aes(PC1,val),alpha=0.2) +
  facet_wrap(~PC,ncol=1,scales="free_y")

get_PCs <- function(df,n_PC=Inf) {
  df <- as.data.frame(df)
  cat("Running PCA...\n")
  df_pca <- prcomp(x=df)
  df_pca_rotation <- df_pca$rotation[,1:min(n_PC,ncol(df))]
  df_names <- row.names(df_pca$rotation)
  cat("Calculating PCs\n")
  df_PCs <- (as.matrix(df[,match(df_names,names(df))]) %*% df_pca_rotation) %>%
    as_tibble()
  cat("Done.\n")
  return(df_PCs)
}

ycom_pca <- ycom_county %>% select(FIPS=GEOID,County=GeoName,TotalPop) %>%
  bind_cols(beliefs_PCs %>% select(belief_PC1=PC1)) %>% 
  bind_cols(perceptions_PCs %>% select(perception_PC1=PC1)) %>% 
  bind_cols(policy_PCs %>% select(policy_PC1=PC1,policy_PC3=PC3))


write_csv(ycom_pca,"data/format/climate_perceptions_ycom_pca.csv")

# p_WGI_PCA <- factoextra::fviz_pca_biplot(wgi_pca,geom="point",col.ind = "darkgray",col.var="darkred") +
#   coord_equal()
# png(file.path(output_dir_path,"WGI_PCA_1_2.png"),width=1000,height=600)
# print(p_WGI_PCA)
# dev.off()