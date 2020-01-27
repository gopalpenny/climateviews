# prep_YCOM_pca.R
#
# Apply PCA to YCOM dataset to understand major components of climate change perceptions.


library(tidyverse)

out_path <- ggp::fig_set_output("prep_YCOM_pca.R")

meta <- read_csv("data/format/YCOM_2019_meta.csv")

table(ycom$GeoType)
ycom_state <- ycom %>% filter(GeoType=="State") %>% 
  mutate(state=state.abb[match(GeoName,state.name)],
         state=ifelse(grepl("District of Columbia",GeoName),"D.C.",state),
         state=factor(state,levels=state[order(human)]))
ycom_state_long <- ycom_state %>%
  gather(question,percent,-GeoType,-GEOID,-GeoName,-TotalPop,-state) %>%
  mutate(response=factor(if_else(grepl("Oppose",question),"oppose","affirm"),levels=c("oppose","affirm")),
         topic=gsub("Oppose","",question)) %>%
  select(-question) %>% left_join(meta %>% select(Group,topic),by="topic")

ggplot(ycom_state_long) + geom_bar(aes(state,percent,fill=response),stat="identity") +
  facet_wrap(~topic) + theme(axis.text.x=element_text(angle=90,size=4))

GGally::ggpairs(ycom_state[,names(ycom_state) %in% meta$topic[meta$Group==1] & !grepl("Oppose",names(ycom_state))],mapping=aes(alpha=0.01))

ycom_county <- ycom %>% filter(GeoType=="County")
GGally::ggpairs(ycom_county[,names(ycom_county) %in% meta$topic[meta$Group==1] & !grepl("Oppose",names(ycom_county))],mapping=aes(alpha=0.01))


wgi_pca <- prcomp(x=WGI_prep_wide_pca)

wgi_names <- c("ControlofCorruption","GovernmentEffectiveness","Political StabilityNoViolence", "RegulatoryQuality","RuleofLaw","VoiceandAccountability")
wgi_col_ind <- match(wgi_names,names(WGI_prep_wide))
wgi_pca_rotation <- wgi_pca$rotation[,1:3]
WGI_save <- WGI_prep_wide %>% 
  cbind(as.matrix(WGI_prep_wide[,wgi_col_ind]) %*% wgi_pca_rotation) %>%
  as_tibble()

p_WGI_PCA <- factoextra::fviz_pca_biplot(wgi_pca,geom="point",col.ind = "darkgray",col.var="darkred") +
  coord_equal()
png(file.path(output_dir_path,"WGI_PCA_1_2.png"),width=1000,height=600)
print(p_WGI_PCA)
dev.off()