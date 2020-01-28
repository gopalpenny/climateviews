# prep_YCOM_pca.R
#
# Apply PCA to YCOM dataset to understand major components of climate change perceptions.


library(tidyverse)

out_path <- ggp::fig_set_output("prep_YCOM_pca")

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

ycom_county_all <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group %in% 1:5]))
ycom_county_beliefs <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group==1]))
ycom_county_perceptions <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group==2]))
ycom_county_policy <- ycom_county %>% select(which(names(.) %in% meta$topic[meta$Group %in% c(3,4)]))


p_beliefs <- GGally::ggpairs(ycom_county_beliefs,mapping=aes(alpha=0.01))
ggsave(file.path(out_path,"pairs_beliefs.png"),p_beliefs,width=3,height=3)
p_perceptions <- GGally::ggpairs(ycom_county_perceptions,mapping=aes(alpha=0.01))
ggsave(file.path(out_path,"pairs_perceptions.png"),p_perceptions,width=3,height=3)
p_policy <- GGally::ggpairs(ycom_county_policy,mapping=aes(alpha=0.01))
ggsave(file.path(out_path,"pairs_policy.png"),p_policy,width=3,height=3)

#### GET PRINCIPLE COMPONENTS
get_PCs <- function(df,n_PC=Inf,normalize=TRUE,output="PCs") {
  # df should be cleaned before running function (e.g., with na.omit())
  # if output=="PCs" -- return principle components
  # if output=="pca" -- return pca output from prcomp()
  df <- as.data.frame(df)
  
  if (normalize) {
    df <- df %>% mutate_all(function(x) (x-mean(x))/sd(x))
  }
  cat("Running PCA...\n")
  df_pca <- prcomp(x=df)
  df_pca_rotation <- df_pca$rotation[,1:min(n_PC,ncol(df))]
  df_names <- row.names(df_pca$rotation)
  cat("Calculating PCs\n")
  df_PCs <- (as.matrix(df[,match(df_names,names(df))]) %*% df_pca_rotation) %>%
    as_tibble()
  cat("Done.\n")
  if (output=="PCs") {
    return(df_PCs)
  } else if (output=="PCA") {
    return(df_pca)
  } else {
    stop("output must be PCs or PCA")
  }
}


## run PCA for all variables
all_pca <- get_PCs(ycom_county_all,output="PCA")
p_all_pca2 <- factoextra::fviz_pca_biplot(all_pca,c(1,2),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="darkred") #+ coord_equal()
p_all_pca3 <- factoextra::fviz_pca_biplot(all_pca,c(1,3),geom="point",col.ind = "#333333",alpha.ind = 0.2)# + coord_equal()
p_all_pca4 <- factoextra::fviz_pca_biplot(all_pca,c(1,4),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="darkgreen")# + coord_equal()
p_all_pca5 <- factoextra::fviz_pca_biplot(all_pca,c(1,5),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="purple")# + coord_equal()
p_all_pca6 <- factoextra::fviz_pca_biplot(all_pca,c(1,6),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="orange")# + coord_equal()
p_all_pca7 <- factoextra::fviz_pca_biplot(all_pca,c(1,6),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="brown")# + coord_equal()
ycom_pca_all <- get_PCs(ycom_county_all)

ycom_pca_full <- ycom_county %>% rename(fips=GEOID) %>% 
  bind_cols(ycom_pca_all %>% select(PC1:PC6))
write_csv(ycom_pca_full,"data/format/climate_perceptions_ycom_pca.csv")

# plots
p_beliefs_pcs <- ggplot(all_PCs %>% gather(PC,val,PC2:PC4)) + geom_point(aes(PC1,val),alpha=0.2) +
  facet_wrap(~PC,ncol=1,scales="free_y") + labs(title="Beliefs PCs")
# GGally::ggpairs(all_PCs %>% select(PC1:PC6))

png(file.path(out_path,"ycom_all_pca.png"),width=1000,height=600)
egg::ggarrange(p_all_pca2,p_all_pca3,p_all_pca4,p_all_pca5,ncol=2)
dev.off()


## run PCA for groups of  variables

# Beliefs
beliefs_pca <- get_PCs(ycom_county_beliefs,output="PCA")
p_beliefs_pca2 <- factoextra::fviz_pca_biplot(beliefs_pca,c(1,2),geom="point",col.ind = "darkgray",col.var="darkred") #+ coord_equal()
p_beliefs_pca3 <- factoextra::fviz_pca_biplot(beliefs_pca,c(1,3),geom="point",col.ind = "darkgray")# + coord_equal()
beliefs_PCs <- get_PCs(ycom_county_beliefs)
p_beliefs_pcs <- ggplot(beliefs_PCs %>% gather(PC,val,PC2:PC4)) + geom_point(aes(PC1,val),alpha=0.2) +
  facet_wrap(~PC,ncol=1,scales="free_y") + labs(title="Beliefs PCs")

# Perceptions
perceptions_pca <- get_PCs(ycom_county_perceptions,output="PCA")
p_perceptions_pca2 <- factoextra::fviz_pca_biplot(perceptions_pca,c(1,2),geom="point",col.ind = "darkgray",col.var="darkred") #+ coord_equal()
p_perceptions_pca3 <- factoextra::fviz_pca_biplot(perceptions_pca,c(1,3),geom="point",col.ind = "darkgray") #+ coord_equal()
perceptions_PCs <- get_PCs(ycom_county_perceptions)
p_perceptions_pcs <- ggplot(perceptions_PCs %>% gather(PC,val,PC2:PC5)) + geom_point(aes(PC1,val),alpha=0.2) + #coord_equal() + 
  facet_wrap(~PC,ncol=1,scales="free_y") + labs(title="Perceptions PCs")

# Policy
policy_pca <- get_PCs(ycom_county_policy,output="PCA")
p_policy_pca2 <- factoextra::fviz_pca_biplot(policy_pca,c(1,2),geom="point",col.ind = "#333333",alpha.ind = 0.2,col.var="darkred") #+ coord_equal()
p_policy_pca3 <- factoextra::fviz_pca_biplot(policy_pca,c(1,3),geom="point",col.ind = "#333333",alpha.ind = 0.2)# + coord_equal()
policy_PCs <- get_PCs(ycom_county_policy)
p_policy_pcs <- ggplot(policy_PCs %>% gather(PC,val,PC2:PC5)) + geom_point(aes(PC1,val),alpha=0.2) +
  facet_wrap(~PC,ncol=1,scales="free_y") + labs(title="Policy PCs")

png(file.path(out_path,"ycom_pcs.png"),width=1000,height=800)
egg::ggarrange(p_beliefs_pcs,p_perceptions_pcs,p_policy_pcs,nrow = 1)
dev.off()

png(file.path(out_path,"ycom_pca1_2_3.png"),width=1200,height=800)
egg::ggarrange(p_all_pca2,p_beliefs_pca2,p_perceptions_pca2,p_policy_pca2,
               p_all_pca3,p_beliefs_pca3,p_perceptions_pca3,p_policy_pca3,ncol=2,byrow = FALSE)
dev.off()

ycom_pca_variable_groups <- ycom_county %>% select(FIPS=GEOID,County=GeoName,TotalPop) %>% rename(fips=FIPS) %>%
  bind_cols(beliefs_PCs %>% select(PC1:PC3) %>% setNames(paste0("belief_",names(.)))) %>% 
  bind_cols(perceptions_PCs %>% select(PC1:PC3) %>% setNames(paste0("perception_",names(.)))) %>% 
  bind_cols(policy_PCs %>% select(PC1:PC3) %>% setNames(paste0("policy_",names(.))))

### TEST REGRESSIONS AGAINST PCA (ALL)
# Now that PCA is complete, remove demographic trends in the data
demographics_pct <- read_csv("data/format/mit_election_18.csv") %>% 
  select(fips,female_pct, age29andunder_pct, age65andolder_pct,
         clinton16_pct,obama12_pct,white_pct, black_pct, hispanic_pct, nonwhite_pct, rural_pct) %>%
  setNames(gsub("_pct$","",names(.)))

ycom_pca_lm_prep <- ycom_pca_full %>% 
  left_join(demographics_pct,by="fips") #%>%
ycom_pca_lm <- ycom_pca_lm_prep %>% gather(PC,val,PC1:PC6) %>% 
  na.omit() %>% group_by(PC) %>%
  summarize(rsq=summary(lm(val~clinton16 + female + black + hispanic + white))$r.squared)
# lm_results <- summary(lm(val~PC:clinton16 + PC:female + PC:black + PC:hispanic + PC:white,ycom_prep_lm))
# perceptions_resid <- ycom_prep %>% bind_cols(residuals=round(lm_results$res,4)) %>% select(-val) %>%
#   spread(PC,residuals)
# perceptions_pca <- ycom_pca %>% left_join(perceptions_resid %>% select(fips,matches(".*PC[1-3]r$")),by="fips")

# dev.off()