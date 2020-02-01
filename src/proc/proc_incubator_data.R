# proc_incubator_data.R

# in progress -- a few visualizations that don't really show anything

df <- read_csv("results/output/incubator_data.csv") %>%
  mutate(wildfire_risk=factor(wildfire_risk,levels=c("Low","Medium","High")),
         log10_amount_2010s=log10(amount_2010s))

summary(lm(CO2limits~log10_amount_2010s+clinton16+black+hispanic+nonwhite,data=df %>% filter(abs(log10_amount_2010s)!=Inf)))
summary(lm(worried~log10_amount_2010s+clinton16+black+hispanic+nonwhite,data=df %>% filter(abs(log10_amount_2010s)!=Inf)))

ggplot(df %>% gather(ycomp,y_val,human, worried, CO2limits)) + 
  geom_boxplot(aes(ycomp,y_val,color=wildfire_risk))

ggplot(df %>% gather(ycomp,y_val,human, worried, CO2limits) %>% 
         gather(var,val,D2_pct_mean,log10_amount_2010s)) + 
  geom_point(aes(val,y_val,color=clinton16),alpha=0.5) + 
  facet_grid(ycomp~var,switch = "both",scales="free_x") +
  scale_color_gradientn(colors=rev(colorspace::diverge_hcl(n=49)[-seq(19,31)]),limits=c(0,100)) +
  # scale_color_viridis_c() + 
  theme(strip.placement = "outside",strip.background = element_rect(fill=NA), axis.title = element_blank())

ggplot(df %>% gather(PC,PCval,PC1:PC4) %>% gather(var,val,matches("^D[1-4]_pct_mean"))) + 
  geom_point(aes(val,PCval,color=human),alpha=0.2) + 
  facet_grid(PC~var,switch = "both") +
  scale_color_viridis_c() + 
  theme(strip.placement = "outside",strip.background = element_rect(fill=NA), axis.title = element_blank())

ggplot(df %>% gather(PC,PCval,PC1:PC4) %>% gather(var,val,nonwhite,black,hispanic)) + 
  geom_point(aes(val,PCval,color=human),alpha=0.2) + 
  facet_grid(PC~var,switch = "both") +
  scale_color_viridis_c()+ 
  theme(strip.placement = "outside",strip.background = element_rect(fill=NA), axis.title = element_blank())

summary(lm(PC3~D1_pct_mean + amount_2010s + wildfire_risk,data=df))
summary(lm(PC3~amount_2010s,data=df))

summary(lm(PC3~wildfire_risk,data=df))
