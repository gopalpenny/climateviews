# prep_Boustan_supersevere.R

boustan <- read_csv("data/orig/Boustan/data-ElSJG.csv")

write_csv(boustan,"data/format/longterm_disaster_count.csv")
