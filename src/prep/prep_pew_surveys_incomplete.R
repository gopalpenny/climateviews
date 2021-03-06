library(haven)
library(tidyverse)

#### WAVE 39
path <- "data/orig/pew/W39_Nov18/ATP W39.sav"
file.exists(path)

pew_w39 <- read_sav(path)

n <- ncol(pew_w39)
labels_list <- map(1:n, function(x) attr(pew_w39[[x]], "label") )
# if a vector of character strings is preferable
colnames_vector <- names(pew_w39)
colnames_vector[108]
labels_vector <- map_chr(1:ncol(pew_w39), function(x) attr(pew_w39[[x]], "label") )
# questions <- tibble(q=labels_vector)
# write_csv(questions,"data/format/W39_Nov18_questions.csv")
pew_w39 %>% select(108)

pew_w39 %>% select(c(1,grep("eight",labels_vector)))

ggplot(pew_w39) + geom_histogram(aes(WEIGHT_W39_FINAL))
mean(pew_w39$WEIGHT_W39_FINAL)

a <- pew_w39 %>% select(c(1,grep("limate",labels_vector)))
table(a$LRNGFPO_W39)

### SEPT 18

path <- "data/orig/pew/Sept18/Sept18 public.sav"
sept18 <- read_sav(path)
labels_vector <- map_chr(1:ncol(sept18), function(x) attr(sept18[[x]], "label") )
labels_vector[grepl("district",labels_vector)]
