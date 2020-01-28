# prep_mit_election_data.R

# https://electionlab.mit.edu/data
# https://github.com/MEDSL/2018-elections-unoffical
mit_election <- read_csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv") %>% 
  mutate(obama12_pct= 100 * obama12 / (romney12 + obama12),
         clinton16_pct= 100 * clinton16 / (trump16 + clinton16),
         urban_pct = 100 - rural_pct)

write_csv(mit_election,"data/format/mit_election_18.csv")
