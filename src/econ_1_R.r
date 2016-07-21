install.packages("R.utils")
install.packages("dplyr")
options(scipen=999)
require(R.utils)
require(dplyr)

cap.raw = read.csv("/Users/bloh356/Documents/fuzzy-waffle/data/capacity_eia.txt.gz", sep="\t", header=TRUE, comment.char="")

cap.eia = cap.raw %>%
  filter(summer_capacity != 0) %>%
  # Remove plants with no summer time capacity (not sure why they exist in the data anyway)
  filter(status_code_1 != "RE" & status_code_2 != "RE") %>%
  # Remove retired plants
  filter(status_code_2 != "CN") %>%
  # Remove previously planned but now canceled projects
  filter(status_code_2 != "PL" & status_code_2 != "P" & status_code_2 != "IP") %>%
  # Remove planned projects (P and PL) and indefinitely postponed, 
  filter(status_code_2 != "CO" & status_code_2 != "L" & status_code_2 != "T") %>%
  # Remove units under construction, units awaiting regulatory approval, and units with regulatory approval but not under construciton
  filter(status_code_2 != "U" & status_code_2 != "V" & status_code_2 != "TS" & status_code_1 != "TS") %>%
  # Remove additional plants under construction (U and V) and constructed power plants not yet in operation
  filter(status_code_2 != "LE") %>%
  # Remove sites that are not constructed (legal delays)
  mutate(status_fulltext = 2)
