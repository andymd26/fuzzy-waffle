ca.mapping = read.table(paste0(path_data, "ca.mapping.txt.gz"), header=TRUE, sep="\t")
# Dataset that maps the unique identifier in the CA dataset to the EIA identification system
# Source: http://energyalmanac.ca.gov/electricity/web_qfer/
ca.mapping = ca.mapping %>%
  mutate(plant.id = tolower(plant.id)) %>%
  # standardize all the text
  select(c(plant.id, plant_code))
  # we only need the mapping identifiers

ca.elec.almanac = read.table(paste0(path_data, "ca.elec.almanac.txt.gz"), header=TRUE, sep="\t")
ca.elec.almanac = ca.elec.almanac %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  left_join(ca.mapping, by = "plant.id") %>%
  filter(year != 2015)

cap.eia = read.table(paste0(path_data, "clean_capacity_eia_860_overnight_cost.txt.gz"), header=TRUE, sep ="\t")
cap.eia = cap.eia %>%
  left_join(ca.elec.almanac, by = c("year", "plant_code")) %>%
  mutate(heat.rate = 1000*heat.rate) %>%
  # Converts units from MMbtu/MWh to btu/kWh (units favored by the EIA)
  mutate(heat_rate = ifelse(is.na(heat_rate) == FALSE, heat_rate, heat.rate))

gz1 = gzfile(paste(path_data,"clean_capacity_eia_860_overnight_cost.txt.gz", sep=""), "w")
write.table(cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Adds the heat rate information from the California energy almanac