ca.mapping = read.table(paste0(path_data, "ca.mapping.txt.gz"), header=TRUE, sep="\t")
# Dataset that maps the unique identifier in the CA dataset to the EIA identification system
# Source: http://energyalmanac.ca.gov/electricity/web_qfer/
ca.mapping = ca.mapping %>%
  filter(is.na(plant_code)==FALSE) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  distinct(plant.id, plant_code, .keep_all = TRUE) %>%
  select(plant.id, plant_code)

ca.elec.almanac = read.table(paste0(path_data, "ca.elec.almanac.txt.gz"), header=TRUE, sep="\t")
ca.elec.almanac = ca.elec.almanac %>%
  filter(heat.rate != 0) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  left_join(ca.mapping, by = "plant.id") %>%
  mutate(heat.rate = 1000*heat.rate) %>%
  # Converts units from MMbtu/MWh to btu/kWh (units favored by the EIA)
  select(plant_code, year, heat.rate) %>%
  filter(year != 2015)

cap.eia = read.table(paste0(path_data, "mapping_860_overnight_R.txt.gz"), header=TRUE, sep ="\t")
cap.eia = cap.eia %>%
  mutate(heat_rate = replace(heat_rate, heat_rate == 0, NA)) %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  mutate(heat_rate = ifelse(is.na(heat_rate)==FALSE, heat_rate, heat.rate))


gz1 = gzfile(paste(path_data,"ca_almanac_R.txt.gz", sep=""), "w")
write.table(cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Adds the heat rate information from the California energy almanac