ca.mapping = read.table(paste0(path_data, "ca.mapping.txt.gz"), header=TRUE, sep="\t")
# Dataset that maps the unique identifier in the CA dataset to the EIA identification system
# Source: http://energyalmanac.ca.gov/electricity/web_qfer/
ca.mapping = ca.mapping %>%
  filter(is.na(plant_code)==FALSE) %>%
  mutate(fuel_1_general = regmatches(as.character(plant.id), gregexpr('[a-zA-Z]+', plant.id))) %>%
  mutate(date = as.Date(Start.Date, format = '%m/%d/%Y')) %>%
  mutate(fuel_1_general = ifelse(fuel_1_general == "C", "coal", 
                                 ifelse(fuel_1_general == "S", "solar",
                                        ifelse(fuel_1_general == "T", "geothermal",
                                               ifelse(fuel_1_general == "N", "uranium",
                                                      ifelse(fuel_1_general == "E", "biomass",
                                                             ifelse(fuel_1_general == "H", "water",
                                                                    ifelse(fuel_1_general == "W", "wind", fuel_1_general)))))))) %>%
  mutate(in_service = year(date)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  mutate(Unit = tolower(Unit)) %>%
  mutate(Unit = gsub("[^a-zA-Z0-9]","",Unit)) %>%
  mutate(Unit = gsub(" ","", Unit)) %>%
  mutate(unit.numeric = regmatches(Unit, gregexpr('[0-9]+', Unit))) %>%
  # standardize all the text
  mutate(id.1 = paste0(plant_code, Unit)) %>%
  mutate(id.2 = paste0(plant_code, unit.numeric)) %>%
  select(c(plant.id, Unit, plant_code, unit.numeric, in_service, fuel_1_general, id.1, id.2))
  # we only need the mapping identifiers


unit.numeric = regmatches(ca.mapping$Unit, gregexpr('[0-9]+', ca.mapping$Unit))
names(ca.mapping)[names(ca.mapping)=="Unit"] = "generator_code"

ca.elec.almanac = read.table(paste0(path_data, "ca.elec.almanac.txt.gz"), header=TRUE, sep="\t")
ca.elec.almanac = ca.elec.almanac %>%
  filter(heat.rate != 0) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  left_join(ca.mapping, by = "plant.id") %>%
  select(plant.id, generator_code, plant_code, unit.numeric, in_service, fuel_1_general, year, id.1, id.2, heat.rate) %>%
  filter(year != 2015)

ca.1 = ca.elec.almanac %>%
  select(id.1, year, heat.rate)
ca.2 = ca.elec.almanac %>%
  select(id.2, year, heat.rate)

cap.eia = read.table(paste0(path_data, "mapping_860_overnight_R.txt.gz"), header=TRUE, sep ="\t")
id = cap.eia %>%
  mutate(id.1 = paste0(plant_code, generator_code)) %>%
  mutate(unit.numeric = regmatches(as.character(generator_code), gregexpr('[0-9]+', generator_code))) %>%
  mutate(id.2 = paste0(plant_code, unit.numeric)) %>%
  select(plant_code, generator_code, unit.numeric, in_service, fuel_1_general, year, id.1, id.2, heat_rate)

match.1 = ca.elec.almanac[(ca.elec.almanac$id.1 %in% id$id.1),]
final.1 = match.1 %>%
  select(id.1, year, heat.rate) 
unmatch = ca.elec.almanac[!(ca.elec.almanac$id.1 %in% id$id.1),]
match.2 = unmatch[(unmatch$id.2 %in% id$id.2),]
final.2 = match.2 %>%
  select(id.2, year, heat.rate)
unmatch.2 = unmatch[!(unmatch$id.2 %in% id$id.2),]
match.3 = unmatch.2 %>%
  left_join(ca.elec.almanac, by = c("plant_code", "in_service", "year", "fuel_1_general"))


### This is where i'm working. We are working through the unmatched entries between the CA energy almanac and EIA Form 860.
### There are presently ~3,500 entries unmatched. I think we can do a lot by matching the unmatched information to the CA almanac using the in_service data,
### plant_code, and fuel_1_general. However, the issue we're running into is that the regular expressions in the ca.mapping creation is creating lists inside
### the dataframe (as opposed to vectors). The first thing we need to do then is to fix this issue so that we can use the left_join (above) and see how
### many remain unassigned. 


id = id %>%
  left_join(final.1, by = c("id.1", "year")) %>%
  left_join(final.2, by = c("id.2", "year"))

sum(!(ca.1$id.1 %in% id$id.1))

cap.eia = cap.eia %>%
  left_join(ca.elec.almanac, by = c("year", "id.1")) %>%
  left_join(ca.elec.almanac, by = c("year", "id.2")) %>%
  mutate(heat.rate = 1000*heat.rate) %>%
  # Converts units from MMbtu/MWh to btu/kWh (units favored by the EIA)
  mutate(heat_rate = ifelse(is.na(heat_rate) == FALSE, heat_rate, heat.rate))

gz1 = gzfile(paste(path_data,"ca_almanac_R.txt.gz", sep=""), "w")
write.table(cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Adds the heat rate information from the California energy almanac