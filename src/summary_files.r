cap.eia = read.table(paste0(path_data, "ca_almanac_R.txt.gz"), header=TRUE, sep ="\t")
summary.cap.eia.year = cap.eia %>%
  filter(is.na(fuel_1_general) == FALSE) %>%
  group_by(overnight_category, year, fuel_1_general) %>%
  summarize(capacity_mw = sum(summer_capacity),
            n = n(), 
            avg_size_mw = sum(summer_capacity)/n(),
            avg_age = mean(age),
            heat_rate = mean(heat_rate, na.rm=TRUE)) %>%
  arrange(overnight_category, fuel_1_general, year) %>%
  ungroup() %>%
  group_by(overnight_category, fuel_1_general) %>%
  complete(year = seq(from = 1990, to = 2014, by = 1)) %>%
  mutate(diff_mw = capacity_mw - lag(capacity_mw)) %>%
  mutate(outlier = ifelse(heat_rate <= mean(heat_rate, na.rm=TRUE) - 2*sd(heat_rate, na.rm=TRUE) |
                            heat_rate >= mean(heat_rate, na.rm=TRUE) + 2*sd(heat_rate, na.rm=TRUE), 1, 0)) %>%
  # Identify potential outliers (most of the outliers identified are a simple case of no variance (i.e., identical values))
  ungroup() %>%
  arrange(overnight_category, fuel_1_general, year) 

theoretical.limits = data.frame(overnight_category = c("biomass", "coal", "conventional combined cycle", "conventional combined cycle", 
                                                       "conventional combustion turbine", "conventional combustion turbine", "igcc"),
                                fuel_1_general = c("biomass", "coal", "natural gas", "oil", "natural gas", "oil", "coal"), 
                                year.2050 = c(0.31,0.45,0.57,0.59,0.38,0.39,0.48),
                                year.2100 = c(0.36,0.51,0.62,0.64,0.42,0.44,0.54)
                                )
# Source: The data is from Table 5 of 'Cost of power or power of cost: a US modeling perspective' by Muratori, Ledna, McJeon, et al.
# We can use this data to put some upper bounds on the heat rate gains by power plant type
theoretical.limits = melt(theoretical.limits, id.vars = c("overnight_category", "fuel_1_general"))
colnames(theoretical.limits) = c("overnight_category", "fuel_1_general", "year", "efficiency")
theoretical.limits = theoretical.limits %>%
  mutate(year = as.character(year)) %>%
  mutate(year = replace(year, year=="year.2050", 2050)) %>%
  mutate(year = replace(year, year=="year.2100", 2100)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(heat_rate = 3412/efficiency)
  # We can convert the efficiency back to a heat rate using the equivalent btu content of a kWh (~3,412 btu). This is also in the documentation. 

summary.cap.eia.year = summary.cap.eia.year %>%
  plyr::rbind.fill(theoretical.limits)

h.r.model = summary.cap.eia.year %>%
  filter(heat_rate > 0) %>%
  group_by(overnight_category, fuel_1_general) %>%
  complete(year = seq(from = 1990, to = 2100, by = 1)) %>% 
  ungroup() %>%
  group_by(overnight_category, fuel_1_general) %>%
  do({mod <- lm(heat_rate ~ year, data = .)
  pred.heat.rate <- predict(mod, newdata = .["year"])
  data.frame(., pred.heat.rate)}) %>%
  select(c(overnight_category, fuel_1_general, year, pred.heat.rate))
# We still have overnight category, general fuel combinations with low sample sizes (even after implementing the aggregation measures). 

summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(h.r.model, by = c("overnight_category", "fuel_1_general", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

energy.prices = read.table(paste0(path_data, "energy.prices.txt.gz"), header=TRUE, sep ="\t")

uranium.prices = read.table(paste0(path_data, "uranium.prices.txt.gz"), header=TRUE, sep ="\t")
uranium.prices = uranium.prices[,1:2] %>%
  # grab the year column and weighted average price across all sources
  mutate(fuel_1_general = "uranium")
  # add the necessary identifier
colnames(uranium.prices)[names(uranium.prices) == "weighted.avg.price.nominal"] = "fuel.price"
energy.prices = rbind(energy.prices, uranium.prices)

summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(energy.prices, by = c('fuel_1_general','year')) %>%
  mutate(adj.fuel.price = (fuel.price/10^6)*pred.heat.rate)
# Fuel price is in $ per 10^6 btu, which we convert to $ per kWh using the heat rate and fuel price.

  

gz1 = gzfile(paste(path_data,"summary_files.txt.gz", sep=""), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file