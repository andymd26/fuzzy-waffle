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

h.r.model = summary.cap.eia.year %>%
  filter(heat_rate > 0) %>%
  group_by(overnight_category, fuel_1_general) %>%
  complete(year = seq(from = 1990, to = 2014, by = 1)) %>% 
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
summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(energy.prices, by = c('fuel_1_general','year')) %>%
  mutate(adj.fuel.price = (fuel.price/10^6)*pred.heat.rate)
# Fuel price is in $ per 10^6 btu, which we convert to $ per kWh using the heat rate and fuel price.

gz1 = gzfile(paste(path_data,"summary_files.txt.gz", sep=""), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file