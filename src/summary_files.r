summary.cap.eia = cap.eia %>%
  group_by(overnight_category, fuel_1, year) %>%
  summarize(capacity_tw = sum(summer_capacity)/1000000, 
            # Capacity (TW) by overnight category, fuel, and year
            n = n(), 
            # Count of plants (same divisions)
            avg_size_mw = sum(summer_capacity)/n(),
            # Average size of the plants (same divisions)
            heat_rate = mean(heat_rate, na.rm=TRUE)) %>%
  # Average heat rate (same divisions)
  ungroup() %>%
  group_by(overnight_category, fuel_1) %>%
  complete(year = seq(from = 1990, to =2014, by = 1)) %>%
  # Fill in the missing years for the unique combination of overnight category and primary fuel in the database
  ungroup() %>%
  arrange(overnight_category, fuel_1, year)
# Order the data

summary.cap.eia.year = cap.eia %>%
  group_by(overnight_category, year, fuel_1, fuel_1_text) %>%
  summarize(capacity_mw = sum(summer_capacity),
            n = n(), 
            avg_size_mw = sum(summer_capacity)/n(),
            avg_age = mean(age),
            heat_rate = mean(heat_rate, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(overnight_category, fuel_1, fuel_1_text) %>%
  complete(year = seq(from = 1990, to = 2014, by = 1)) %>%
  mutate(diff_mw = capacity_mw - lag(capacity_mw)) %>%
  mutate(outlier = ifelse(heat_rate <= mean(heat_rate, na.rm=TRUE) - 2*sd(heat_rate, na.rm=TRUE) |
                            heat_rate >= mean(heat_rate, na.rm=TRUE) + 2*sd(heat_rate, na.rm=TRUE), 1, 0)) %>%
  # Identify potential outliers (most of the outliers identified are a simple case of no variance (i.e., identical values))
  mutate(heat_rate = replace(heat_rate, overnight_category=="biomass" & fuel_1=="REF" & year==1993, NA)) %>%
  # One proposed outlier (replace the heat_rate value with NA)[see supporting documentation for more detail]
  mutate(heat_rate = replace(heat_rate, overnight_category=="steam turbine" & fuel_1=="FO4" & year==1993, NA)) %>%
  # Second proposed outlier (replace the heat_rate value with NA)
  ungroup() %>%
  arrange(year) %>%
  arrange(overnight_category, fuel_1, year) 

h.r.model = summary.cap.eia.year %>%
  filter(heat_rate > 0) %>%
  group_by(overnight_category, fuel_1, fuel_1_text) %>%
  complete(year = seq(from = 1990, to = 2014, by = 1)) %>% 
  ungroup() %>%
  group_by(overnight_category, fuel_1) %>%
  do({mod <- lm(heat_rate ~ year, data = .)
  pred <- predict(mod, newdata = .["year"])
  data.frame(., pred)}) %>%
  select(c(overnight_category, fuel_1, fuel_1_text, year, pred))

summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(h.r.model, by = c("overnight_category", "fuel_1", "fuel_1_text", "year")) %>%
  arrange(overnight_category, fuel_1, year)



gz1 = gzfile(paste(path_data,"summary_summer_cap_eia_860_overnight_cost.txt.gz", sep=""), "w")
write.table(summary.cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file

gz1 = gzfile(paste(path_data,"summary_year_summer_cap_eia_860_overnight_cost.txt.gz", sep=""), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file

