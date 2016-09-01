data.raw = read.table(paste0(path_data, "processed.overnight.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)

data = data.raw %>%
  select(overnight_category, fuel_1_general, pred.heat.rate, year, diff_mw, adj.fuel.price, base.overnight, variable.o.m, fixed.o.m) %>%
  filter(year >= 1999) %>%
  # We don't have overnight cost data for the period prior to this (we have 1997 but are missing 1998)
  filter(fuel_1_general != "biomass") %>%
  # We don't have biomass fuel prices at this time.
  filter(fuel_1_general != "lpg") %>%
  # We don't have lpg fuel prices at this time. Also, probably more importantly, no power plants use it.
  filter(fuel_1_general != "black liquor") %>%
  # We don't have black liquor fuel prices at this time.
  filter(fuel_1_general != "kerosene") %>%
  # We don't have kerosene fuel prices at this time.
  filter(fuel_1_general != "propane") %>%
  # We don't have propane fuel prices at this time. Also, probably more importantly, not many power plants use it.
  filter(fuel_1_general != "MWh") %>%
  # We don't have MWh fuel prices at this time.
  filter(overnight_category != "conventional combined cycle" | fuel_1_general != "coal") %>%
  # We don't have heat rate data for conventional combined cycle plants that use coal
  filter(overnight_category != "conventional combined cycle" | fuel_1_general != 'kerosene') %>%
  # We don't have heat rate data for conventional combined cycle plants that use kerosene
  filter(overnight_category != 'conventional combustion turbine' | fuel_1_general != 'coal') %>%
  # We don't have heat rate data for a conventional combustion turbine plant that uses coal
  filter(fuel_1_general != 'black liquor') %>%
  # We don't have price data for black liquor
  filter(year != 2100 & year != 2050) %>%
  # These values were only useful in the regression model
  filter(year != 1990) %>%
  # These are all NAs given the construction of our dependent variable
  filter(fuel_1_general != "lpg") %>%
  filter(fuel_1_general != 'propane') %>%
  filter(overnight_category != 'distributed') %>%
  arrange(overnight_category, fuel_1_general, year)
