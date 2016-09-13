data.raw = read.table(paste0(path_data, "processed.overnight.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)

data.final = data.raw %>%
  select(overnight_category, fuel_1_general, year, capacity.factor.avg, capacity_mw, n, pred.heat.rate, adj.fuel.price, base.overnight, variable.o.m, fixed.o.m) %>%
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
  filter(overnight_category != 'fuel cell') %>%
  # We don't have good heat rate data at this time. If we wanted to we could use the nth heat rate from the overnight cost dataset.
  mutate(adj.fuel.price = ifelse(fuel_1_general=='geothermal' | fuel_1_general== 'wind' | fuel_1_general=='solar' | 
                                   fuel_1_general=='water', 0, adj.fuel.price)) %>%
  filter(overnight_category != "steam turbine" | fuel_1_general != "oil") %>%
  # We don't have heat rate data for steam turbine plants that use oil
  filter(overnight_category != "steam turbine" | fuel_1_general != "natural gas") %>%
  # We don't have cost data for steam turbine plants that use natural gas (beyond 2000 they aren't a category in the AEO)
  mutate(adj.overnight = base.overnight/capacity.factor.avg) %>%
  mutate(choice = paste(overnight_category, fuel_1_general, sep = " ")) %>%
  filter(choice != 'igcc coal') %>%
  filter(choice != 'nuclear uranium') %>%
  # Neither of these alternatives is ever selected in the mix, which seems to cause issues in the mlogit estimation
  arrange(overnight_category, fuel_1_general, year) %>%
  select(year, choice, overnight_category, fuel_1_general, capacity.factor.avg, capacity_mw, adj.fuel.price, variable.o.m, fixed.o.m, adj.overnight)

gz1 = gzfile(paste0(path_data,"data.final.txt.gz"), "w")
write.table(data.final, file = gz1, sep="\t", col.names = TRUE, row.names = FALSE)
close(gz1) 

categories = unique(data.final[, c('overnight_category', 'fuel_1_general')]) %>%
  mutate(choice = paste(overnight_category, fuel_1_general, sep=" ")) %>%
  mutate(year = NA) %>%
  mutate(id = NA) %>%
  mutate(year = NA) %>%
  mutate(decision = FALSE)

my_func = function(x, y, z){
  a = categories[!categories$choice %in% x, ]
  a['id'] = z
  a['year'] = y
  return(data.frame(a))
}
# This function adds all of the choice alternatives to the dataset (not just the option that was selected)

capacity_1mw = data.final %>%
  select(year,overnight_category, fuel_1_general, choice, capacity_mw) %>%
  slice(rep(1:n(), round(capacity_mw))) %>%
  mutate(id = seq(from=1, to = nrow(.), by = 1)) %>%
  mutate(decision = TRUE)

df = capacity_1mw %>%
  group_by(id) %>%
  do(my_func(x = .$choice, y = .$year, z = .$id)) %>%
  ungroup() %>%
  bind_rows(capacity_1mw) %>%
  select(overnight_category, fuel_1_general, choice, year, id, decision) %>%
  left_join(data.final, by = c('overnight_category', 'fuel_1_general', 'choice', 'year')) %>%
  arrange(id, choice)

gz1 = gzfile(paste0(path_data,"data.expanded.txt.gz"), "w")
write.table(df, file = gz1, sep="\t", col.names = TRUE, row.names = FALSE)
close(gz1) 

df = read.table(paste0(path_data, "data.expanded.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)


test = df %>%
  filter(choice != 'igcc coal') %>%
  filter(choice != 'nuclear uranium') %>%
  arrange(id, choice)

test.1 = mlogit.data(df, choice = 'decision', shape = 'long', chid.var = 'id',
                alt.levels = c("coal coal", "conventional combined cycle natural gas", "conventional combined cycle oil",
                               "conventional combustion turbine natural gas", "conventional combustion turbine oil",         
                               "geothermal geothermal", "hydro water", "photovoltaic solar",
                               "solar thermal solar", "wind wind"))
test.1 = mlogit.data(test, choice = 'decision', shape = 'long', alt.var = 'choice')
f = mFormula(decision ~ adj.overnight)
#View(model.matrix(f, test.1))

f.1 = mlogit(f, test.1, shape='long', alt.var='choice')
f.1 = mlogit(decision ~ adj.overnight | id, data = test.1)

summary(mlogit(decision ~ 0 | adj.overnight, data = test.1))

test = df %>%
  filter(year == 2014) %>%
  mutate(new = paste(id, choice, sep='.')) 

test$id = as.character(test$id)
test$choice = as.character(test$choice)

data = mlogit.data(test, choice = 'decision', shape = 'wide', id.var = c('id','year'), alt.var = 'choice')
data = data %>%
  arrange(fuel_1_general)

ml.capacity = mlogit(decision ~ adj.fuel.price + variable.o.m + fixed.o.m + adj.overnight | 0, df)

nl.capacity = mlogit(decision ~ adj.fuel.price + variable.o.m + fixed.o.m + adj.overnight, data, panel = TRUE,
                     nests = list(coal= c('coal coal', 'igcc coal'), oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                                  ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                                  geothermal = 'geothermal geothermal', nuclear = 'nuclear uranium', solar = c('solar thermal solar', 'photovoltaic solar'),
                                  hydro = 'hydro water', wind = 'wind wind'), un.nest.el=TRUE)
