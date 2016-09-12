my_func = function(x, y, z){
  a = categories[!categories$choice %in% x, ]
  a['id'] = z
  a['year'] = y
  return(data.frame(a))
}
# This function adds all of the choice alternatives to the dataset (not just the option that was selected)

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
  arrange(overnight_category, fuel_1_general, year) %>%
  select(year, choice, overnight_category, fuel_1_general, capacity.factor.avg, capacity_mw, adj.fuel.price, variable.o.m, fixed.o.m, adj.overnight)
  
gz1 = gzfile(paste0(path_data,"data.final.txt.gz"), "w")
write.table(data.final, file = gz1, sep="\t", col.names = TRUE, row.names = FALSE)
close(gz1) 

categories = unique(data.final[, c('overnight_category', 'fuel_1_general')]) %>%
  mutate(choice = paste(categories$overnight_category,categories$fuel_1_general, sep=" ")) %>%
  mutate(year = NA) %>%
  mutate(id = NA) %>%
  mutate(year = NA) %>%
  mutate(decision = FALSE)

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
  left_join(data.final, by = c('overnight_category', 'fuel_1_general', 'choice', 'year'))
  



data = mlogit.data(data.final, shape = 'long', choice = 'choice', varying = 8:11, alt.levels = c(1,2), id = "year")
capacity = mlogit.data(data.final, shape = 'long', varying = 8:11, choice = 'choice', 
                       alt.var = c('adj.overnight','adj.fuel.price','variable.o.m','fixed.o.m'),
                       alt.levels = c('coal coal','conventional combined cycle natural gas',"conventional combined cycle oil","conventional combustion turbine natural gas",
                                      "conventional combustion turbine oil","geothermal geothermal","hydro water","igcc coal","nuclear uranium","photovoltaic solar",
                                      "solar thermal solar","wind wind"))




library("mlogit")
data("HC", package = "mlogit")
HC <- mlogit.data(HC, varying = c(2:8, 10:16), choice = "depvar", shape = "wide")
cooling.modes <- index(HC)$alt %in% c(
room.modes <- index(HC)$alt %in% c(
  '
  erc
  '
  ,
  '
  er
  '
)
R> # installation / operating costs for cooling are constants,
  R> # only relevant for mixed systems
  R> HC$icca[!cooling.modes] <- 0
R> HC$occa[!cooling.modes] <- 0
R> # create income variables for two sets cooling and rooms
  R> HC$inc.cooling <- HC$inc.room <- 0
R> HC$inc.cooling[cooling.modes] <- HC$income[cooling.modes]
R> HC$inc.room[room.modes] <- HC$income[room.modes]
R> # create an intercet for cooling modes
  R> HC$int.cooling <- as.numeric(cooling.modes)
R> # estimate the model with only one nest elasticity
  R> nl <- mlogit(depvar ~ ich + och +icca + occa + inc.room + inc.cooling + int.cooling | 0, HC,
                  +              nests = list(cooling = c(
                    '
                    gcc
                    '
                    ,
                    '
                    ecc
                    '
                    ,
                    '
                    erc
                    '
                    ,
                    '
                    hpc
                    '
                  ),
                  +              other = c(
                    '
                    gc
                    '
                    ,
                    '
                    ec
                    '
                    ,
                    '
                    er
                    '
                  )), un.nest.el = TRUE)


mutate(id.choice = NA) %>%
  mutate(id.choice = replace(id.choice, choice == "coal coal", 1)) %>%
  mutate(id.choice = replace(id.choice, choice == "conventional combined cycle natural gas", 2)) %>%
  mutate(id.choice = replace(id.choice, choice == "conventional combined cycle oil", 3)) %>%
  mutate(id.choice = replace(id.choice, choice == "conventional combustion turbine natural gas", 4)) %>%
  mutate(id.choice = replace(id.choice, choice == "conventional combustion turbine oil", 5)) %>%
  mutate(id.choice = replace(id.choice, choice == "geothermal geothermal", 6)) %>%
  mutate(id.choice = replace(id.choice, choice == "hydro water", 7)) %>%
  mutate(id.choice = replace(id.choice, choice == "igcc coal", 8)) %>%
  mutate(id.choice = replace(id.choice, choice == "nuclear uranium", 9)) %>%
  mutate(id.choice = replace(id.choice, choice == "photovoltaic solar", 10)) %>%
  mutate(id.choice = replace(id.choice, choice == "solar thermal solar", 11)) %>%
  mutate(id.choice = replace(id.choice, choice == "wind wind", 12)) %>%