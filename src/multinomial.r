data.raw = read.table(paste0(path_data, "processed.overnight.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)

data.final = data.raw %>%
  select(overnight_category, fuel_1_general, year, capacity.factor.avg, capacity_mw, n, pred.heat.rate, adj.fuel.price, base.overnight, variable.o.m, fixed.o.m) %>%
  filter(year >= 2001) %>%
  # We don't have overnight cost data for the period prior to this (we have 1997 but are missing 1998). Mlogit has issues with missing data. 
  #   To incorporate any of this data we would need to impute missing values during the period. 
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
  filter(overnight_category != 'fuel cell' & overnight_category != 'hydro') %>%
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
  mutate(choice_id = seq(from=1,to=nrow(.), by = 1)) %>%
  mutate(choice = as.factor(choice)) %>%
  mutate(year = as.factor(as.character(year))) %>%
  mutate(fossil.use = ifelse(fuel_1_general == 'coal' | fuel_1_general == 'oil' | fuel_1_general == 'ng', 1, 0)) %>%
  mutate(coal.use = ifelse(fuel_1_general=='coal', 1, 0)) %>%
  mutate(ng.use = ifelse(fuel_1_general=='natural gas', 1, 0)) %>%
  mutate(oil.use = ifelse(fuel_1_general=='oil', 1, 0)) %>%
  mutate(renewable.use = ifelse(fuel_1_general=='solar' | fuel_1_general=='wind' | fuel_1_general == 'geothermal', 1, 0)) %>%
  mutate(fossil.use = as.factor(fossil.use)) %>%
  mutate(coal.use = as.factor(coal.use)) %>%
  mutate(ng.use = as.factor(ng.use)) %>%
  mutate(oil.use = as.factor(oil.use)) %>%
  mutate(renewable.use = as.factor(renewable.use)) %>%
  mutate(cost = adj.fuel.price + variable.o.m + fixed.o.m + adj.overnight) %>%
  arrange(overnight_category, fuel_1_general, year) %>%
  select(choice_id, year, choice, overnight_category, fuel_1_general, fossil.use, coal.use, ng.use, oil.use, renewable.use, 
         capacity.factor.avg, capacity_mw, adj.fuel.price, variable.o.m, fixed.o.m, adj.overnight, cost)

gz1 = gzfile(paste0(path_data,"data.final.txt.gz"), "w")
write.table(data.final, file = gz1, sep="\t", col.names = TRUE, row.names = FALSE)
close(gz1) 

summary.stats.0 = data.final %>%
  group_by(choice) %>%
  summarize(mw = sum(capacity_mw)) %>%
  ungroup() %>%
  mutate(proporation = round((mw/sum(mw))*100, 1))

summary.stats.1 = data.final %>%
  group_by(fuel_1_general, year) %>%
  summarize(capacity = sum(capacity_mw))

summary.stats.2 = data.final %>%
  group_by(overnight_category, year) %>%
  summarize(capacity = sum(capacity_mw))

p1 = ggplot(summary.stats.1, aes(year, capacity, fill= fuel_1_general)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14), legend.position = c(0.75,0.75), legend.background = element_rect(colour = NA), 
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Primary fuel")) +
  labs(x = "Year", y = "Capacity additions (MW)")

dev.off()
pdf(paste0(path_figure, "summary_stats_fuel.pdf"), width=8.5,height=11) 
p1
dev.off()

p1 = ggplot(summary.stats.2, aes(year, capacity, fill= overnight_category)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14), legend.position = c(0.75,0.75), legend.background = element_rect(colour = NA), 
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Production technology")) +
  labs(x = "Year", y = "Capacity additions (MW)")

dev.off()
pdf(paste0(path_figure, "summary_stats_tech.pdf"), width=8.5,height=11) 
p1
dev.off()

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
  select(year, overnight_category, fuel_1_general, choice, capacity_mw) %>%
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

# Model estimation
df = read.table(paste0(path_data, "data.expanded.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)
df = df %>%
  mutate(overnight_category = as.factor(overnight_category)) %>%
  mutate(fuel_1_general = as.factor(fuel_1_general)) %>%
  mutate(choice = as.factor(choice)) %>%
  mutate(year = as.factor(as.character(year))) %>%
  mutate(fossil.use = as.factor(fossil.use)) %>%
  mutate(coal.use = as.factor(coal.use)) %>%
  mutate(ng.use = as.factor(ng.use)) %>%
  mutate(oil.use = as.factor(oil.use)) %>%
  mutate(renewable.use = as.factor(renewable.use))
# All categorical variables need to be set as factors

df.mlogit = mlogit.data(df, choice = 'decision', 
                        shape = 'long', 
                        alt.levels = c('coal coal', 'conventional combined cycle natural gas', 'conventional combustion turbine oil',
                                       'conventional combined cycle oil', 'conventional combustion turbine natural gas', 'geothermal geothermal', 
                                       'wind wind','solar thermal solar', 'photovoltaic solar'))
# Random lessons learned in using mlogit:
# We don't have a panel dataset (we have a repeated cross-section), which means that we don't use the 'id.var' parameter
# If done correctly, in the non-panel data the row names will be the paste0(choice #, choice alternative, sep='.') 
# Even if a nest has one alternative you must use c()
# We set the reference level as the renewables 


formula.0.mlogit = mFormula(decision ~ cost|0)
# No nests equation
formula.1.mlogit = mFormula(decision ~ adj.overnight + fixed.o.m  + adj.fuel.price + variable.o.m | 1 | 0)
# Includes a constant for each production technology
formula.nl.1.mlogit = mFormula(decision ~ fixed.o.m + adj.fuel.price + variable.o.m + adj.overnight | 0)
# Disaggregated cost factors
formula.nl.3.mlogit = mFormula(decision ~ cost | 0)
# Could have just used formula.0.mlogit

#### IIA testing 
f.0 = mlogit(formula.0.mlogit, df.mlogit, shape='long', alt.var='choice')
# No nests
iia.test = mlogit(formula.0.mlogit, df.mlogit, shape='long', alt.var='choice', 
                  alt.subset = c('conventional combined cycle natural gas', 'conventional combustion turbine oil', 
                                 'conventional combined cycle oil', 'conventional combustion turbine natural gas',
                                 'geothermal geothermal'))
# Same model but with a subset of alternatives (in preparation for the Hausman consistency test)
hmftest(z=iia.test, x=f.0)
# From the Hausman-McFadden test we can say that the IIA assumption is rejected
####

f.nl.0.a = mlogit(formula.nl.1.mlogit, shape='long', alt.var='choice', df.mlogit,
             nests = list(fossil= c('coal coal', 'conventional combined cycle natural gas', 'conventional combustion turbine oil', 
                                    'conventional combined cycle oil', 'conventional combustion turbine natural gas'),
                          renewables = c('geothermal geothermal', 'wind wind','solar thermal solar', 'photovoltaic solar')))
f.nl.0.b = mlogit(formula.nl.4.mlogit, shape='long', alt.var='choice', df.mlogit,
                nests = list(fossil= c('coal coal', 'conventional combined cycle natural gas', 'conventional combustion turbine oil', 
                                       'conventional combined cycle oil', 'conventional combustion turbine natural gas'),
                             renewables = c('geothermal geothermal', 'wind wind','solar thermal solar', 'photovoltaic solar')))
# Fossil vs. renewable fuels nest structure
f.nl.1 = mlogit(formula.nl.2.mlogit, shape='long', alt.var='choice', df.mlogit,
             nests = list(coal = c('coal coal'), 
                          ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                          oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                          renewables = c('geothermal geothermal', 'wind wind','solar thermal solar', 'photovoltaic solar')), 
             unscaled = TRUE)
f.nl.2 = mlogit(formula.nl.3.mlogit, shape='long', alt.var='choice', df.mlogit,
                nests = list(coal = c('coal coal'), 
                             ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                             oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                             renewables = c('geothermal geothermal', 'wind wind','solar thermal solar', 'photovoltaic solar')), 
                unscaled = TRUE)



#### GCAM structure 
f.nl.3 = mlogit(formula.nl.3.mlogit, shape='long', alt.var='choice', df.mlogit,
                nests = list(coal = c('coal coal'), 
                             ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                             oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                             solar = c('solar thermal solar', 'photovoltaic solar'),
                             geothermal = c('geothermal geothermal'),
                             wind = c('wind wind')), 
                unscaled = TRUE)
# Nesting structure akin to GCAM. Note: The coal branch is degenerate (i.e., has only one alternative), which we address through the unscaled parameter.

#### Disaggreated cost components
f.nl.4 = mlogit(formula.nl.1.mlogit, shape='long', alt.var='choice', df.mlogit,
                nests = list(coal = c('coal coal'), 
                             ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                             oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                             solar = c('solar thermal solar', 'photovoltaic solar'),
                             geothermal = c('geothermal geothermal'),
                             wind = c('wind wind')), 
                unscaled = TRUE)

# un.nest.el = TRUE means that we are forcing the hypothesis of a unique elasticity for nested logit (each nest)

x = unique(data.final$year)
z = sample(1:length(x), 7, replace=TRUE)
cv.df.fit = rbind(df.mlogit[which(df.mlogit$year == x[z[1]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[2]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[3]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[4]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[5]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[6]]), ],
                     df.mlogit[which(df.mlogit$year == x[z[7]]), ])
y = sample(1:length(x), 7, replace=TRUE)
cv.df.test = rbind(df.mlogit[which(df.mlogit$year == x[y[1]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[2]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[3]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[4]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[5]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[6]]), ],
                  df.mlogit[which(df.mlogit$year == x[y[7]]), ])

f.nl.1 = mlogit(formula.nl.2.mlogit, shape='long', alt.var='choice', cv.df.fit,
                nests = list(coal = 'coal coal', 
                             ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                             oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                             renewables = c('geothermal geothermal', 'wind wind','solar thermal solar', 'photovoltaic solar')), unscaled = TRUE)

stats = cv.df.test %>%
  group_by(choice) %>% 
  summarize(mw = sum(decision == TRUE)) %>%
  ungroup() %>%
  mutate(perc = round(mw/sum(mw),2))

pred.shares = apply(predict(f.nl.1, newdata=cv.df.test), 2, mean)

cv.test = fitted(f.0, newData=cv.df.mlogit)
apply(predict(f.0, newdata=cv.df.mlogit), 2, mean)

