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
  mutate(vc = adj.fuel.price + variable.o.m + fixed.o.m) %>%
  arrange(overnight_category, fuel_1_general, year) %>%
  select(choice_id, year, choice, overnight_category, fuel_1_general, fossil.use, coal.use, ng.use, oil.use, renewable.use, 
         capacity.factor.avg, capacity_mw, adj.fuel.price, variable.o.m, fixed.o.m, adj.overnight, vc, cost)

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
  mutate(decision = FALSE)

categories.iv = data.frame(fuel_1_general = c(unique(data.final[, 'fuel_1_general']))) %>%
  mutate(year = NA) %>%
  mutate(id = NA) %>%
  mutate(decision = FALSE)

my_func = function(x, y, z){
  a = categories[!categories$choice %in% x, ]
  a['id'] = z
  a['year'] = y
  return(data.frame(a))
}
my_func.iv = function(x, y, z){
  a = categories.iv[!categories.iv$fuel_1_general %in% x, ]
  a['id'] = z
  a['year'] = y
  return(data.frame(a))
}
# These functions adds all of the choice alternatives in the first stage regression (and fuel alternatives in the second stage) to the dataset (not just the option that was selected)
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
# Random lessons learned in using mlogit:
# We can only include alternatives that are selected at least once.
# We don't have a panel dataset (we have a repeated cross-section), which means that we don't use the 'id.var' parameter
# If done correctly, in the non-panel data the row names
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
                        alt.levels = c('coal coal', 'conventional combined cycle natural gas', 'conventional combined cycle oil',
                                       'conventional combustion turbine natural gas', 'conventional combustion turbine oil', 
                                       'geothermal geothermal', 'photovoltaic solar','solar thermal solar', 'wind wind'))
# If done correctly, in the non-panel data the row names will be the paste0(choice #, choice alternative, sep='.') 
# Even if a nest has one alternative you must use c()
# We set the reference level as the renewables 
# If trying to get the nuclear data below we need to remember to add 'nuclear uranium' to alt.levels in df.mlogit above

# nuclear = df.mlogit[df.mlogit$choice=="nuclear uranium", ]
# I ran this one time through to get the nuclear data in the right format. Otherwise, we screen out nuclear in the data.final dataset above (it's never selected)
# gz1 = gzfile(paste0(path_data,"nuclear.data.txt.gz"), "w")
# write.table(nuclear, file = gz1, sep="\t", col.names = TRUE, row.names = TRUE)
# close(gz1)
# We dropped nuclear from the multinomial model fitting (because it has not been built after the 1970s and thus given the length of record of our data 
#   it wouldn't be selected by the model. But we can use the estimated model and the cost data to guess how much it would be built if the choice of nuclear
#   only depended on cost factors. We do this below.)

formula.0.mlogit = mFormula(decision ~ cost|1)
# No nests equation
formula.1.mlogit = mFormula(decision ~ adj.overnight + fixed.o.m  + adj.fuel.price + variable.o.m | 1 | 0)
# Includes a constant for each production technology
formula.nl.1.mlogit = mFormula(decision ~ adj.overnight + fixed.o.m + adj.fuel.price + variable.o.m  | 0)
formula.nl.1.mlogit = mFormula(decision ~ adj.overnight + fixed.o.m + adj.fuel.price + variable.o.m  | 1 | 0)
# Disaggregated cost factors
formula.nl.3.mlogit = mFormula(decision ~ cost | 0)
# Could have just used formula.0.mlogit
formula.nl.3b.mlogit = mFormula(decision ~ cost | 1)
# GCAM model
formula.nl.4.mlogit = mFormula(decision ~ adj.overnight + vc | 0)
# We sum together the variable costs separate from the investment costs
# unless we estimate a model with intercepts we don't need to specify a 'reflevel'

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
                unscaled = TRUE, un.nest.el = FALSE)
# Nesting structure akin to GCAM. Note: The coal branch is degenerate (i.e., has only one alternative), which we address through the unscaled parameter.

#### Disaggreated cost components
f.nl.4 = mlogit(formula.nl.1.mlogit, shape='long', alt.var='choice', df.mlogit,
                nests = list(coal = c('coal coal'), 
                             ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                             oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                             solar = c('solar thermal solar', 'photovoltaic solar'),
                             geothermal = c('geothermal geothermal'),
                             wind = c('wind wind')), 
                unscaled = TRUE, un.nest.el = FALSE)
f.nl.5.a = mlogit(formula.nl.4.mlogit, shape='long', alt.var='choice', df.mlogit,
                                   nests = list(coal = c('coal coal'), 
                                                ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                                                oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                                                solar = c('solar thermal solar', 'photovoltaic solar'),
                                                geothermal = c('geothermal geothermal'),
                                                wind = c('wind wind')), 
                                   unscaled = TRUE, un.nest.el = FALSE)
f.nl.6.a = mlogit(formula.nl.3b.mlogit, shape='long', alt.var='choice', df.mlogit,
                  nests = list(coal = c('coal coal'), 
                               ng = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'),
                               oil = c('conventional combustion turbine oil', 'conventional combined cycle oil'),
                               solar = c('solar thermal solar', 'photovoltaic solar'),
                               geothermal = c('geothermal geothermal'),
                               wind = c('wind wind')), 
                  unscaled = TRUE, un.nest.el = TRUE)
# This is the unrestricted model (a log-sum coefficient for each nest)
f.nl.5.b = update(f.nl.5.a, un.nest.el=TRUE)
# This is the restricted model (one log-sum coefficient)
lrtest(f.nl.5.b, f.nl.5.a)
# We reject the null hypothesis that the two nests have the same log-sum coefficient based on a c (i.e., we should use the unrestricted model)

round(apply(fitted(f.nl.5.a, outcome = FALSE), 2, mean)*100, 1)
# Predicted market share (Model 3)
round(apply(fitted(f.nl.4, outcome = FALSE), 2, mean)*100, 1)
# Predicted market share (Model 2)
round(apply(fitted(f.nl.3, outcome = FALSE), 2, mean)*100, 1)
# Predicted market share (Model 1 - GCAM parameterization)


# 1. A nested logit model is a conditional logit model at the twig level, so we first estimate the conditional logit model for the alternatives in the nest where the new option resides
f.1 = mlogit(formula.nl.4.mlogit, shape='long', alt.var='choice', df.mlogit, 
             alt.subset = c('conventional combined cycle natural gas', 'conventional combustion turbine natural gas'))

X = model.matrix(f.nl.5.a)
chid = index(df.mlogit)$chid
Xn = X[grep('coal', rownames(X)), ]
Xn[, 'vc'] = Xn[, "vc"]*1.76
unchid = unique(index(df.mlogit)$chid)
rownames(Xn) = paste(unchid, 'new', sep = '.')
chidb = c(chid, unchid)
X = rbind(X, Xn)
X = X[order(chidb), ]
eXb = as.numeric(exp(X %*% coef(f.nl.5.a)[1:2]))
SeXb = as.numeric(tapply(eXb, sort(chidb), sum))
P = eXb/SeXb[sort(chidb)]
P = matrix(P, ncol = 10, byrow = TRUE)
apply(P, 2, mean)



nuclear = read.table(paste0(path_data, "nuclear.data.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)
nuclear = nuclear[, c('adj.overnight', 'vc')]
X = rbind(X, nuclear)
X = X[order(row.names(X)), ]
# Predict how much nuclear would be in the market were it not for regulatory or other hurdles

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

pred.shares = apply(predict(f.nl.4, newdata=cv.df.test), 2, mean)

cv.test = fitted(f.0, newData=cv.df.mlogit)
apply(predict(f.0, newdata=cv.df.mlogit), 2, mean)




# The code below estimates the nested logit using the definition of conditional probability
f.1 = mlogit(decision ~ 1 + cost, shape='long', alt.var='choice', df.mlogit, reflevel="conventional combined cycle natural gas")
f.2 = mlogit(decision ~ 1 + adj.overnight + vc, shape='long', alt.var='choice', df.mlogit, reflevel="conventional combined cycle natural gas")
# First we estimate the 1st stage regression coefficients, we allow for alternative specific constants but enforce the condition that the slope for the cost is constant across
# alternatives. 
f.1.coef = data.frame(coefficients = c(coef(f.1)[1:8])) %>%
  mutate(choice = rownames(f.1.coef)) %>% 
  mutate(choice = as.factor(gsub(":.*","", x$choice, perl=TRUE)))
model.pred = df.mlogit %>%
  left_join(f.1.coef, by = 'choice')  %>%
  mutate(coefficients = ifelse(is.na(test$coefficients), 1, test$coefficients)) %>%
  mutate(y.1 = exp(test$coefficients + test$cost*coef(f.1)['cost'])) %>%
  group_by(fuel_1_general, id) %>%
  mutate(y.tot = sum(y.1)) %>%
  ungroup() %>%
  mutate(perc.1 = y.1/y.tot)
iv.mlogit.1 = df.mlogit %>%
  filter(decision == TRUE) %>%
  select(fuel_1_general, id, decision, year)
fuel.iv = model.pred %>%
  filter(decision == TRUE) %>%
  group_by(year, fuel_1_general) %>%
  summarize(iv = log(sum(y.1)))
iv.mlogit = iv.mlogit.1 %>%
  group_by(id) %>%
  do(my_func.iv(x = .$fuel_1_general, y = .$year, z = .$id)) %>%
  left_join(fuel.iv, by = c('year', 'fuel_1_general')) %>%
  ungroup() %>%
  bind_rows(iv.mlogit.1) %>%
  mutate(iv = ifelse(is.na(iv), 0, iv)) %>%
  arrange(id, fuel_1_general)
iv.data = mlogit.data(iv.mlogit, choice = 'decision', 
                        shape = 'long', alt.var = "fuel_1_general")
f.2 = mlogit(decision ~ 1 + iv, shape='long', alt.var='fuel_1_general', iv.data, reflevel="natural gas")
f.2.coef = data.frame("fuel_1_general" = c("coal", "natural gas", "oil", "geothermal", "solar", "wind"), 
                 "coefficient.2" = c(coef(f.2)[1], 0, coef(f.2)[3], coef(f.2)[2], coef(f.2)[4], coef(f.2)[5]),
                 "iv.coef" = c(coef(f.2)[6], coef(f.2)[6], coef(f.2)[6], coef(f.2)[6], coef(f.2)[6], coef(f.2)[6]))
y.2 = data.frame(predict(f.2, newdata = iv.data, outcome = FALSE)) %>%
  mutate(id = as.integer(rownames(.)))

model.pred = model.pred %>%
  left_join(iv.mlogit, by = c("id", "fuel_1_general", "year", "decision")) %>%
  filter(decision == TRUE) %>%
  left_join(y.2, by = c("id")) %>%
  mutate(y.2 = exp(coefficient.2 + iv.coef*iv)) %>%
  group_by(id) %>%
  mutate(y.2.tot = sum(y.2)) %>%
  ungroup() %>%
  mutate(perc.2 = y.2/y.2.tot) %>%
  mutate(overall.perc = perc.1*perc.2) %>%
  group_by(choice) %>%
  filter(decision == TRUE) %>%
  summarize(test = mean(perc.2, na.rm=TRUE))



f.2 = mlogit(decision ~ fuel_1_general + iv, shape='long', alt.var='choice', df.mlogit)
stage.1 = stage.1 %>%
  left_join(x.1, by=c('fuel_1_general')) %>%
  y.2 = exp(coefficient.2 + iv.coef*iv)
  

y_2 = as.data.frame(apply(predict(f.2, newdata = iv.data, outcome = FALSE), 2, mean))

stage.1 = model.pred %>%
  group_by(fuel_1_general, id) %>%
  mutate(y.tot = sum(y_1)) %>%
  ungroup() %>%
  mutate(perc.1 = y_1/y.tot)


y = predict(f.1, newdata = df.mlogit, outcome = FALSE)

df = capacity_1mw %>%
  
  ungroup() %>%
  bind_rows(capacity_1mw) %>%
  select(overnight_category, fuel_1_general, choice, year, id, decision) %>%
  left_join(data.final, by = c('overnight_category', 'fuel_1_general', 'choice', 'year')) %>%
  arrange(id, choice)
