overnight = read.table(paste0(path_data, "overnight.cost.longform.txt.gz"), header=TRUE, sep ="\t")

overnight = overnight %>%
  mutate(tech_abbrev = as.character(tech_abbrev)) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="fuell cell", "fuel cells")) %>%
  # Spelling in 1997 is off
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="fuel cell", "fuel cells")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="cc", "conventional combined cycle")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="ct", "conventional combustion turbine")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="conv gas cc", "conventional combined cycle")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="conv ct", "conventional combustion turbine")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="adv nuc", "nuclear")) %>%
  mutate(tech_abbrev = replace(tech_abbrev, tech_abbrev=="steam", "steam turbine")) %>%
  filter(tech_abbrev == "coal" | tech_abbrev == "steam turbine" | tech_abbrev == "conventional combined cycle" |
           tech_abbrev == "conventional combustion turbine" | tech_abbrev == "fuel cell" | tech_abbrev == "biomass" |
           tech_abbrev == "geothermal" | tech_abbrev == "solar thermal" | tech_abbrev == "photovoltaic" | tech_abbrev == "wind" |
           tech_abbrev == "igcc" | tech_abbrev == "hydro" | tech_abbrev == "nuclear")
  # keep the overnight categories in our model framework

ggplot(overnight) +
  geom_point(aes(x = aeo_year, y = overnight_nth), size = 1.5) +
  facet_wrap(~tech_abbrev, ncol = 3) +
  ylim(0, 8000) +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
 
gz1 = gzfile(paste(path_data,"overnight.cost.longform.txt.gz", sep=""), "w")
write.table(overnight, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1) 
