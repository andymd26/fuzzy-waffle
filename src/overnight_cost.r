overnight = read.table(paste0(path_data, "overnight.cost.txt"), header=TRUE, sep ="\t", as.is = TRUE)
# wind = read.table(paste0(path_data, "overnight.cost.longform.txt.gz"), header=TRUE, sep ="\t")
# Wind data from Bollinger 2009 could be incorporated here to extend the data further back in time
overnight = overnight %>%
  mutate(variable.o.m = variable.o.m*.001)
  # The original units are Mills per kWh, which we convert to dollars per kWh. A mill is equal to 1/10 of a cent. 
Source: http://www.eia.gov/tools/faqs/faq.cfm?id=19&t=5

summary.cap.eia.year = read.table(paste0(path_data, "summary_files.txt.gz"), header=TRUE, sep ="\t", as.is = TRUE)
summary.cap.eia.year  = summary.cap.eia.year %>%
  filter(year != 2100 & year != 2050) %>%
  left_join(overnight, by = c("overnight_category", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)
  
gz1 = gzfile(paste0(path_data,"overnight.cost.longform.txt.gz"), "w")
write.table(overnight, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1) 

gz1 = gzfile(paste0(path_data,"processed.overnight.txt.gz"), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1) 