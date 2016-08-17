summary.cap.eia.year = read.table(paste0(path_data, "summary_files.txt.gz"), header=TRUE, sep ="\t")

p1 = ggplot(summary.cap.eia.year) +
  geom_point(aes(x = year, y = heat_rate), size = 0.75, colour = 'blue') +
  geom_line(aes(x = year, y = pred.heat.rate)) +
  facet_wrap(~overnight_category + fuel_1_general) +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size = 4), axis.title = element_text(size = 4),
        strip.text.x = element_text(size = 4))

p2 = ggplot(summary.cap.eia.year) +
  geom_point(aes(x = year, y = n), size = 0.75, colour = 'blue') +
  facet_wrap(~overnight_category + fuel_1_general) +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size = 4), axis.title = element_text(size = 4),
        strip.text.x = element_text(size = 4))

p3 = ggplot(summary.cap.eia.year) +
  geom_point(aes(x = year, y = avg_age), size = 0.75, colour = 'blue') +
  facet_wrap(~overnight_category + fuel_1_general) +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size = 4), axis.title = element_text(size = 4),
        strip.text.x = element_text(size = 4))

p4 = ggplot(summary.cap.eia.year) +
  geom_point(aes(x = year, y = capacity_mw), size = 0.75, colour = 'blue') +
  facet_wrap(~overnight_category + fuel_1_general) +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size = 4), axis.title = element_text(size = 4),
        strip.text.x = element_text(size = 4))

pdf(paste0(path_figure, "summary_files_p4.pdf"), width=8.5,height=11) 
p4
dev.off()
