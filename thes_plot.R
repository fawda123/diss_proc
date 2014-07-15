# plot theses (masters) data
rm(list = ls())

# load data
load('thes_parse.RData')

# for correct ordering by median given limits on plot
thes_parse <- thes_parse[thes_parse$Pages <= 250, ]

# table of number of theses per major, minimum of five
pop.maj <- sort(table(thes_parse$Major), decreasing = T)
pop.maj <- data.frame(num_thes = pop.maj)
pop.maj$Major <- row.names(pop.maj)

# subset data by majors with at least five
# add number of theses to major column
to_plo <- thes_parse
to_plo <- merge(to_plo, pop.maj, by = 'Major', all.x = T)
to_plo$Major <- paste0(to_plo$Major, ' (', to_plo$num_thes, ')')

# median page length by thesis
med_pgs <- ddply(to_plo,
                 .(Major), 
                 summarize, 
                 median = median(Pages)
)
med_pgs <- med_pgs[order(med_pgs$median),]

# reorder major as factor by median page length
to_plo$Major <- factor(to_plo$Major, labels = med_pgs$Major, 
                       levels = med_pgs$Major)

# merge with median pages for fill
to_plo <- merge(to_plo, med_pgs, by = 'Major', all.x = T)
to_plo <- merge(to_plo, pop.maj, by = 'Major', all.x = T)

# boxplots of pages by major, collored by median, pdf
pdf('thes_plo.pdf',width = 11,height = 10, family = 'serif')
thes_plo <- ggplot(to_plo, aes(x = Major, y = Pages, fill = median)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,250) + 
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(thes_plo)
dev.off()

# boxplots of pages by major, collored by median, jpeg
jpeg('thes_plo.jpg',width = 11,height = 10, family = 'serif', units = 'in',
     res = 600)
thes_plo <- ggplot(to_plo, aes(x = Major, y = Pages, fill = median)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,250) + 
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(thes_plo)
dev.off()

######
# barplots, number of theses by major
to_plo <- pop.maj
to_plo$Major <- factor(pop.maj$Major, labels = pop.maj$Major, 
                       levels = pop.maj$Major)
# pdf
pdf('thes_counts.pdf',width = 12,height = 6, family = 'serif')
thes_plo <- ggplot(to_plo, aes(x = Major, y = num_thes, fill = num_thes)) + 
  geom_bar(lwd = 0.3, stat = 'identity') + 
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  ylab('Number of theses in record') +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, 
                                   vjust = 0))
print(thes_plo)
dev.off()

#jpg
jpeg('thes_counts.jpg', width = 12, height = 6, family = 'serif', units = 'in',
     res = 600)
thes_plo <- ggplot(to_plo, aes(x = Major, y = num_thes, fill = num_thes)) + 
  geom_bar(lwd = 0.3, stat = 'identity') + 
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  ylab('Number of theses in record') +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, 
                                   vjust = 0))
print(thes_plo)
dev.off()
