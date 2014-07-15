# plot dissertation (doctoral) data
rm(list = ls())

# load data
load('diss_parse.RData')

# for correct ordering by median given limits on plot
diss_parse <- diss_parse[diss_parse$Pages <= 500, ]

# table of number of dissertations per major, minimum of five
pop.maj <- sort(table(diss_parse$Major), decreasing = T)
pop.maj <- data.frame(num_diss = pop.maj)
pop.maj$Major <- row.names(pop.maj)

# subset data by majors with at least five
# add number of dissertations to major column
to_plo <- diss_parse
to_plo <- merge(to_plo, pop.maj, by = 'Major', all.x = T)
to_plo$Major <- paste0(to_plo$Major, ' (', to_plo$num_diss, ')')

# median page lengthy by dissertation
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

######
# boxplots of pages by major, collored by median
pdf('diss_plo.pdf',width = 11,height = 10, family = 'serif')
diss_plo <- ggplot(to_plo, aes(x = Major, y = Pages, fill = median)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,500) + 
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(diss_plo)
dev.off()

# boxplots of pages by major, collored by median
jpeg('diss_plo.jpg',width = 11,height = 10, family = 'serif', units = 'in', 
     res = 600)
diss_plo <- ggplot(to_plo, aes(x = Major, y = Pages, fill = median)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,500) + 
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(diss_plo)
dev.off()

######
# barplots, number of dissertations by major

to_plo <- pop.maj
to_plo$Major <- factor(pop.maj$Major, labels = pop.maj$Major, 
                        levels = pop.maj$Major)
# pdf
pdf('diss_counts.pdf',width = 12,height = 6, family = 'serif')
diss_plo <- ggplot(to_plo, aes(x = Major, y = num_diss, fill = num_diss)) + 
  geom_bar(lwd = 0.3, stat = 'identity') + 
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(7, 'Blues')) +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  ylab('Number of dissertations in record') +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, 
                                   vjust = 0))
print(diss_plo)
dev.off()

#jpg
jpeg('diss_counts.jpg', width = 12, height = 6, family = 'serif', units = 'in',
     res = 600)
diss_plo <- ggplot(to_plo, aes(x = Major, y = num_diss, fill = num_diss)) + 
  geom_bar(lwd = 0.3, stat = 'identity') + 
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  ylab('Number of dissertations in record') +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, 
                                   vjust = 0))
print(diss_plo)
dev.off()