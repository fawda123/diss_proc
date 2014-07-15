# plots comparing thesis, dissertation data

# load data
load('thes_parse.RData')
load('diss_parse.RData')

# remove outliers
thes_parse <- thes_parse[thes_parse$Pages <= 250, ]
diss_parse <- diss_parse[diss_parse$Pages <= 500, ]

# combine data
thes <- data.frame(thes_parse, Type = 'Thesis')
diss <- data.frame(diss_parse, Type = 'Dissertation')
all_dat <- rbind(thes, diss)

######
# histogram all majors

# pdf
pdf('all_hist.pdf', width = 6, height = 6, family = 'serif')
all_hist <- ggplot(all_dat, aes(x = Pages, fill = Type)) +
  geom_histogram(binwidth = 10, alpha = 0.6) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 500)) +
  theme(legend.position = 'top') +
  ylab('Count') +
  xlab('Number of pages')
print(all_hist)
dev.off()

# jpeg
jpeg('all_hist.jpg', width = 6, height = 6, family = 'serif', units = 'in', 
     res = 600)
all_hist <- ggplot(all_dat, aes(x = Pages, fill = Type)) +
  geom_histogram(binwidth = 10, alpha = 0.6) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 500)) +
  theme(legend.position = 'top') +
  ylab('Count') +
  xlab('Number of pages')
print(all_hist)
dev.off()

######
# boxplots by major, dissertations and theses

# find majors that are common to both datasets
match_maj <- intersect(unique(thes$Major), unique(diss$Major))
all_dat <- all_dat[all_dat$Major %in% match_maj, ]

# median page lengthy by dissertation
med_pgs <- ddply(all_dat,
                 .(Major, Type), 
                 summarize, 
                 median = quantile(Pages, 0.5)
)
med_pgs <- med_pgs[order(med_pgs$media),]
med_pgs <- med_pgs[med_pgs$Type == 'Dissertation', ]
med_pgs <- med_pgs[order(med_pgs$median), ]

# reorder major as factor by median page length
all_dat$Major <- factor(all_dat$Major, labels = med_pgs$Major, 
                       levels = med_pgs$Major)

# pdf
pdf('all_plo.pdf', family = 'serif', width = 8, height = 7)
all_plo <- ggplot(all_dat, aes(x = Major, y = Pages, fill = Type)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,500) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(all_plo)
dev.off()

# jpg
jpeg('all_plo.jpg', family = 'serif', width = 8, height = 7, units = 'in',
     res = 600)
all_plo <- ggplot(all_dat, aes(x = Major, y = Pages, fill = Type)) + 
  geom_boxplot(lwd = 0.3) + 
  ylim(0,500) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
print(all_plo)
dev.off()

####
# some stats
mod <- lm(Pages ~ Type, data = all_dat)

mod_ran <- lme(fixed = Pages ~ Type, random = ~ 1|Major, data = all_dat)
