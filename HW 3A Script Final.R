library(tidyverse)
library(palmerpenguins)
library(rstatix)

#Question 1
adelie = penguins %>%
  filter(species == "Adelie")
head(adelie)
summary(adelie)

pdf('figures/adelie_body_mass.pdf', width=7, height=5)
ggplot(data=adelie) +
  geom_histogram(aes(x=body_mass_g))
dev.off()

mean(adelie$body_mass_g, na.rm=TRUE)
sd(adelie$body_mass_g, na.rm=TRUE)

#outlier test to check for any outliers
adelie %>% 
  identify_outliers(body_mass_g)

# Q-Q plot to check normality assumption
pdf('figures/adelie_qq.pdf', width=7, height=5)
ggplot(data=adelie) + 
  stat_qq(aes(sample=body_mass_g))
dev.off()

#Adelie t-test - used a one sample t-test
t.test(adelie$body_mass_g, mu=4700) # base r version
adelie %>%                          #dplyr version
  t_test(body_mass_g ~ 1, mu=4700)

#Effect size
adelie %>% cohens_d(body_mass_g ~ 1, mu = 4700)

# The Adelie body mass was significantly different from the literature, with a p-value of 5.06e-59 and an effect size of -2.18.

#Question 2
chinstrap = penguins %>%
  filter(species == "Chinstrap")
head(chinstrap)
summary(chinstrap)

pdf('figures/chinstrap_body_mass.pdf', width=7, height=5)
ggplot(data=chinstrap) +
  geom_histogram(aes(x=body_mass_g))
dev.off()

mean(chinstrap$body_mass_g, na.rm=TRUE)
sd(chinstrap$body_mass_g, na.rm=TRUE)

#outlier test to check for any outliers
chinstrap %>% 
  identify_outliers(body_mass_g)

# Q-Q plot to check normality assumption
pdf('figures/chinstrap_qq.pdf', width=7, height=5)
ggplot(data=chinstrap) + 
  stat_qq(aes(sample=body_mass_g))
dev.off()

#Chinstrap t-test - used a one sample t-test; body mass comes from: Sanz, J.J. (1995). Field Metabolic Rates of Breeding Chinstrap Penguins (Pygoscelis antarctica) in the South Shetlands. Physiological Zoology, 69(3), 586-598. 
t.test(chinstrap$body_mass_g, mu=3790) # base r version
chinstrap %>%                          #dplyr version
  t_test(body_mass_g ~ 1, mu=3790)

#Effect size
chinstrap %>% cohens_d(body_mass_g ~ 1, mu = 3790)

# The Chinstrap body mass was not significantly different from the literature, with a p-value of 0.226 and an effect size of -0.148.

#Question 3 - used an independent sample t-test

adelie_vs_chinstrap = penguins %>%
  filter(species %in% c("Chinstrap", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() 
glimpse(adelie_vs_chinstrap)

# summary stats
adelie_vs_chinstrap %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g), sd=sd(body_mass_g))

# histogram
pdf('figures/adelie_vs_chinstrap_bodymass.pdf', height = 7, width = 5)
ggplot(aes(x=body_mass_g), data=adelie_vs_chinstrap) + 
  geom_histogram() + 
  facet_wrap(~species)
dev.off()

# check outliers 
adelie_vs_chinstrap %>%
  group_by(species) %>%
  identify_outliers(body_mass_g)
#chinstrap has two outliers that are not extreme

# check normality - looks pretty good
pdf('figures/adelie_vs_chinstrap_qq.pdf', height = 7, width = 5)
ggplot(adelie_vs_chinstrap) + 
  stat_qq(aes(sample=body_mass_g)) + 
  facet_wrap(~species)
dev.off()

# check equality of variance - p = 0.0291 which is >0.05, so we can accept null and go ahead
adelie_vs_chinstrap %>% levene_test(body_mass_g ~ species)

# t-test
t.test(adelie_vs_chinstrap$body_mass_g ~ adelie_vs_chinstrap$species) # base R 

adelie_vs_chinstrap %>% 
  t_test(body_mass_g~species) # dplyr version

# p = 0.588; must accept the null hypothesis that the two masses are not significantly different

#effect size - -0.0766; negligible
adelie_vs_chinstrap %>% cohens_d(body_mass_g~species)

# Question 4: 
summary(penguins)
pdf('figures/flipper_vs_bodymass_all.pdf', height=7, width=5)
ggplot() + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g), data=penguins)
dev.off()
# figure shows what appears to be a strong positive correlation with no outliers

#check normality assumption
pdf('figures/flipper_qq_all.pdf', height=7, width=5)
ggplot(penguins) + 
  stat_qq(aes(sample=flipper_length_mm))
dev.off()
pdf('figures/bodymass_qq_all.pdf', height=7, width=5)
ggplot(penguins) +
  stat_qq(aes(sample=body_mass_g))
dev.off()
#both figures show good normality; most lie within -1 and 1

#check correlation
cor(x=penguins$flipper_length_mm, y=penguins$body_mass_g, use="complete.obs") # gives only coefficient r
cor.test(x=penguins$flipper_length_mm, y=penguins$body_mass_g, use="complete.obs") # gives t, df, p, r
# r = 0.87; high correlation!

#graph
library(GGally)
pdf('figures/flipper_vs_bodymass_matrix_all.pdf', height = 7, width = 5)
penguins %>%
  select(flipper_length_mm, body_mass_g) %>%
  ggpairs()
dev.off()

#Question 5:
pdf('figures/flipper_vs_bodymass_species.pdf', height=7, width=5)
ggplot() + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g), data=penguins) + 
  facet_wrap(~species)
dev.off()
# figure shows what appears to be a strong positive correlation with 1 or 2 outliers max

#check normality assumption
pdf('figures/flipper_qq_species.pdf', height=7, width=5)
ggplot(penguins) + 
  stat_qq(aes(sample=flipper_length_mm)) + 
  facet_wrap(~species)
dev.off()
pdf('figures/bodymass_qq_species.pdf', height=7, width=5)
ggplot(penguins) +
  stat_qq(aes(sample=body_mass_g)) + 
  facet_wrap(~species)
dev.off()
#both figures show good normality; most lie within -1 and 1

#check correlation
adelie = penguins %>%
  filter(species == "Adelie")
gentoo = penguins %>%
  filter(species == "Gentoo")
chinstrap = penguins %>%
  filter(species == "Chinstrap")

cor(x=adelie$flipper_length_mm, y=adelie$body_mass_g, use="complete.obs") # gives only coefficient r
# r = 0.47
cor(x=gentoo$flipper_length_mm, y=gentoo$body_mass_g, use="complete.obs")
# r = 0.70
cor(x=chinstrap$flipper_length_mm, y=chinstrap$body_mass_g, use="complete.obs")
# r = 0.64

#graph
library(GGally)
pdf('figures/flipper_vs_bodymass_matrix_species.pdf', height = 7, width = 5)
penguins %>%
  group_by(species) %>%
  select(flipper_length_mm, body_mass_g) %>%
  ggpairs(aes(color=species))
dev.off()

# most correlated in Gentoo, least correlated in Adelie

#adding this section because Git won't recognize these files and I need to add edits
1 + 1
2+2



