## Data source:
## http://www.amstat.org/publications/jse/v13n1/datasets.hayden.html

dat <- read.table(file.path("test", "outlier-dat.txt"))
hist(dat$V2)
summary(dat$V2)
sd(dat$V2)

dat_new <- data.frame(dat, V2_new = codeOutliersZ(dat$V2))
hist(dat_new$V2_new)
summary(dat_new$V2_new)
sd(dat_new$V2_new, na.rm = T)

dat_new3 <-
  dat_new %>%
  mutate(outlier = ifelse(is.na(V2_new), T, F),
         V1 = factor(V1, levels = rev(levels(V1))) ) %>%
  arrange(V1)

ggplot(dat_new3) +
  geom_point(aes(x = V2,
                 y = V1,
                 colour = outlier)) +
  theme_bw()
