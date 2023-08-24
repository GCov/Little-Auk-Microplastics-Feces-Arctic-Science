library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)

theme1 <-
  theme_bw() +
  theme(
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 7,
                        family = "serif"),
    axis.text = element_text(size = 7),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

littleauk <- read.csv("littleauk.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)

str(littleauk)

levels(littleauk$final.ID)

# What is plastic?

littleauk$plastic <-
  with(littleauk, ifelse(final.ID == "Polyethylene terepthalate",
                        yes = 1,
                        ifelse(
                          final.ID == "Polypropylene",
                          1,
                          ifelse(
                            final.ID == "Polystyrene",
                            1,
                            ifelse(final.ID == "Synthetic rubber",
                                   1, 0)))))

littleauk$plastic

littleauk$plastic[is.na(littleauk$plastic)] <- 0

# Summarize by bird

auksum <-
  littleauk %>% 
  group_by(sample.id,
           sampler,
           plot.number,
           elevation,
           life.stage,
           gular.pouch.status) %>% 
  summarize(plastic.count = sum(plastic)) %>% 
  ungroup

auksum$plastic.count <- as.factor(auksum$plastic.count)

levels(auksum$plastic.count) <- c("Absent", "Present")

# remove blanks

auksum <- 
  auksum %>% 
  filter(sample.id != "B1" &
           sample.id != "B2" &
           sample.id != "B3" &
           sample.id != "B4" &
           sample.id != "B5" &
           sample.id != "B6")

# summarize for all adults and chicks


auksum2 <-
  auksum %>% 
  group_by(life.stage,
           plastic.count) %>% 
  summarize(count = length(plastic.count)) %>%
  ungroup() %>% 
  group_by(life.stage) %>% 
  mutate(percent = (count / sum(count)) * 100) %>% 
  ungroup()

# plot adults vs. chicks

tiff("Adult vs. Chick Plot.tif",
     width = 8.84,
     height = 6,
     units = "cm",
     res = 300)

ggplot(auksum2) +
  geom_col(aes(x = life.stage,
               y =  percent,
               fill = as.factor(plastic.count)),
           colour = "black") +
  scale_fill_viridis_d(option = "plasma",
                       name = "Plastic in Feces") +
  labs(x = "Life Stage",
       y = "Percent") +
  scale_y_continuous(expand = c(0,0)) +
  theme1

dev.off()

lifestagemod <-
  glmmTMB(plastic.count ~ life.stage,
          family = binomial(link = "logit"),
          data = auksum)

plot(simulateResiduals(lifestagemod))

summary(lifestagemod)

# Compare adults by gular pouch status

adults <- 
  auksum %>% 
  filter(life.stage == "Adult")

adultsum <-
  adults %>% 
  group_by(gular.pouch.status,
           plastic.count) %>% 
  summarize(count = length(plastic.count)) %>%
  ungroup() %>% 
  group_by(gular.pouch.status) %>% 
  mutate(percent = (count / sum(count)) * 100) %>% 
  ungroup()

ggplot(adultsum) +
  geom_col(aes(x = gular.pouch.status,
               y =  percent,
               fill = as.factor(plastic.count)),
           colour = "black") +
  scale_fill_viridis_d(option = "plasma",
                       name = "Plastic in Feces") +
  labs(x = "Life Stage",
       y = "Percent") +
  scale_y_continuous(expand = c(0,0)) +
  theme1

gularpouchmod <-
  glmmTMB(plastic.count ~ gular.pouch.status,
          family = binomial(link = "logit"),
          data = adults)

plot(simulateResiduals(gularpouchmod))

summary(gularpouchmod)

# Summarize by particle length

lengthsum <- 
  littleauk %>% 
  filter(!is.na(length) &
           plastic == 1)

lengthsum

# Summarize by shape

