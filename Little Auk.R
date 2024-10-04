# Set up packages and plotting theme ####

library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)

theme1 <-
  theme_bw() +
  theme(
    axis.text = element_text(size = 7, family = "sans"),
    axis.title = element_text(size = 9, family = "sans"),
    strip.background = element_blank(),
    strip.text = element_text(size = 8, family = "sans"),
    legend.text = element_text(size = 10, family = "sans"),
    panel.grid = element_blank()
  )

# Prepare data ####

littleauk <- read.csv("littleauk.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)

str(littleauk)

levels(littleauk$final.ID)

summary(littleauk$colour)

levels(littleauk$colour) <-
  c(
    "Black",
    "Blue",
    "Clear",
    "Green",
    "Green",
    "Multi-colour",
    "Olive",
    "Pink",
    "Red",
    "White",
    "Yellow",
    "Yellow-green"
  )

summary(littleauk$shape)

levels(littleauk$shape) <- c("Film", "Fragment", "Gel")

littleauk$num <- ifelse(is.na(littleauk$shape), 0, 1)

# Plot particles by by Spectroscopic ID ####

tiff(
  'Figure 2.tiff',
  height = 10,
  width = 18.2,
  units = "cm",
  res = 600,
  compression = "lzw"
)

ggplot(subset(littleauk, !is.na(final.ID))) +
  geom_bar(
    aes(x = shape, y = num, fill = final.ID),
    colour = 'black',
    linewidth = 0.25,
    position = 'fill',
    stat = 'identity'
  ) +
  labs(x = 'Shape', y = 'Proportion of Particles') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "turbo", name = "") +
  theme1

dev.off()

# Plot by Particle Colour ----

tiff(
  'Figure 3.tiff',
  height = 10,
  width = 18.2,
  units = "cm",
  res = 600,
  compression = "lzw"
)

ggplot(subset(littleauk, !is.na(final.ID))) +
  geom_bar(
    aes(x = shape, y = num, fill = colour),
    alpha = 0.9,
    colour = 'black',
    size = 0.25,
    position = 'fill',
    stat = 'identity'
  ) +
  labs(x = 'Shape', y = 'Proportion of Particles') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    values = c(
      "black",
      "dodgerblue3",
      "gray",
      "forestgreen",
      "turquoise",
      "khaki4",
      "palevioletred1",
      "red3",
      "white",
      "goldenrod2",
      "darkolivegreen3"
    ),
    name = ""
  ) +
  theme1

dev.off()

# Calculate mean and SD ####

x <- c(rep(0, times = 110 - 9), rep(1, times = 9))
mean(x)
sd(x)

# Indicate which particles are plastic? ####

littleauk$plastic <-
  with(littleauk,
       ifelse(
         final.ID == "Polyethylene terepthalate",
         yes = 1,
         ifelse(
           final.ID == "Polypropylene",
           1,
           ifelse(
             final.ID == "Polystyrene",
             1,
             ifelse(final.ID == "Synthetic rubber", 1, 0)
           )
         )
       ))

littleauk$plastic

littleauk$plastic[is.na(littleauk$plastic)] <- 0

# Summarize by bird ####

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

# Remove blanks ####

auksum <-
  auksum %>%
  filter(
    sample.id != "B1" &
      sample.id != "B2" &
      sample.id != "B3" &
      sample.id != "B4" &
      sample.id != "B5" &
      sample.id != "B6"
  )

# Summarize for all adults and chicks ####


auksum2 <-
  auksum %>%
  group_by(life.stage, plastic.count) %>%
  summarize(count = length(plastic.count)) %>%
  ungroup() %>%
  group_by(life.stage) %>%
  mutate(percent = (count / sum(count)) * 100) %>%
  ungroup()

# Plot adults vs. chicks ####

tiff(
  "Figure 5.tiff",
  width = 8.84,
  height = 5,
  units = "cm",
  res = 600
)

ggplot(auksum2) +
  geom_col(aes(
    x = life.stage,
    y =  percent,
    fill = as.factor(plastic.count)
  ), colour = "black") +
  scale_fill_viridis_d(option = "plasma", name = "Plastic in Feces") +
  labs(x = "Life Stage", y = "Percent") +
  scale_y_continuous(expand = c(0, 0)) +
  theme1

dev.off()

# Run GLM ####

lifestagemod <-
  glmmTMB(plastic.count ~ life.stage,
          family = binomial(link = "logit"),
          data = auksum)

plot(simulateResiduals(lifestagemod))  # looks good

summary(lifestagemod)

# Compare adults by gular pouch status ####

adults <-
  auksum %>%
  filter(life.stage == "Adult")

adultsum <-
  adults %>%
  group_by(gular.pouch.status, plastic.count) %>%
  summarize(count = length(plastic.count)) %>%
  ungroup() %>%
  group_by(gular.pouch.status) %>%
  mutate(percent = (count / sum(count)) * 100) %>%
  ungroup()

ggplot(adultsum) +
  geom_col(aes(
    x = gular.pouch.status,
    y =  percent,
    fill = as.factor(plastic.count)
  ),
  colour = "black") +
  scale_fill_viridis_d(option = "plasma", name = "Plastic in Feces") +
  labs(x = "Life Stage", y = "Percent") +
  scale_y_continuous(expand = c(0, 0)) +
  theme1

gularpouchmod <-
  glmmTMB(
    plastic.count ~ gular.pouch.status,
    family = binomial(link = "logit"),
    data = adults
  )

plot(simulateResiduals(gularpouchmod))

summary(gularpouchmod)

# Summarize by particle length

lengthsum <-
  littleauk %>%
  filter(!is.na(length) &
           plastic == 1)

lengthsum
