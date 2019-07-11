library(tidyverse)
library(modelr)
fg_train <- read_csv("data/nfl_fg_train.csv")

logit_1 <- glm(Success ~ Distance, family = binomial, data = fg_train)


logit_2 <- glm(Success ~ Distance + Kicker, family = binomial, data = fg_train)

fg_train <-
  fg_train %>%
  add_predictions(model = logit_1, type = "link", var = "Logit1") %>%
  add_predictions(model = logit_1, type = "response", var = "phat1")
grid <- 
  fg_train %>%
  data_grid(Distance)

grid <- 
  grid %>%
  add_predictions(model = logit_1, type = "response", var = "logit_distance")

ggplot(grid) +
  geom_line(aes(x = Distance, y = logit_distance))

# Create a grid for each 
new_grid <-
  fg_train %>%
  data_grid(Kicker, Distance) %>%
  add_predictions(model = logit_2, type = "response", var = "phat") %>%
  filter(Kicker %in% c("Bailey", "Vinatieri", "Zuerlein"))

# create a separate plot 
ggplot(new_grid) + 
  geom_line(aes(x = Distance, y = phat, col = Kicker))
