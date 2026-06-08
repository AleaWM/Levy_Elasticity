library(tidyverse)

##Uses df from 2023 Methods Comp##

cc <- read_csv("concealed_carry.csv")

panel_data <- pdata.frame(cc, index = c("stateid", "year"))

panel_data$lag_vio_1  <- plm::lag(panel_data$vio, 1)

panel_data$lag_vio_2  <- plm::lag(panel_data$vio, 2)

panel_data$lag_vio_3  <- plm::lag(panel_data$vio, 3)

panel_data <- panel_data %>%
  mutate(log_lag_vio = log(lag_vio),
         log_vio = log(vio))

cor.test(panel_data$log_vio, panel_data$log_lag_vio)

cor.test(panel_data$vio, panel_data$lag_vio)

panel_data <- panel_data %>%
  filter(year != c("1977", "1978", "1978", "1979"))

cclm1 <- lm(mur ~ log(vio) + lag(vio), data = cc)

summary(cclm1)

cclm2 <- lm(mur ~ log(vio) + log(lag(vio)), data = cc)

summary(cclm2)