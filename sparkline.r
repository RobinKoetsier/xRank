library(dplyr)
library(sparkline)

data <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = list(weight)) %>%
  mutate(boxplot = NA, sparkline = NA)

reactable(data, columns = list(
  weight = colDef(cell = function(values) {
    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
  }),
  boxplot = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]], type = "box")
  }),
  sparkline = colDef(cell = function(value, index) {
    df <- data2 %>% filter(feed == data$feed[index])
   
   print(df$weight2)
    sparkline(df$weight2[[1]])
  })
))

data2 <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = (list(zoo::rollmean(weight,5))))


data2 <- data.frame("feed"=chickwts$feed,
                    "weight"= runif(length(chickwts$feed), -10, 10) ) %>%
  group_by(feed) %>%
  summarise(weight1 = (list(zoo::rollmean(weight,5))),
            weight2 = list(weight))

sparkline(data2$weight2[[1]])
sparkline(df$weight2[[1]])
