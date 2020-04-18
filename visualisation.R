library(tidyverse)
library(dslabs)

data("gapminder")


#### Time series plots ####

# scatterplot of fertility rate in US by year
gapminder %>% filter(country == "United States") %>% 
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>% filter(country == "United States") %>% 
  ggplot(aes(year, fertility)) +
  geom_line()

### multiple time series
# 2 line for 2 countries with group
gapminder %>% filter(country %in% c("Germany","Vietnam")) %>% 
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

# 2 line with different colors for 2 countries with color (col)
gapminder %>% filter(country %in% c("Germany","Vietnam")) %>% 
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

# use label instead of legend
labels <- data.frame(country = c("Vietnam", "Germany"),
                     x = c(1968, 1980), y = c(6, 2))            # create label position
gapminder %>% filter(country %in% c("Germany","Vietnam")) %>% 
  ggplot(aes(year, fertility, col = country)) +
  geom_line() +
  geom_label(data = labels, aes(x, y, label = country), size = 5) +    # add label
  theme(legend.position = "none")                                     # remove legend

### Transformations ####

# add dollars per days
g <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
present_year <- 2010


# basic histogram
g %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black")

# using log scaled data
g %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, col = "black")

gapminder %>% filter(year == present_year) %>% 
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5, col = "black")

# using log scale
g %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2") +
  geom_histogram(binwidth = 1, col = "black")

#### Stratifying and boxplots ####


p1 <- g %>% filter(year == past_year, !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%  # change order of factor by a function apply to a numeric vector
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  theme(axis.text.x = element_text(hjust = 1, angle = 90)) +    # rotate the x-axis values
  xlab("")                                                     # remove x-axis labels
 
p1 + geom_boxplot()

p1 + scale_y_continuous(trans = "log2") +
  geom_boxplot()

p1 + scale_y_continuous(trans = "log2") +
  geom_boxplot() +
  geom_point(show.legend = FALSE)


#### Comparing distributions ####

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
country_past <- gapminder %>%
  filter(year == past_year, !is.na(gdp)) %>% 
  .$country
country_present <- gapminder %>%
  filter(year == present_year, !is.na(gdp)) %>% 
  .$country
country_list <- intersect(country_past, country_present)

# comparing with histogram
g %>% filter(year == past_year & !is.na(gdp)) %>% 
  mutate(group = if_else(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

g %>% filter(year == present_year & !is.na(gdp)) %>% 
  mutate(group = if_else(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

g %>% filter(year %in% c(past_year, present_year),
             country %in% country_list) %>% # filter countries with available data in all periods
  mutate(region = reorder(region, dollars_per_day, FUN = median),
         group = if_else(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# comparing with boxplots

p <- g %>% filter(year %in% c(past_year, present_year), country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%  # change order of factor by a function apply to a numeric vector
  ggplot() +
  theme(axis.text.x = element_text(hjust = 1, angle = 90)) +    # rotate the x-axis values
  xlab("") +                                                    # remove x-axis labels
  scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year~.)

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

#### Density plots ####


# basic density plots comparing 2 distributions
g %>% filter(year %in% c(past_year, present_year),
             country %in% country_list) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)

# change the y-axis to count (computed value by density) 
g %>% filter(year %in% c(past_year, present_year),
             country %in% country_list) %>% 
  mutate(group = if_else(region %in% west, "West", "Developing")) %>% 
  ggplot() +
  geom_density(aes(x = dollars_per_day, y = ..count.., fill = group),
               alpha = 0.2, bw = 0.75) +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~year)

# add new region groups with case_when
g1 <- g %>% mutate(group = case_when(
  .$region %in% west ~ "West",
  .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
  .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
  TRUE ~ "Others"
)) %>% mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia",
                                               "Sub-Saharan Africa", "West")))

# stacked density plots
g1 %>% filter(year %in% c(past_year, present_year),
              country %in% country_list) %>% 
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(aes(y = ..count..), alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# add weight by population ??
g1 %>% filter(year %in% c(past_year, present_year),
              country %in% country_list) %>% 
  group_by(year) %>% 
  mutate(weight = population/sum(population*2)) %>% 
  ungroup() %>% 
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

#### Logistic (logit) transformation ####


g2 <- g %>% 
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ as.character(.$region)
  ))

# make new data frame with group average income and avera infant survival rate
surv_income <- g2 %>% 
  filter(year == present_year, !is.na(gdp), !is.na(infant_mortality), !is.na(group)) %>% 
  group_by(group) %>% 
  summarise(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population)) %>% 
  arrange(income)

# plot infant survival rate versus income
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, col = group)) +
  scale_x_continuous(trans = "log2", limits = c(.25, 150)) +
  scale_y_continuous(trans = "logit", limits = c(.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)
