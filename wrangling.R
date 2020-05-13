library(dslabs)
library(tidyverse)

path <- system.file("extdata", package = "dslabs")
files_name <- list.files(path)
files <- file.path(path,files_name)
dir.create(file.path(getwd(),"Data"))
data_dir <- file.path(getwd(),"Data")
file.copy(files, data_dir)
file.exists(file.path(data_dir, files_name))

#### gather() and spread() ####


# import original wide data
wide_data <- read_csv(file.path(data_dir, "fertility-two-countries-example.csv"))

# tidy data from dslabs
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>% 
  select(country, year, fertility)

# gather wide data
new_tidy_data <- wide_data %>% 
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>% 
  gather(year, fertility, -country)
head(new_tidy_data)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names
new_tidy_data <- wide_data %>% 
  gather(year, fertility, -country, convert = T)
class(new_tidy_data$year)

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% 
  spread(year, fertility)
head(new_wide_data)

#### separate() and unite() ####

# import data
raw_dat <- read_csv(file.path(data_dir, "life-expectancy-and-fertility-two-countries-example.csv"))

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year","variable_name"), sep = "_")
dat %>% separate(key, c("year","variable_name")) # default separator is underscore

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year","first","second"), fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), extra = "merge") %>% 
  spread(variable_name, value)

# separate then unite
dat %>% separate(key, c("year","first","second"), fill = "right") %>% 
  unite(variable_name, first, second, sep = "_") %>% 
  spread(variable_name,value) %>% 
  rename(fertility = fertility_NA)

#### Practice ####

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide, month, co2, -year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data("admissions")
dat <- admissions %>% select(-applicants)
dat %>% spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp2 <- unite(tmp, column_name, c(key, gender), sep = "_")
spread(tmp2, column_name, value)

# Lahman library
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master, by = "playerID") %>% 
  select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>% 
  right_join(top_names) %>% 
  select(nameFirst, nameLast, teamID, HR, salary)

awards_16 <- AwardsPlayers %>% filter(yearID == 2016)

top_names %>% semi_join(awards_16) 
awards_16 %>% anti_join(top_names) %>% pull(playerID) %>% unique() %>% length()

#### Web scraping ####

library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state")
h <- read_html(url)
class(h)
html_text(h)
tab <- h %>% html_nodes("table")
tab <- tab[[2]] %>% html_table()
tab <- tab[c(1:3,7)] %>% setNames(c("state", "population", "total", "murder_rate")) 

# Practice
# Question 1, 2 and 3
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

tab1 <- html_table(nodes[[1]], fill = T)
tab2 <- html_table(nodes[[2]], fill = T)
tab3 <- html_table(nodes[[3]], fill = T)
tab4 <- html_table(nodes[[4]], fill = T)

tab_1 <- html_table(nodes[[10]])
tab_1 <- tab_1[2:31, 2:4] %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- html_table(nodes[[19]])
tab_2 <- tab_2[2:nrow(tab_2),] %>% setNames(c("Team", "Payroll", "Average"))

full_join(tab_1, tab_2, by = "Team")

# Question 4 and 5
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")

ttab <- html_table(tab, fill = T)
sapply(1:9, function(i){ncol(ttab[[i]])})

# Extra
url <- "https://m.imdb.com/chart/bestpicture/"
h <- read_html(url)

title <- html_nodes(h, ".best-picture-item-title") %>% html_text(trim = T) 
year_winning <- html_nodes(h, ".titlemeter") %>% html_text(trim = T)
rating <- html_nodes(h, ".h4") %>% html_text(trim = T)

best_movies <- data.frame(title = title, year_winning = year_winning, IMDB_rating = rating)
