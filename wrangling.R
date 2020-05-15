# !diagnostics off
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
murders_raw <- tab[c(1:3,7)] %>% setNames(c("state", "population", "total", "murder_rate")) 

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

#### String processing ####

# escape single quote with double quotes (& vice versa)
s <- '5"'
cat(s)  #use cat() to view the rendered string
s <- "10'"
cat(s)

# escape both with backslash
s <- "5'10\""
cat(s)

# detect whether there are commas
commas <- function(x) any(str_detect(x,","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population,",","")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2) # parse_number did more correction

murder_new <- murders_raw %>% mutate_at(2:3, parse_number)
head(murder_new)

# height data
data("reported_heights")
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% filter(is.na(x)) %>% head(10)


# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

### regular expression
pattern <- ","
str_detect(murders_raw$total, pattern) 

str_subset(reported_heights$height, "cm")

yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
str_detect(s, "\\d")
str_view(s, "\\d")
str_view_all(s, "\\d")
str_view(s, "[56]")

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view_all(s, pattern)

pattern <- "^\\d{1,3}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"  #add space
str_subset(problems, pattern_2)

### process input height
library(english)

words_to_numbers <- function(x){
  x %>% str_to_lower()
  for (i in 0:11) {
   x <- str_replace_all(x, words(i), as.character(i)) 
  }
  x
}

convert <- function(s){
  s %>%
    str_replace_all("feet|foot|ft","'") %>% 
    str_replace_all("inches|inch|''|\"|cm|and", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$","\\1'\\2") %>% 
    str_replace("^([56])'?$", "\\1'0") %>% 
    str_replace("^([12])\\s*,\\s*(\\d)$", "\\1.\\2") %>% 
    str_trim()
}

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height,
         height = words_to_numbers(height) %>% convert()) %>% 
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>% 
  mutate(guess = 12 * feet + inches) %>% 
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height, #inches
    between(height/2.54, smallest, tallest) ~ height/2.54,  #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54,  #meters
    TRUE ~ as.numeric(NA)
  )) %>% 
  mutate(height = if_else(is.na(height) & inches < 12 & between(guess, smallest, tallest),
                          guess, height)) %>% 
  select(-guess)

# check converted entries
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

### string splitting

filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
x <- str_split(lines, ",")
col_names <- x[[1]]
x <- x[-1]

library(purrr)
map(x, function(y) y[1]) %>% head(2)
map(x, 1)  #shortcut by map

dat <- tibble(map_chr(x, 1),  
              map_chr(x, 2),
              map_chr(x, 3),
              map_chr(x, 4),
              map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>% head

x <- str_split(lines, ",", simplify = TRUE)  #simplify return a matrix instead of list
col_names <- x[1,]
x <- x[-1,]
colnames(x) <- col_names
x %>% as_tibble() %>%
  mutate_all(parse_guess) %>%
  head(5)

### Practice
data("raw_data_research_funding_rates")
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)

the_names_2 <- the_names_2 %>% 
  str_trim() %>% 
  str_split("\\s+", simplify = T)

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)

### recode variable name
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

### Practice
schedule <- tibble(day = c("Monday", "Tuesday"), staff = c("Mandy, Chris and Laura", "Steve, Ruth and Frank"))

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(staff)

### Practice
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

polls <- polls %>% filter(str_detect(Remain, "%")) %>% 
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))

#### Text mining #####
library(tidyverse)
library(lubridate)
library(scales)

#url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
#trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
#  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
#  filter(!is_retweet & !str_detect(text, '^"')) %>%
#  mutate(created_at = parse_date_time(created_at, 
#                                      orders = "a b! d! H!:M!:S! z!* Y!",
#                                      tz="EST")) 

library(dslabs)
data("trump_tweets")

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

library(tidytext)

### Practice
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
options(digits = 3)

PP <- gutenberg_download(1342)  #Pride and Prejudice
words <- PP %>% unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word)
words <- words %>% filter(!str_detect(word, "\\d"))
words %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% filter(n > 100) %>% nrow()
afinn <- get_sentiments("afinn")
afinn_sentiments <- words %>% inner_join(afinn, by = "word")


#### Assessment ####

library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("xdg-open", args = fn)  #linux

txt <- pdf_text(fn)
x <- str_split(txt[[9]], "\n")
s <- x[[1]] %>% str_trim()
header_index <- str_which(s, "2015")[1]
header <- s[header_index] %>% str_split("\\s+", simplify = T)
month <- header[1,1]
header <- header[,-1]

tail_index <- str_which(s, "Total")
n <- str_count(s, "\\d+")
ind <- n > 1
ind <- replace(ind, 1:header_index,F)
ind <- replace(ind, tail_index:length(s),F)
s <- s[ind]

s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
setNames(s, c("day",header))
header <- c("day", header)

tab <- as_tibble(s) %>% setNames(header) %>% 
  mutate_all(parse_number) %>% 
  mutate(month = month)

tab %>% filter(day >= 20) %>% .$`2017` %>% mean()

tab <- tab %>% gather(year, deaths, `2015`:`2018`)

tab %>% filter(year != 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20, col = "red")
