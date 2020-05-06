library(gtools)
library(tidyverse)
library(dslabs)


# setting the random seed
set.seed(1, sample.kind = "Rounding")

#### Monte Carlo simulations ####
beads <- rep(c("red", "blue"), times=c(2,3))
sample(beads, 1)
B <- 1000
events <- replicate(B, sample(beads, 1))
tab <- table(events)
prop.table(tab)

# with replacement
ev2 <- sample(beads, 1000, replace = TRUE)
prop.table(table(ev2))


#### Combination and permutation ####
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
decks <- expand.grid(number = numbers, suit = suits)
decks <- paste(decks$number, decks$suit)

kings <- paste("King", suits)
mean(decks %in% kings)

hands <- permutations(52, 2, v = decks)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)


aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands_c <- combinations(52, 2, v = decks)
mean(hands_c[,1] %in% aces & hands_c[,2] %in% facecard |
     hands_c[,1] %in% facecard & hands_c[,2] %in% aces)

hand <- sample(decks, 2)
(hand[1] %in% aces & hand[2] %in% facecard |
    hand [1] %in% facecard & hand[2] %in% aces)

blackjack <- function(){
  hand <- sample(decks, 2)
  (hand[1] %in% aces & hand[2] %in% facecard |
    hand [1] %in% facecard & hand[2] %in% aces)
}
blackjack()
B <- 10000
results <- replicate(B, blackjack())
mean(results)

#### Birthday problem ####
# checking for duplicate birthdays in a group of 50 ppl
n <- 50
bdays <- sample(1:365, n, replace = TRUE)
any(duplicated(bdays))

# Monte Carlo simulation with B=10000 replicates
B <- 10000
bday_results <- replicate(B, {       #return vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(bday_results)

# making a lookup table
compute_prob <- function(n, B=10000){
  sameday <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(sameday)
}
n <- seq(1,60)
prob <- sapply(n, compute_prob)
plot(n, prob)

# computing exact probability
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365  #return vector of fractions for the formulars
  1 - prod(prob_unique)  #return 1 - total product of all fractions
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting result from Monte Carlo simulation and exact calculation on the same graph
plot(n, prob)  #plot Monte Carlo simulation result
lines(n, eprob, col = "red")


#### Determine the number of Monte Carlo simulations ####

B <- 10^seq(1, 5, len = 100)
try_b <- function(B, n = 22){
  sameday <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays)) })
  mean(sameday)
}
try_prob <- sapply(B, try_b)

plot(log10(B), try_prob, type = "l")  #plot a graph of estimates with different Bs
  

#### Monty Hall problem ####

B <- 100000

# create the Monte Carlo simulation
montyhall <- function(strategy){
  doors <- as.character(1:3)
  prize <- sample(doors, 1)
  pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(prize, pick)], 1)
  choice <- ifelse(strategy == "stick", pick, doors[!doors %in% c(pick, show)])
  choice == prize
}

stick <- replicate(B, montyhall("stick"))
mean(stick)

switch <- replicate(B, montyhall("switch"))
mean(switch)


#### Assessment ####

# Olympic running
nrow(permutations(8, 3))
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
result <- replicate(10000,{
  medals <- sample(runners, 3)
  all(medals %in% "Jamaica")
})
mean(result)

# Restaurant management
nrow(combinations(2,1)) * nrow(combinations(6,2)) * nrow(combinations(6,1))
nrow(combinations(3,1)) * nrow(combinations(6,2)) * nrow(combinations(6,1))
nrow(combinations(3,1)) * nrow(combinations(6,3)) * nrow(combinations(6,1))

combo_entree <- function(choice){
  nrow(combinations(3,1)) * nrow(combinations(6,2)) * nrow(combinations(choice,1))
}
selection <- 1:12
combos <- sapply(selection, combo_entree)
selection[combos == 405]

combo_sides <- function(choice){
  nrow(combinations(3,1)) * nrow(combinations(choice,2)) * nrow(combinations(6,1))
}
selection <- 2:12
combos <- sapply(selection, combo_sides)
min(selection[combos > 365])

# Esophageal cancer and alcohol/tobacco use
data("esoph")

nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
esoph %>% group_by(alcgp) %>%
  summarise(cases = sum(ncases), controls = sum(ncontrols)) %>% 
  pull(cases) %>% sum()

esoph %>% group_by(tobgp) %>%
  summarise(cases = sum(ncases), controls = sum(ncontrols))

esoph %>% group_by(tobgp, alcgp) %>%
  summarise(cases = sum(ncases), controls = sum(ncontrols))
