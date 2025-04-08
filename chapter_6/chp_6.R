# Chapter 6 #####
### Rohaan Farrukh #####

#Environment
library(devtools)
as.environment("package:stats")
globalenv()
baseenv()
emptyenv()

parent.env(globalenv())
parent.env(emptyenv())
ls(emptyenv())

ls(globalenv())

head(globalenv()$deck, 3)
assign("new", "Hello Global", envir = globalenv())
globalenv()$new

#active environment
environment()

#Assignment
new
new <- "Hello Active"
new

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

#Evaluation
show_env <- function(){
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}

show_env()

show_env()

environment(show_env)

environment(parenvs)

show_env <- function(){
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()

foo <- "take me to your runtime"
show_env <- function(x = foo){
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()

deal <- function() {
  deck[1, ]
}

environment(deal)

deal()

deal()

deal()

DECK <- deck
deck <- deck[-1, ]
head(deck, 3)

deal <- function() {
  card <- deck[1, ]
  deck <- deck[-1, ]
  card
}

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}

deal()

shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

head(deck, 3)
a <- shuffle(deck)
head(deck, 3)
head(a, 3)

shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

#Closure
shuffle()
deal()
deal()
setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
}

setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)

deal <- cards$deal
shuffle <- cards$shuffle

deal

shuffle

environment(deal)

environment(shuffle)

setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}
cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle

rm(deck)
shuffle()
deal()