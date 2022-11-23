library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)

# The functions might be useful for A4
source("~/Documents/info201/assignments/a4-valeriielam/source/a4-helpers.R")

get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/assignments/a4-valeriielam/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- get_data() %>% 
    group_by(year, total_jail_pop) %>% 
    summarise(get_year_jail_pop = sum(year * total_jail_pop ) / sum(total_jail_pop))

plot_jail_pop_for_us <- function(){
  return(ggplot(data = get_year_jail_pop) +
           geom_col(mapping = aes(x = year, y = total_jail_pop)))
}

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
   
states <- c("WA", "CA","NY")

get_jail_pop_by_states<- function(states){
  return(get_data() %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop_state = sum(total_jail_pop, na.rm = TRUE)))
    
}

plot_jail_pop_by_states <- function(states){
  
  return(ggplot(data = get_jail_pop_by_states(states),
         aes(x = year, y = total_jail_pop_state, group = state, color = state)) + 
    geom_line())
}


plot_jail_pop_by_states(states)


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


states <- c("WA","CA","NY")

black_jail_plot_by_states <- function(states)
          return(get_data() %>% 
           filter(state %in% states) %>%
           group_by(black_jail_pop, year) %>%
           summarize(black_jail_plot_by_states = sum(black_jail_pop, na.rm = TRUE)))

white_jail_plot_by_states <- function(states)
          return(get_data() %>% 
          filter(state %in% states) %>%
          group_by(white_jail_pop, year) %>%
          summarize(white_jail_plot_by_states = sum(white_jail_pop, na.rm = TRUE)))
          


black_and_white_jail_plot <- function(states){
  return(ggplot(data = get_data()) +
  geom_point(mapping = aes(x = year, y = black_jail_plot_by_states)))
  
}
black_and_white_jail_plot()


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


total_jail_pop <- get_data() %>% 
  group_by(year, state) %>% 
  summarise(total_jail_pop = sum(year * total_jail_pop ) / sum(total_jail_pop))

plot_jail_pop <- function(states){
  return(ggplot(data = total_jail_pop)) +
    geom_col(mapping = aes(x = year, y = total_jail_pop))
}

plot_jail_pop()

# Load data frame ---- 

