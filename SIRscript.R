
#this script runs an interactive SIR model simulating a zombie apocalypse.

###Note: if the interactive model won't run after you download necessary packages,
#restart R and try again ###

#load packages
library(deSolve)
library(gganimate)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(png)
library(reshape2)

#create SIR equation
SIR <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S #susceptible individuals
    dI <-  beta * I * S - gamma * I #infected individuals
    dZ <-  gamma * I #Zombified individuals
    return(list(c(dS, dI, dZ)))
  })
}

parameters  <- c(
  beta  = runif(1, min = 0.00035, max = 0.00045), # infectious contact rate (/person/day)
  #randomly select beta value between the two values notes
  gamma = 0.5    # recovery rate (/day)
)

#input initial values for SIR model
initial_numbers <- c(
  S = 100,  #number of susceptible individuals at time = 0
  I = 500,  #number of infectious individuals at time = 0
  Z =  0  # number of dead zombies (recovered (and immune) at time = 0
)

#set the total amount of time (in days)
time <- seq(0, 30)

SIR_numbers <- ode( #differential equation function
  y = initial_numbers,
  times = time,
  func = SIR,
  parms = parameters
)

#make SIR_numbers into a dataframe
SIR_numbers <- as.data.frame(SIR_numbers)
SIR_numbers

#reformat the SIR_numbers df into long format
SIR_reformat <- melt(SIR_numbers, id = "time", measure = c("S", "I", "Z"))

#rename S, I, and R values --> Susceptible, Infected, Zombified
SIR_reformat2 <- SIR_reformat |>
  mutate(Variables = case_when(grepl("S", variable) ~ "Susceptible",
                               grepl("I", variable) ~ "Infected",
                               grepl("Z", variable) ~ "Zombified"))

#plot interactive graph
graph <- SIR_reformat2 |> #use reformatted data
  ggplot(aes(x= time, y = value, color = Variables)) + #plot lines w/ diff colors
  geom_line(size = 1) + #change size of line
  labs(title = "Zombie Apocalypse SIR Model", #add title
       y = "Number Of People", #add y-axis name
       x = "Time (days)") + #add x-axis name
  theme(legend.background = element_blank(), #add background to legend
        plot.title = element_text(hjust = 0.5),#move title to center
        aspect.ratio = 0.4) 
graph + transition_reveal(time) #plot interactive graph

#Reference:
#https://rpubs.com/choisy/sir
#https://github.com/abhimotgi/dataslice/blob/master/R/gganimate%20code.R
#https://stackoverflow.com/questions/43696227/mutate-with-case-when-and-contains
