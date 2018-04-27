# Slipery
#
# 1D sliding-block model of earthquake rupture/interaction.  Simulates a strike-slip
# fault using a variable number of linked sliding blocks.  Blocks move when net
# force exceeds frictional resistance (stick/slip).  Block interactions extend two
# blocks in either direction for force computations.  Interactions extend further.
#
# Block interactions, static friction, size distribution and number are user
# selected.  Here size is a interchangeable proxy for fault strength--
# a long block is equivalent to a strong block (high friction).
#
# Most parameters are chosen to make results realistic.  They are not derived from
# observation.  This is an analog model after all...

library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(magrittr)
source("runSimulation.R")

# Impose personal bias on graphics and warnings
theme_set(theme_bw())
options(warn=-1)

# Set initial state and globals
scale_length <- 20
vp <- 0.02
X <- matrix( 0 )
A <- vector( "double", 0 )
