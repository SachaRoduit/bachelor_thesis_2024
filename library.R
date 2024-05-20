# path for my computer
myPaths <- .libPaths()

myPaths <- c(myPaths, "C://Program Files//R//win-library//4.1")

.libPaths(myPaths)
setwd("~/Bachelor_Arbeit/data")

#libraries used 
library(magrittr)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(rsample)
library(openxlsx)
library(stringr)
library(fastDummies)
library(ggmap)
library(igraph)
library(tm)
library(openxlsx)
library(ranger)
library(caret)
library(pROC)
library(yardstick)
library(lubridate)
library(tidyverse)
library(xgboost)  




