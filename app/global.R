library(shiny)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)
library(mlr)
library(rpart.plot)
library(scales)


FullData = readRDS('data/FullData.RDS')
FullDataT = readRDS('data/FullDataT.RDS')

DataList = list(All = FullData, Ted = FullDataT)
