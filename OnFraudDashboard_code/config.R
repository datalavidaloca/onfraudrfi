#Este archivo encapsula todas las entradas y librerias de la app.

is.installed <- function(paquete) is.element(
  paquete, installed.packages())


if(!is.installed('SPARQL'))
  install.packages('SPARQL')

if(!is.installed('ggplot2'))
  install.packages('ggplot2')

if(!is.installed('dplyr'))
  install.packages('dplyr')

if(!is.installed('rJava'))
  install.packages('rJava')

if(!is.installed('shiny'))
  install.packages('shiny')

if(!is.installed('shinydashboard'))
  install.packages('shinydashboard')

if(!is.installed('plotly'))
  install.packages('plotly')

if(!is.installed('devtools'))
  install.packages('devtools')

if(!is.installed('jcheng5/bubbles'))
  devtools::install_github("jcheng5/bubbles")

if(!is.installed('reshape2'))
  install.packages('reshape2')

if(!is.installed('DT'))
  install.packages('DT')

if(!is.installed('properties'))
  install.packages('properties')

library (SPARQL)
library(ggplot2)
library (dplyr)
library (rJava)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(bubbles)
library(reshape2)
library(DT)
library(properties)


myProps <- read.properties("config.properties")

devolver.endpoint <- function()
{
  return (myProps$endpoint);
}
devolver.rdf <- function()
{
  return (myProps$rdf);
}

