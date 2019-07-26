# Group Members
# Arushi Mahajan, ID: 11915079
# Kapil Bindal, ID:
# Sivesh Kango, ID:


try(require(shiny) || install.packages("shiny"))
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
try(require("fmsb")||install.packages("fmsb"))

library("shiny")
library("fmsb")
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)

