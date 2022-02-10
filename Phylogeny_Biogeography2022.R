# Biogeopgraphy Project 2022 
# Author(s): Olivia H. Hawkins 
# Date: 02/10/2022 
# Goals: Build phylogeny

# Git-hub 
browseURL("https://github.com/hawkinso/BiogeographyProject2022.git")

# Load libraries 
library(rfishbase)
library(tidyverse)
library(tidyr)
library(ape)
library(ggplot2)
library(ggtree)
library(Biostrings)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# PHYLOGENY ---- 

# Import phylogeny
tree <- read.tree("Labridae_newick")

# Work with tree 
ggtree(tree,mrsd = TRUE)