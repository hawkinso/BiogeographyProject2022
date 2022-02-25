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
library(phylobase)
library(treeio)
library(reshape2)
library(ggstance)
library(rstatix)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# PHYLOGENY ---- 

# Import phylogeny
tree <- read.tree("Labridae_newick") # read.tree only for newick
tree <- read.nexus("labridtree.txt") 

# Import dataframe containing genera, species names, scientific names, and references 
species.file <- read.csv("Species_Author_CommonName_BiogeographyProject.csv")

genera <- as.data.frame(species.file %>% 
  select(Group,Scientific.Name))

# Make tree data merge with group data
tree.new <- full_join(as_tibble(tree),genera, by= c('label'="Scientific.Name"))
tree.final <- as.treedata(tree.new)

# Prune tree so that the tree represents the species with molecular data and fish base profiles
pruned.tree <- drop.tip(tree,tree$tip.label[-match(genera, tree$tip.label)])
identical(pruned.tree$tip.label, rownames(genera))  # Always double-check to make sure the process did what you think it did

## Building trees ## ---- 

# Separate by genus and get node labels 
pruned.tree.tib <- as_tibble(pruned.tree)

groupInfo <- split(pruned.tree$tip.label,
                   gsub("_\\w+","",pruned.tree$tip.label))
pruned.tree <- groupOTU(pruned.tree,groupInfo)

ggtree(pruned.tree,
       aes(color=group),
       layout="circular")+
  geom_tiplab(size=1,
              aes(angle=angle))+
  theme(legend.position = "none")

  

