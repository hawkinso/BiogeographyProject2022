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
library(picante)
library(RColorBrewer)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# Getting the phylogeny and making it usable for R ----
# Go to the following website to find the tree for the fish lineages you are interested in
browseURL("https://fishtreeoflife.org/")

# Download the time calibrated phylogeny. This will download as a '.tre' file but you will need to convert to 
# a Nexus or Newick file. 
# Go to the following website to convert to the file you choose 
browseURL("http://phylogeny.lirmm.fr/phylo_cgi/data_converter.cgi")

# Upload the .tre file and choose Nexus or Newick as your output format. 
# After it converts, copy and past the data into a text file and make sure that it starts at #NEXUS (or # NEWICK). 

# Import phylogeny
tree <- read.tree("Labridae_newick") # read.tree only for newick
tree <- read.nexus("labridtree.txt") 
# New tree to try 
tree <- read.nexus("Labrid340PNAS.nex")

# Import dataframe containing genera, species names, scientific names, and references 
species.file <- read.csv("Species_Author_CommonName_BiogeographyProject.csv")

genera <- as.data.frame(species.file %>% 
  select(Group,Scientific.Name))

# Prune tree so that the tree represents the species with molecular data and fish base profiles
pruned.tree <- drop.tip(tree,tree$tip.label[-match(genera, tree$tip.label)])
identical(pruned.tree$tip.label, rownames(genera))  # Always double-check to make sure the process did what you think it did

## Building trees ## ---- 

# Separate by genus and visualize 
pruned.tree.tib <- as_tibble(pruned.tree)

groupInfo <- split(pruned.tree$tip.label,
                   gsub("_\\w+","",pruned.tree$tip.label))
pruned.tree <- groupOTU(pruned.tree,groupInfo)

# Circular 
ggtree(pruned.tree,
       aes(color=group),
       layout="circular")+
  geom_tiplab(size=1,
              aes(angle=angle),color="black")+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "none")

# Rectangular
ggtree(pruned.tree,
       aes(color=group))+
  geom_tiplab(size=1,color="black")+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_tree2()+
  theme(legend.position = "none")

#### (AR data pruned) tree. Available for 340 species 
AR <- read.csv("ARdata.csv")
AR <- AR %>%
  select(genus_species,Aspect.Ratio)
genera.AR <- as.data.frame(AR %>% 
                          select(genus_species))
genera.AR <- genera.AR[1:340, ]


groupInfo <- split(tree$tip.label,
                   gsub("_\\w+","",tree$tip.label))
pruned.tree <- groupOTU(tree,groupInfo)
pruned.tree <- as_tibble(pruned.tree)
pruned.tree$label <- sub("_"," ",pruned.tree$label)
pruned.tree <- as.treedata(pruned.tree)

# Extend color palette to the number of genera 
# Define the number of colors you want
nb.cols <- 72
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Plot rectangular
ggtree(pruned.tree,
       aes(color=group),
       layout="rectangular")+
  scale_color_manual(values=mycolors)+
  geom_tiplab(size=1,color="black")+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_tree2()+
  xlab("Millions of Years")+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold"))

# plot circular
ggtree(pruned.tree,
       aes(color=group),
       layout="circular",branch.length = "none")+
  scale_color_manual(values=mycolors)+
   geom_tiplab(size=1,color="black",face="italic")+
   guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_tree2()+
  theme(legend.position = "none",
        axis.text.x = element_text(face="bold"))


# Add in traits to plot on the phylogeny ---- 

# Read in data set with ecology data 
ecol.dat <- read.csv("Ecology_pruned.csv")

# Read in AR data again 
# AR is binned by low, medium, high 
AR <- read.csv("ARdata.csv")

# We are interested in aspect ratio and habitat type
# First we will look at aspect ratio
AR <- data.frame(label=AR$genus_species,AR$AR..L.M.H)
AR <- AR[1:340, ]
tree.tib <- as_tibble(tree)
tree.tib <- tree.tib[1:340, ]
tree.AR <- full_join(tree.tib,AR, by='label')
tree.AR <- as.phylo(tree.AR)

ggtree(tree.AR,
       layout="circular")+
  geom_tiplab(size=1,color="black",face="italic")+
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_tree2()+
  geom_tippoint(aes(colour=AR.AR..L.M.H))+
  theme(legend.position = "none",
        axis.text.x = element_text(face="bold"))


# Let's look at habitat type 
ecol.dat <- ecol.dat %>%
  select(genus_species,Habitat.type,GenvsSpec)

groupInfo <- split(ecol.dat$genus_species,
                   gsub("_\\w+","",ecol.dat$genus_species))

ecol.dat$group <- sub("_.*","",ecol.dat$genus_species)




