library(tidyverse)
library(broom)
library(cowplot)
library(RColorBrewer)

metadata1 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  mutate(Hour=factor(Hour))

shared <- read_tsv("data/mothur/final.0.03.subsample.shared") %>% 
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  pivot_longer(-group, names_to = 'otu', values_to = 'count') %>% 
  mutate(relabund = count/1816)

taxonomy <- read_tsv("data/mothur/final.taxonomy") %>% 
  rename_all(tolower) %>% 
  # Split taxonomic information into separate columns for each taxonomic level  
  mutate(taxonomy=str_replace_all(taxonomy, c("\\(\\d*\\)" = "", #drop digits with parentheses around them
                                              ';$' = "", #removes semi-colon 
                                              'Bacteria_unclassified' = 'Unclassified',
                                              "_unclassified" = " Unclassified"))) %>% 
  # Separate taxonomic levels into separate columns according to semi-colon.
  separate(taxonomy, into=c("kingdom", "phylum", "class", "order", "family", "genus"), sep=';')

##Easily readable colors for top 10
colours = c( "#A54657",  "#582630", "#F7EE7F", "#4DAA57","#F1A66A","#F26157", "#F9ECCC", "#679289", "#33658A",
                         "#F6AE2D","#86BBD8")


shared_taxa <- inner_join(shared, taxonomy) %>%
  group_by(group, otu) %>% 
  summarize(agg_relabund=sum(relabund)) %>% 
  ungroup()

top_otus <- shared_taxa %>% 
  group_by(otu) %>% 
  summarize(median=median(agg_relabund)) %>% 
  arrange((desc(median))) %>% 
  top_n(n=10, median) %>% 
  pull(otu)


















