library(tidyverse)
library(broom)
library(cowplot)
library(RColorBrewer)

metadata <- read_tsv("data/mothur/resv_metadata.txt") %>% 
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

top_phyla <- c("Firmicutes", "Bacteroidetes", "Proteobacteria",
               "Actinobacteria", "Deferribacteres", "Fusobacteria")

shared_taxa <- inner_join(shared, taxonomy)


shared_taxa %>% 
  filter(phylum %in% top_phyla) %>% 
  ggplot(., aes(x=group, fill=phylum, y=relabund)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 14, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 12, face = "bold")) + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "Relative Abundance (%)", fill = "Phylum") + 
  scale_fill_brewer(palette = "Set1")

















