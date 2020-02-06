library(tidyverse)
library(broom)
library(cowplot)
library(RColorBrewer)

metadata <- read_tsv("resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                       'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB')) %>% 
  mutate(Hour=factor(Hour))

shared <- read_tsv("sample.final.shared") %>% 
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>% 
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                       'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB')) %>% 
  pivot_longer(-group, names_to = 'otu', values_to = 'count') %>% 
  mutate(relabund = count/1000)

######### Now need a combined metadata and shared file with only the interesting groups ####
relgroups <- grep()

relevant_groups <- read_tsv("sample.final.shared") %>%
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>% 
  filter(group %in% relgroups)







taxonomy <- read_tsv("final.taxonomy") %>% 
  rename_all(tolower) %>% 
  # Split taxonomic information into separate columns for each taxonomic level  
  mutate(taxonomy=str_replace_all(taxonomy, c("\\(\\d*\\)" = "", #drop digits with parentheses around them
                                              ';$' = "", #removes semi-colon 
                                              'Bacteria_unclassified' = 'Unclassified',
                                              "_unclassified" = " Unclassified"))) %>% 
  # Separate taxonomic levels into separate columns according to semi-colon.
  separate(taxonomy, into=c("kingdom", "phylum", "class", "order", "family", "genus"), sep=';')

taxa_metadata <- inner_join(shared, taxonomy) %>% 
  group_by(group, phylum) %>% 
  summarize(agg_relabund = sum(relabund)) %>%
  inner_join(., metadata, by='group') %>% 
  ungroup() %>%
  select(-R1_filename, -R2_filename)


top_phyla <- taxa_metadata %>%
  group_by(phylum) %>% 
  summarize(median = median(agg_relabund)) %>% 
  arrange(desc(median)) %>% 
  top_n(n=4, median) %>% 
  pull(phylum)

taxa_metadata %>% 
  filter(phylum %in% top_phyla) %>% 
  ggplot(aes(x=phylum, y = agg_relabund, color = Hour))+
  geom_boxplot()


  



  





