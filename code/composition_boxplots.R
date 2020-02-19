library(tidyverse)
library(broom)
library(cowplot)
library(RColorBrewer)

metadata1 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  mutate(Hour=factor(Hour))

metadata2 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(Hour == '48') %>% 
  filter(Treatment == 'ResveratrolA') %>% 
  mutate(Hour=factor(Hour))

metadata3 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(Hour == '48') %>% 
  filter(Treatment == 'WineExtractA') %>% 
  mutate(Hour=factor(Hour))

metadata4 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(Hour == '48') %>% 
  filter(Treatment == 'ResveratrolB') %>% 
  mutate(Hour=factor(Hour))

metadata5 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(Hour == '48') %>% 
  filter(Treatment == 'WineExtractB') %>% 
  mutate(Hour=factor(Hour))

relevant_metadata <- rbind(metadata1, metadata2, metadata3, metadata4, metadata5) %>% 
  select(-R1_filename, -R2_filename) %>% 
  filter(!group %in% c('48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB'))
                       
                       
shared <- read_tsv("data/mothur/final.0.03.subsample.shared") %>% 
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>% 
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                       'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB')) %>% 
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


taxa_metadata <- inner_join(shared, taxonomy)

##plot phyla
phylum_taxa <- taxa_metadata %>% 
  group_by(group, phylum) %>% 
  summarize(agg_relabund = sum(relabund)) %>% 
  inner_join(., relevant_metadata, by = 'group') %>% 
  ungroup()

top_phyla <- phylum_taxa %>%
  group_by(phylum) %>% 
  summarize(median = median(agg_relabund)) %>% 
  arrange(desc(median)) %>% 
  top_n(3, median) %>% 
  pull(phylum)

phylum_taxa %>% 
  filter(phylum %in% top_phyla) %>% 
  mutate(agg_relabund = agg_relabund + 1/20000) %>% 
  ggplot(aes(x= reorder(phylum, -agg_relabund), y=agg_relabund, color=Treatment))+
  geom_hline(yintercept=1/1816, color="gray")+
  geom_boxplot()+
  labs(title=NULL, 
       x=NULL,
       y="Relative abundance (%)")+
  scale_y_log10(breaks=c(1e-4, 1e-3, 1e-2, 1e-1, 1), labels=c(1e-2, 1e-1, 1, 10, 100))+
  theme_classic()

##plot genus data

genus_data <- taxa_metadata %>% 
  group_by(group, genus) %>% 
  summarize(agg_relabund=sum(relabund)) %>% 
  inner_join(., relevant_metadata, by = "group") %>% 
  ungroup() 

top_genus <- genus_data %>% 
  group_by(genus) %>% 
  summarize(median=median(agg_relabund)) %>% 
  arrange((desc(median))) %>% 
  top_n(5, median) %>% 
  pull(genus)

genus_data %>% 
  filter(genus %in% top_genus) %>% 
  mutate(agg_relabund = agg_relabund + 1/2000) %>% 
  ggplot(aes(x= reorder(genus, -agg_relabund), y=agg_relabund, color=Treatment))+
  geom_hline(yintercept=1/1816, color="gray")+
  geom_boxplot()+
  labs(title=NULL, 
       x=NULL,
       y="Relative abundance (%)")+
  scale_y_log10(breaks=c(1e-4, 1e-3, 1e-2, 1e-1, 1), labels=c(1e-2, 1e-1, 1, 10, 100))+
  theme(axis.text.x = element_text(angle=45, hjust=1))






