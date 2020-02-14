library(tidyverse)
library(ggplot2)
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

##Read in metadata for all 48hr samples that we care about 
relevant_metadata <- read_tsv("data/mothur/resv_metadata.txt") %>%
  #rbind(metadata1, metadata2, metadata3, metadata4, metadata5) %>% 
  rename("group" = "Group") %>%
  filter(Hour == '48') %>%
  rbind(metadata1) %>%
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


shared_taxa <- inner_join(shared, taxonomy)

taxa_metadata <- inner_join(shared_taxa, relevant_metadata)



##Easily readable colors for top 10
colours = c( "#A54657",  "#582630", "#F7EE7F", "#4DAA57","#F1A66A","#F26157", "#F9ECCC", 
             "#679289", "#33658A","#F6AE2D","#86BBD8")

top_phyla <- c("Firmicutes", "Bacteroidetes", "Proteobacteria",
               "Actinobacteria", "Deferribacteres", "Fusobacteria")


## need to add code make Y axis equal to 100%
taxa_metadata %>% 
  filter(phylum %in% top_phyla) %>% 
  ggplot(., aes(x=Treatment, fill=phylum, y=100*relabund)) +
  ## normalize values to 100
  geom_bar(stat = "identity") +
  facet_wrap(~ stool_id) +
  theme(axis.text.x = element_text(angle = 90, size = 8, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 12, face = "bold")) + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "Relative Abundance (%)", fill = "Phylum") + 
  scale_fill_manual(values = colours)


##add the broth controls and all the other conditions


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






