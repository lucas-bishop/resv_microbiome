library(tidyverse)
library(ggplot2)
library(broom)
library(cowplot)
library(RColorBrewer)


metadata1 <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  mutate(Hour=factor(Hour))


metadata48hr <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(Hour == '48') %>% 
  mutate(Hour=factor(Hour))


relevant_metadata <- rbind(metadata1,metadata48hr) %>% 
  select(-R1_filename, -R2_filename) %>% 
  filter(!Treatment %in% c("control"))


shared1 <- read_tsv("data/mothur/final.0.03.subsample.shared") %>% 
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>%
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                       'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB', 'S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  pivot_longer(-group, names_to = 'otu', values_to = 'count') %>%
  mutate(count = (count/2))


shared2 <- read_tsv("data/mothur/final.0.03.subsample.shared") %>% 
  select(-label, -numOtus) %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  pivot_longer(-group, names_to = 'otu', values_to = 'count')

##combine the two shared so that relabund column has all numbers normalized to 1
shared <- rbind(shared1, shared2) %>% 
  mutate(relabund = count/1816)
##divide by the lowest read count of relevant samples (1816) multiplied by two to account for tech. replicate


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

###
taxa_metadata <- inner_join(shared_taxa, relevant_metadata)
### 

top_phyla <- c("Firmicutes", "Bacteroidetes", "Proteobacteria",
               "Actinobacteria", "Deferribacteres", "Fusobacteria")


## need to group by stool id, and get read counts for each phylum from each treatment, and make relative to total phyla read counts by stool_id
taxa_metadata %>% 
  filter(phylum %in% top_phyla) %>% 
  ggplot(., aes(x=stool_id, y=count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ phylum) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Donor ID", y = "Read Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 8, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 12, face = "bold"))
 



