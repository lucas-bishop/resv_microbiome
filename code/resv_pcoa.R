library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

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


pcoa <- read_tsv("data/mothur/final.braycurtis.0.03.lt.pcoa.axes")

metadata_pcoa <- inner_join(relevant_metadata, pcoa, by='group')


ggplot(metadata_pcoa, aes(x=axis1, y=axis2, color = Treatment, shape = Hour)) +
  geom_point(size = 4, alpha = 0.8) +
  facet_wrap(~ stool_id) +
  coord_fixed() +
  labs(title="PCoA of Bray-Curtis dissimilarity",
       subtitle = "0hr raw vs. RWE and Res",
       x="PCo Axis 1",
       y="PCo Axis 2") +
  theme_classic()
