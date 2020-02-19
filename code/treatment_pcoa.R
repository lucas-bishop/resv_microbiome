library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

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


pcoa <- read_tsv("data/mothur/final.braycurtis.0.03.lt.pcoa.axes")

metadata_pcoa <- inner_join(relevant_metadata, pcoa, by='group')


pcoa1 <- ggplot(metadata_pcoa, aes(x=axis1, y=axis2, shape = Hour)) +
  geom_point(size = 4, alpha = 0.7, aes(color = Treatment)) +
  facet_wrap(~ stool_id) +
  coord_fixed() +
  labs(title="PCoA of Bray-Curtis dissimilarity",
       subtitle = "0hr raw vs. 48Hr Treatments",
       x="PCo Axis 1",
       y="PCo Axis 2") +
  theme_classic()
  
pcoa1 + scale_color_brewer(palette = "Set1")



