library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

metadata <- read_tsv("data/mothur/resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(group %in% c('S3', 'S24', 'S26', 'S27', 'S30')) %>% 
  mutate(Hour=factor(Hour))

pcoa <- read_tsv("data/mothur/final.braycurtis.0.03.lt.pcoa.axes")

metadata_pcoa <- inner_join(metadata, pcoa, by='group')


ggplot(metadata_pcoa, aes(x=axis1, y=axis2, color = stool_id)) +
  geom_point(size = 4, alpha = 0.5) +
  coord_fixed() +
  labs(title="PCoA of Bray-Curtis dissimilarity",
       subtitle="Raw stool samples",
       x="PCo Axis 1",
       y="PCo Axis 2") +
  theme_classic()
###Add plot with the 0 hour, no treatment stool samples? If enough reads##

