library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)

metadata <- read_tsv("resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                       'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB')) %>% 
  mutate(Hour=factor(Hour))

pcoa <- read_tsv("final.braycurtis.0.03.lt.pcoa.axes")

metadata_pcoa <- inner_join(metadata, pcoa, by='group')

res_rwe_pcoa <- metadata_pcoa %>%
  filter(str_detect(Treatment, "Resveratrol"))


ggplot(res_rwe_pcoa, aes(x=axis1, y=axis2, color = Hour, shape = Hour)) +
  geom_point(size = 4, alpha = 0.5) +
  coord_fixed() +
  labs(title="PCoA of Bray-Curtis dissimilarity Between Resv.-treated Samples",
       x="PCo Axis 1",
       y="PCo Axis 2") +
  theme_classic()
