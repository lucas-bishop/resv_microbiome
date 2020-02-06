library(tidyverse)
library(readxl)
library(RColorBrewer)



shared <- read_tsv("sample.final.shared") %>% 
    select(-label, -numOtus) %>% 
    gather(key = "otu", value = "count", -Group) %>% 
    filter(!is.na(count))

p1controls <- shared %>% 
  filter(sample %in% c('DMSOA', 'DMSOB',
                       'Media', 'RESA', 'RESB', 'WEA', 'WEB'))
p2controls <- shared %>% 
  filter(sample %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                       '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB'))
samples <- shared %>% 
  filter(!sample %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                        '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                        'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB'))




metadata <- read_tsv("resv_metadata.txt") %>% 
  rename("group" = "Group") %>% 
  filter(!group %in% c('48hr_DMSOA', '48hr_DMSOB', '48hr_Media',
                        '48hr_RESA', '48hr_RESB', '48hr_WEA', '48hr_WEB',
                        'DMSOA', 'DMSOB', 'Media', 'RESA', 'RESB', 'WEA', 'WEB'))

pcoa <- read_tsv("final.sharedsobs.0.03.lt.pcoa.axes")

metadata_pcoa <- inner_join(metadata, pcoa, by='group')

ggplot(metadata_pcoa, aes(x=axis1, y=axis2, fill = Hour)) +
  geom_point(shape=23, size = 4, alpha = 0.5) +
  coord_fixed() +
  labs(title="PCoA of ThetaYC Distances Between Stool Samples",
       x="PCo Axis 1",
       y="PCo Axis 2") +
  theme_classic()







