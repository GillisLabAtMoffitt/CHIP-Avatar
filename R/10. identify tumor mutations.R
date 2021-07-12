# Litterature tumor mutation

library(tidyverse)

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")
nature2018 <-
  readxl::read_xlsx(paste0(path, "/TumorMuts/litterature/table mut from nature 2018.xlsx")) %>% 
  filter(str_detect(Variant_class , "Oncogenic"))
mutated_gene <- nature2018 %>% 
  distinct(Sample, Gene) %>% 
  group_by(Gene) %>% 
  summarise(nature2018 = n()) %>% 
  arrange(desc(n))



