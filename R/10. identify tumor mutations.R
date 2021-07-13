# Litterature tumor mutation

library(tidyverse)
library(datapasta)


path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")
nature2018 <-
  readxl::read_xlsx(paste0(path, "/TumorMuts/litterature/Nature Bolli 2018.xlsx")) %>% 
  filter((Variant_class == "Oncogenic"))
nature_bolli <- nature2018 %>% 
  distinct(Sample, Gene) %>% 
  mutate(total = NROW(unique(Sample))) %>% 
  mutate(mutation_count = NROW(unique(Gene))) %>% 
  
  group_by(Gene, total) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(nature_bolli=(count/total*100)) %>% 
  select(Gene, nature_bolli)

cancer_cell_lohr <- 
  readxl::read_xlsx(paste0(path, "/TumorMuts/litterature/Cancer cell 2014.xlsx"), skip = 1)
cancer_cell_lohr1 <- cancer_cell_lohr %>% 
  filter(n_nonsilent != 0) %>% 
  group_by(gene) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(cancercell_lohr=(count/203*100)) %>% 
  select(Gene = gene, cancercell_lohr)
  













jco_walker <-
  data.frame(data=matrix(c("Overall",
                      "HIST1H1E All 2.8 15 1 < 9×10 < 9×10",
                      "IRF4 All 3.2 15 1 < 9×10 < 9×10",
                      "KRAS All 21.1 104 0 < 9×10 < 9×10","MAX All 2.4 13 0 < 9×10 < 9×10",
                      "NRAS All 19.4 91 0 < 9×10 < 9×10",
                      "TP53 All 3.0 16 0 < 9×10 < 9×10","TRAF3 All 3.7 19 0 < 9×10 < 9×10",
                      "FAM46C All 5.6 26 1 5.00 ×10 1.05 ×10",
                      "DIS3 All 8.6 48 1 5.88 ×10 1.11 ×10","BRAF All 6.7 36 0 8.77 ×10 1.50 ×10",
                      "LTB All 3.0 14 1 2.84 ×10 4.46 ×10",
                      "CYLD All 2.4 14 0 6.65 ×10 9.66 ×10",
                      "RB1 All 1.5 7 0 1.96 ×10 2.64 ×10",
                      "t(4;14)",
                      "DIS3 t(4;14) 25.4 18 0 7.05 ×10 1.33 ×10","FGFR3 t(4;14) 16.9 12 0 6.93 ×10 6.54 ×10",
                      "t(11;14)",
                      "KRAS t(11;14) 33.7 31 0 < 9×10 < 9×10",
                      "NRAS t(11;14) 25.6 22 0 1.44 ×10 1.36 ×10",
                      "DIS3 t(11;14) 11.6 12 0 5.74 ×10 3.61 ×10",
                      "IRF4 t(11;14) 10.5 9 0 1.18 ×10 5.57 ×10",
                      "Hyperdiploidy",
                      "KRAS HRD 20.6 54 0 < 9×10 < 9×10","NRAS HRD 24.4 53 0 < 9×10 < 9×10",
                      "FAM46C HRD 7.1 15 1 6.99 ×10 4.40 ×10",
                      "BRAF HRD 6.7 16 0 1.45 ×10 6.85 ×10",
                      "EGR1 HRD 4.6 11 1 4.65 ×10 1.70 ×10","CYLD HRD 2.9 11 0 1.89 ×10 5.09 ×10"), ncol = 1)
  ) %>% 
  mutate(data = str_replace(data, "< ", "<")) %>% 
  separate(col = data, into = c("Gene", "Group", "Samples with mutation in group (%)","Nonsynonymous Mutations (n=)","Synonymous Mutations (n=)", "p-value q-value"), sep = " ") %>% 
  filter(Group == "All") %>% 
  select(Gene, jco_walker = "Samples with mutation in group (%)")


  
  
  
  
  
  
