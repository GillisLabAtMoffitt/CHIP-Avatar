# Litterature tumor mutation

library(tidyverse)
library(datapasta)


path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")

Hoang_2018 <- data.frame(
              stringsAsFactors = FALSE,
                   check.names = FALSE,
                                        Gene = c("KRAS",
                                                 "NRAS","FAM46C","BRAF","TP53",
                                                 "DIS3","PRDM1","SP140",
                                                 "EGR1","TRAF3","ATM","CCND1",
                                                 "HIST1H1E","LTB","IRF4",
                                                 "FGFR3","RB1","ACTG1","CYLD",
                                                 "MAX","ATR","SAMHD1","PRKD2",
                                                 "PTPN11","TGDS","DNAH5","MYH2",
                                                 "BMP2K","ZNF208","RPL10",
                                                 "TBC1D29","FBXO4","RASA2",
                                                 "OR5M1","RPS3A","PTH2","BAX",
                                                 "C8orf86","CELA1","FCF1",
                                                 "FTL","OR9G1","TNFSF12","TRAF2",
                                                 "FAM154B","HIST1H4H","LEMD2",
                                                 "PABPC1","RPN1","SGPP1"),
                `Walker.et.al.%.(n=463)` = c("21*",
                                                 "19*","6*","7*","3*","9*","2",
                                                 "2","4*","4*","3","2*",
                                                 "3*","3*","3*","3*","2","5",
                                                 "2*","2*","1","<1","2",
                                                 "2","1","3","2","1","1",
                                                 "1","2","0","1","0","0","0",
                                                 "<1","<1","0","<1","0",
                                                 "<1","0","1","<1","1","<1",
                                                 "1","0","<1"),
                   `Lohr.et.al.%(n=203)` = c("23*",
                                                 "20*","11*","6*","8*","11*",
                                                 "5*","4","4","5*","4","3",
                                                 "0","1","2","2","3*","2*",
                                                 "2*","1","1","2","3","2",
                                                 "0","5","1","1","3","2",
                                                 "0","1","3","1","0","1",
                                                 "0","0","<1","0","0","<1",
                                                 "<1","2","<1","<1","0","1",
                                                 "0","1"),
                   `Bolli.et.al.%(n=67)` = c("25*",
                                                 "25*","12*","15*","15*","1",
                                                 "0","7*","7","3","3","4",
                                                 "0","4*","0","0","0","0",
                                                 "3","0","1","1","4","0",
                                                 "4","6","0","0","4","0","0",
                                                 "1","3","0","0","0","0",
                                                 "0","0","0","0","0","0",
                                                 "0","0","0","0","0","0",
                                                 "0"),
                       `CoMMpass.%(n=804)` = c("24*",
                                                 "22*","9","7*","5*","10*","2",
                                                 "3*","4","7*","3","2",
                                                 "4*","3","3","3","2*","3",
                                                 "2*","3*","1","2*","3*",
                                                 "2*","2*","5*","3*","2*","3*",
                                                 "2*","2*","1*","1*","1*",
                                                 "1*","1*","<1*","<1*",
                                                 "<1*","<1*","<1*","<1*","<1*",
                                                 "2*","<1*","<1*","<1*","4*",
                                                 "<1*","1*")
              )

Liu_2015 <- data.frame(
                                 stringsAsFactors = FALSE,
                                      check.names = FALSE,
              `Gene Group Samples_with_mutation_in_group_(%)_n=648` = c("HIST1H1E All 2.8",
                                                                  "IRF4 All 3.2","KRAS All 21.1",
                                                                  "MAX All 2.4",
                                                                  "NRAS All 19.4",
                                                                  "TP53 All 3.0",
                                                                  "TRAF3 All 3.7",
                                                                  "FAM46C All 5.6","DIS3 All 8.6",
                                                                  "BRAF All 6.7",
                                                                  "LTB All 3.0",
                                                                  "CYLD All 2.4",
                                                                  "RB1 All 1.5",
                                                                  "DIS3 t(4;14) 25.4",
                                                                  "FGFR3 t(4;14) 16.9",
                                                                  "KRAS t(11;14) 33.7",
                                                                  "NRAS t(11;14) 25.6",
                                                                  "DIS3 t(11;14) 11.6",
                                                                  "IRF4 t(11;14) 10.5",
                                                                  "KRAS HRD 20.6",
                                                                  "NRAS HRD 24.4",
                                                                  "FAM46C HRD 7.1","BRAF HRD 6.7",
                                                                  "EGR1 HRD 4.6",
                                                                  "CYLD HRD 2.9")
            ) %>% 
  separate(col = 'Gene Group Samples_with_mutation_in_group_(%)_n=648', into = c('Gene', 'Group_liu', 'Liu_2015'), sep = " ")

Hollein_2020 <- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
                           `n=211` = c("KRAS (26%)","NRAS (23%)","TP53 (8%)",
                                       "BRAF (4%)","ATM (2%)")
                ) %>% 
  separate(col = 'n=211', into = c('Gene', 'Hollein_2020_n211'), sep = " ") %>% 
  mutate(Hollein_2020_n211 = str_replace_all(Hollein_2020_n211, "\\(|\\)|%", ""))

Chapman_2011 <- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
                            `n=38` = c("NRAS 9","KRAS 10","FAM46C 5",
                                       "DISC3 4","TP53 3","CCND1 2","PNRC1 2",
                                       "ALOX12B 3","HLA-A 2","MAGED1 2")
                ) %>% 
  separate(col = 'n=38', into = c('Gene', 'Hollein_2020_n38'), sep = " ") %>% 
  mutate(Hollein_2020_n38 = round((as.numeric(Hollein_2020_n38) /38) * 100, 2))


bolli_2018 <-
  readxl::read_xlsx(paste0(path, "/TumorMuts/litterature/Nature Bolli 2018.xlsx")) %>% 
  filter((Variant_class == "Oncogenic")) %>% 
  distinct(Sample, Gene) %>% 
  mutate(total = NROW(unique(Sample))) %>% 
  # mutate(mutation_count = NROW(unique(Gene))) %>% 
  
  group_by(Gene, total) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(bolli_2018_n342 = round(count/total*100, 2)) %>% 
  select(Gene, bolli_2018_n342)


Manojlovi_2017 <- data.frame(
                    `GeneName` = c("KRAS 26.8% 23.7% 0.463",
                                                                                                                                    "NRAS 22.8% 21.3% 0.707",
                                                                                                                                    "FAM46C 12.6% 8.3% 0.093",
                                                                                                                                    "RYR1* 9.4% 4.9% 0.045",
                                                                                                                                    "DIS3 7.9% 10.0% 0.465",
                                                                                                                                    "RPL10* 4.7% 1.0% 0.003",
                                                                                                                                    "PTCHD3* 4.7% 1.0% 0.003",
                                                                                                                                    "BCL7A* 3.9% 0.8% 0.007",
                                                                                                                                    "SPEF2* 3.9% 0.8% 0.001",
                                                                                                                                    "SP140 3.9% 2.5% 0.610",
                                                                                                                                    "BRAF 3.9% 8.3% 0.092",
                                                                                                                                    "MAX 3.9% 2.2% 0.319",
                                                                                                                                    "TRAF3 3.9% 6.9% 0.430",
                                                                                                                                    "LRP1B 3.9% 6.6% 0.257",
                                                                                                                                    "PRKD2 3.9% 2.4% 0.319",
                                                                                                                                    "MYH13* 3.9% 0.8% 0.007",
                                                                                                                                    "ABI3BP* 3.9% 1.0% 0.015",
                                                                                                                                    "BRWD3* 3.9% 0.8% 0.007",
                                                                                                                                    "GRM7* 3.9% 1.0% 0.015",
                                                                                                                                    "AUTS2* 3.9% 1.2% 0.028",
                                                                                                                                    "PARP4* 3.9% 1.0% 0.015",
                                                                                                                                    "PLD1* 3.1% 0.3% 0.002",
                                                                                                                                    "ANKRD26* 3.1% 0.2% 0.0002",
                                                                                                                                    "SETD2 3.1% 2.5% 0.697",
                                                                                                                                    "SAMHD1 2.4% 1.9% 0.608",
                                                                                                                                    "CSMD3 3.1% 6.4% 0.153",
                                                                                                                                    "DDX17* 3.1% 0.7% 0.016",
                                                                                                                                    "STXBP4* 3.1% 0.0% 0.00001",
                                                                                                                                    "RB1 2.4% 1.0% 0.152",
                                                                                                                                    "CYLD 1.6% 2.5% 0.518",
                                                                                                                                    "TP53* 1.6% 6.3% 0.035",
                                                                                                                                    "HIST1H4B 1.6% 0.3% 0.090",
                                                                                                                                    "TBC1D29 0.8% 1.7% 0.452",
                                                                                                                                    "ZNF292 0.8% 2.9% 0.172",
                                                                                                                                    "PABPC1 0.8% 2.7% 0.226",
                                                                                                                                    "RPL5 0.8% 1.4% 0.522",
                                                                                                                                    "FAM111B 0.0% 0.2% 0.643",
                                                                                                                                    "FBXO4 0.0% 0.5% 0.422",
                                                                                                                                    "TRIAP1 0.0% 0.2% 0.643",
                                                                                                                                    "TGDS 0.0% 1.5% 0.162",
                                                                                                                                    "KDM3B 0.0% 2.5% 0.070",
                                                                                                                                    "HIST1H2BH 0.0% 0.8% 0.299",
                                                                                                                                    "WDR45 0.0% 1.0% 0.255",
                                                                                                                                    "IRF4* 0.0% 3.2% 0.041")
                  ) %>% 
  separate(col = 'GeneName', 
           into = c('Gene', 'Manojlovi_2017_AfricanAmerican_n127', 'Manojlovi_2017_Caucasian_n591'), sep = " ") %>% 
  mutate(Manojlovi_2017_AfricanAmerican_n127 = str_replace(Manojlovi_2017_AfricanAmerican_n127, "%", "")) %>% 
  mutate(Manojlovi_2017_Caucasian_n591 = str_replace(Manojlovi_2017_Caucasian_n591, "%", ""))


Somatic_tumor_mutation <- full_join(bolli_2018, Chapman_2011, by = "Gene") %>% 
  full_join(., Hoang_2018, by = "Gene") %>% 
  full_join(., Hollein_2020, by = "Gene") %>% 
  full_join(., Liu_2015, by = "Gene") %>% 
  full_join(., Manojlovi_2017, by = "Gene") %>% 
  arrange(desc(bolli_2018_n342))
  
write_csv(Somatic_tumor_mutation, paste0(path, "/TumorMuts/litterature/Somatic_tumor_mutation.csv"))















# Already in previous papers

# cancer_cell_lohr <- 
#   readxl::read_xlsx(paste0(path, "/TumorMuts/litterature/Cancer cell 2014.xlsx"), skip = 1)
# cancer_cell_lohr1 <- cancer_cell_lohr %>% 
#   filter(n_nonsilent != 0) %>% 
#   group_by(gene) %>% 
#   summarise(count = n()) %>% 
#   ungroup() %>% 
#   mutate(cancercell_lohr=(count/203*100)) %>% 
#   select(Gene = gene, cancercell_lohr)

# jco_walker <-
#   data.frame(data=matrix(c("Overall",
#                       "HIST1H1E All 2.8 15 1 < 9×10 < 9×10",
#                       "IRF4 All 3.2 15 1 < 9×10 < 9×10",
#                       "KRAS All 21.1 104 0 < 9×10 < 9×10","MAX All 2.4 13 0 < 9×10 < 9×10",
#                       "NRAS All 19.4 91 0 < 9×10 < 9×10",
#                       "TP53 All 3.0 16 0 < 9×10 < 9×10","TRAF3 All 3.7 19 0 < 9×10 < 9×10",
#                       "FAM46C All 5.6 26 1 5.00 ×10 1.05 ×10",
#                       "DIS3 All 8.6 48 1 5.88 ×10 1.11 ×10","BRAF All 6.7 36 0 8.77 ×10 1.50 ×10",
#                       "LTB All 3.0 14 1 2.84 ×10 4.46 ×10",
#                       "CYLD All 2.4 14 0 6.65 ×10 9.66 ×10",
#                       "RB1 All 1.5 7 0 1.96 ×10 2.64 ×10",
#                       "t(4;14)",
#                       "DIS3 t(4;14) 25.4 18 0 7.05 ×10 1.33 ×10","FGFR3 t(4;14) 16.9 12 0 6.93 ×10 6.54 ×10",
#                       "t(11;14)",
#                       "KRAS t(11;14) 33.7 31 0 < 9×10 < 9×10",
#                       "NRAS t(11;14) 25.6 22 0 1.44 ×10 1.36 ×10",
#                       "DIS3 t(11;14) 11.6 12 0 5.74 ×10 3.61 ×10",
#                       "IRF4 t(11;14) 10.5 9 0 1.18 ×10 5.57 ×10",
#                       "Hyperdiploidy",
#                       "KRAS HRD 20.6 54 0 < 9×10 < 9×10","NRAS HRD 24.4 53 0 < 9×10 < 9×10",
#                       "FAM46C HRD 7.1 15 1 6.99 ×10 4.40 ×10",
#                       "BRAF HRD 6.7 16 0 1.45 ×10 6.85 ×10",
#                       "EGR1 HRD 4.6 11 1 4.65 ×10 1.70 ×10","CYLD HRD 2.9 11 0 1.89 ×10 5.09 ×10"), ncol = 1)
#   ) %>% 
#   mutate(data = str_replace(data, "< ", "<")) %>% 
#   separate(col = data, into = c("Gene", "Group", "Samples with mutation in group (%)","Nonsynonymous Mutations (n=)","Synonymous Mutations (n=)", "p-value q-value"), sep = " ") %>% 
#   filter(Group == "All") %>% 
#   select(Gene, jco_walker = "Samples with mutation in group (%)")


  
  
  
  
  
  
