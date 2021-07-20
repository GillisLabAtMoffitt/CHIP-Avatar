############################################################################################ I ### Gene known to be mutated in tumor / Literature 

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
                `Walker_2015_n463` = c("21*",
                                                 "19*","6*","7*","3*","9*","2",
                                                 "2","4*","4*","3","2*",
                                                 "3*","3*","3*","3*","2","5",
                                                 "2*","2*","1","<1","2",
                                                 "2","1","3","2","1","1",
                                                 "1","2","0","1","0","0","0",
                                                 "<1","<1","0","<1","0",
                                                 "<1","0","1","<1","1","<1",
                                                 "1","0","<1"),
                   `Lohr_2014_n203` = c("23*",
                                                 "20*","11*","6*","8*","11*",
                                                 "5*","4","4","5*","4","3",
                                                 "0","1","2","2","3*","2*",
                                                 "2*","1","1","2","3","2",
                                                 "0","5","1","1","3","2",
                                                 "0","1","3","1","0","1",
                                                 "0","0","<1","0","0","<1",
                                                 "<1","2","<1","<1","0","1",
                                                 "0","1"),
                   `Bolli_2014_n67` = c("25*",
                                                 "25*","12*","15*","15*","1",
                                                 "0","7*","7","3","3","4",
                                                 "0","4*","0","0","0","0",
                                                 "3","0","1","1","4","0",
                                                 "4","6","0","0","4","0","0",
                                                 "1","3","0","0","0","0",
                                                 "0","0","0","0","0","0",
                                                 "0","0","0","0","0","0",
                                                 "0"),
                       `Hoang_2018_CoMMpass_n804` = c("24*",
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
              ) %>% 
  mutate(Walker_2015_n463 =  str_replace(Walker_2015_n463, "\\*", "")) %>% 
  mutate(Lohr_2014_n203 =  str_replace(Lohr_2014_n203, "\\*", "")) %>% 
  
  mutate(Bolli_2014_n67 =  str_replace(Bolli_2014_n67, "\\*", "")) %>% 
  mutate(Hoang_2018_CoMMpass_n804 =  str_replace(Hoang_2018_CoMMpass_n804, "\\*", ""))


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
  separate(col = 'n=38', into = c('Gene', 'Chapman_2011_n38'), sep = " ") %>% 
  mutate(Chapman_2011_n38 = round((as.numeric(Chapman_2011_n38) /38) * 100, 2))


bolli_2018 <-
  readxl::read_xlsx(paste0(path, "/TumorMuts/literature/Bolli Nature 2018.xlsx")) %>% 
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
  mutate(Manojlovi_2017_Caucasian_n591 = str_replace(Manojlovi_2017_Caucasian_n591, "%", "")) %>% 
  mutate(Gene =  str_replace(Gene, "\\*", ""))



Somatic_tumor_mutation <- full_join(bolli_2018, Chapman_2011, by = "Gene") %>% 
  full_join(., Hoang_2018, by = "Gene") %>% 
  full_join(., Hollein_2020, by = "Gene") %>% 
  # full_join(., Walker_2015, by = "Gene") %>% 
  full_join(., Manojlovi_2017, by = "Gene") %>% 
  mutate_at(c( "Lohr_2014_n203", "Walker_2015_n463", "Bolli_2014_n67", 
               "Hoang_2018_CoMMpass_n804" , "Hollein_2020_n211", "Manojlovi_2017_Caucasian_n591", "Manojlovi_2017_AfricanAmerican_n127" ), as.numeric) %>% 
  mutate(average_percent = round(rowMeans(select(., Lohr_2014_n203: Manojlovi_2017_Caucasian_n591), na.rm = TRUE), 2) ) %>% 
  arrange(desc(average_percent)) %>% 
  select(Gene, average_percent, everything())
  
write_csv(Somatic_tumor_mutation, paste0(path, "/TumorMuts/literature/Somatic_tumor_mutation.csv"))

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

# Walker_2015 <- data.frame(
#                                  stringsAsFactors = FALSE,
#                                       check.names = FALSE,
#               `Gene Group Samples_with_mutation_in_group_(%)_n=648` = c("HIST1H1E All 2.8",
#                                                                   "IRF4 All 3.2","KRAS All 21.1",
#                                                                   "MAX All 2.4",
#                                                                   "NRAS All 19.4",
#                                                                   "TP53 All 3.0",
#                                                                   "TRAF3 All 3.7",
#                                                                   "FAM46C All 5.6","DIS3 All 8.6",
#                                                                   "BRAF All 6.7",
#                                                                   "LTB All 3.0",
#                                                                   "CYLD All 2.4",
#                                                                   "RB1 All 1.5")
#             ) %>% 
#   separate(col = 'Gene Group Samples_with_mutation_in_group_(%)_n=648', into = c('Gene', 'Group_Walker_2015', 'Walker_2015'), sep = " ") %>% 
#   select(-Group_Walker_2015)
  


############################################################################################ II ### Investigate these gene in Avatar
library(maftools)

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "merging slid ID")

tumor <- read_csv(paste0(path, "/List tumor SLID earliest or closest to germline.csv")) %>% 
  select(avatar_id, SLID_tumor) %>% 
  rename(Tumor_Sample_Barcode = SLID_tumor)

germline_patient_data <- readRDS("/Users/colinccm/Documents/GitHub/CHIP-Avatar/germline_patient_data.rds")

germline_patient_data <- germline_patient_data %>% 
  filter(is_patient_MM == "Yes" & is_MMDx_close_to_blood == "Yes") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  filter(raceeth != "Others") %>% 
  mutate(raceeth = factor(raceeth, levels=c("White Non-Hispanic", "Hispanic", "Black"))) %>% 
  left_join(., tumor, by = "avatar_id")

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")

gene_list <- c("KRAS", "NRAS", "FAM46C", "DIS3", "BRAF", "TP53", "RYR1", "DNAH5", "LRP1B",
               "TRAF3", "EGR1", "SP140", "PRKD2", "CYLD", "RB1", "IRF4", "CSMD3", "PTCHD3", 
               "AUTS2", "ABI3BP", "GRM7", "PARP4", "BCL7A", "SPEF2", "MYH13", "BRWD3", 
               "MAX", "RPL10", "DDX17", "SAMHD1", "PLD1", "ANKRD26", "ATM", "CCND1", "SETD2")

tumor_mutation <- read_csv(paste0(path, "/TumorMuts/processed data/somatic mutation from Jamie MAF file.csv")) %>% 
  filter(Hugo_Symbol %in% gene_list)

tumor_mutation_avatar <- right_join(tumor_mutation, germline_patient_data, by = "avatar_id")

tbl <- tumor_mutation_avatar %>% 
  distinct(avatar_id, Hugo_Symbol, .keep_all = TRUE) %>% 
  select(Hugo_Symbol) %>% 
  tbl_summary(sort = list(everything() ~ "frequency")) %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

gt::gtsave(tbl, zoom = 1, paste0(path, "/TumorMuts/Output/Tumor Mutations in MM Avatar.pdf"))

tbl <- tumor_mutation_avatar %>% 
  distinct(avatar_id, Hugo_Symbol, .keep_all = TRUE) %>% 
  select(Hugo_Symbol, raceeth) %>% 
  tbl_summary(by = raceeth,
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% 
  bold_labels() %>% add_p() %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

gt::gtsave(tbl, zoom = 1, paste0(path, "/TumorMuts/Output/Tumor Mutations in MM Avatar by race.pdf"))


system.file()

laml.maf <- system.file("extdata", "tcga_laml.maf.gz", package = "maftools")

laml.clin = system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools')
laml <- read.maf(maf = laml.maf, clinicalData = laml.clin)

laml <- 
  read.maf(maf = paste0(path, "/TumorMuts/raw data/Nancy650AF.maf"), 
           vc_nonSyn=c("frameshift_deletion", "frameshift_insertion", "frameshift_substitution", 
                       "nonframeshift_deletion", "nonframeshift_insertion", "nonframeshift_substitution", 
                       "nonsynonymous_SNV", "splicing", "stopgain_SNV", "stoploss_SNV", "synonymous_SNV"))

laml <- 
  read.maf(maf = paste0(path, "/TumorMuts/raw data/Nancy650AF.maf"), 
           vc_nonSyn=c("frameshift_deletion", "frameshift_insertion",
                       "nonframeshift_deletion", "nonframeshift_insertion",
                       "splicing", "stopgain_SNV", "stoploss_SNV", "nonsynonymous_SNV"), 
           clinicalData = germline_patient_data)

plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)

# col = RColorBrewer::brewer.pal(n = 11, name = 'Paired')
col = c("yellow", "red", "black", "blue", "yellow", "darkolivegreen", "steelblue2", "darkorchid", "chartreuse3", "maroon1", "darkblue")
# col = viridis(11)
names(col) = c("frameshift_deletion", "frameshift_insertion", "frameshift_substitution", 
               "nonframeshift_deletion", "nonframeshift_insertion", "nonframeshift_substitution", 
               "nonsynonymous_SNV", "splicing", "stopgain_SNV", "stoploss_SNV", "synonymous_SNV")

# jpeg(paste0(path, "/TumorMuts/Output/Figures/oncoplot by raceeth.jpeg"), height = 600, width = 900)
oncoplot(laml, genes = gene_list, colors = col,  clinicalFeatures = "raceeth", sortByAnnotation = TRUE)
# dev.off()






















