############################################################################################ I ### Gene known to be mutated in tumor / Literature 

library(tidyverse)
library(gtsummary)
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

# maf_file <- read_delim(file.choose(), delim = "\t")
# 
# path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger")
# 
# final <- read_csv(paste0(path, "/merging slid ID/List tumor SLID earliest or closest to germline.csv")) %>% 
#   select(avatar_id, SLID_germline, collectiondt_germline, SLID_tumor, collectiondt_tumor) %>% 
#   left_join(., maf_file, by = c("SLID_tumor" = "Tumor_Sample_Barcode"))
# 
# final <- final %>% 
#   mutate(tumor_VAF = t_alt_count / t_depth) %>% 
#   mutate(normal_VAF = n_alt_count / n_depth) %>% 
#   purrr::keep(~!all(is.na(.))) %>% 
#   
#   filter(Variant_Classification != "0", 
#          Variant_Classification != "synonymous_SNV", 
#          Variant_Classification != "nonframeshift_substitution",
#          Variant_Classification != "frameshift_substitution") %>% 
#   
#   select("avatar_id", "SLID_germline", "collectiondt_germline", "SLID_tumor", 
#          "collectiondt_tumor", "Hugo_Symbol", "NCBI_Build", "Chromosome",
#          "Start_Position", "End_Position", "Strand", 
#          "Variant_Classification", "Variant_Type", "Reference_Allele", 
#          "Tumor_Seq_Allele1", "Tumor_Seq_Allele2",
#          "HGVSc", "HGVSp_Short", "Transcript_ID", "Exon_Number", 
#          "tumor_VAF", "normal_VAF",
#          "t_depth", "t_ref_count", "t_alt_count", "n_depth", "n_ref_count",
#          "n_alt_count", everything())
# 
# write_csv(final, paste0(path, "/CHIP in Avatar/TumorMuts/processed data/somatic mutation from Jamie MAF file.csv"))

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "merging slid ID")

tumor <- read_csv(paste0(path, "/List tumor SLID earliest or closest to germline.csv")) %>% 
  select(avatar_id, SLID_tumor) %>% 
  rename(Tumor_Sample_Barcode = SLID_tumor)

germline_patient_data <- readRDS("/Users/colinccm/Documents/GitHub/CHIP-Avatar/germline_patient_data.rds")

germline_patient_data <- germline_patient_data %>% 
  filter(is_patient_MM == "Yes" & is_MMDx_close_to_blood == "Yes") %>% 
  filter(raceeth != "Others") %>% 
  mutate(raceeth = factor(raceeth, levels=c("White Non-Hispanic", "Hispanic", "Black"))) %>% 
  left_join(., tumor, by = "avatar_id") %>% 
  distinct(avatar_id, .keep_all = TRUE) 

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")

gene_list <- c("KRAS", "NRAS", "FAM46C", "DIS3", "BRAF", "TP53", "RYR1", "DNAH5", "LRP1B",
               "TRAF3", "EGR1", "SP140", "PRKD2", "CYLD", "RB1", "IRF4", "CSMD3", "PTCHD3", 
               "AUTS2", "ABI3BP", "GRM7", "PARP4", "BCL7A", "SPEF2", "MYH13", "BRWD3", 
               "MAX", "RPL10", "DDX17", "SAMHD1", "PLD1", "ANKRD26", "ATM", "CCND1", "SETD2")

tumor_mutation <- read_csv(paste0(path, "/TumorMuts/processed data/somatic mutation from Jamie MAF file.csv")) %>% 
  filter(Hugo_Symbol %in% gene_list)

tumor_mutation_avatar <- right_join(tumor_mutation, germline_patient_data, by = "avatar_id")

tumor_mutation_avatar %>% filter(!is.na(Hugo_Symbol)) %>% distinct(avatar_id) %>% nrow()


tumor_mutation_avatar %>% 
  distinct(avatar_id, Hugo_Symbol, .keep_all = TRUE) %>% 
  select(Hugo_Symbol) %>% 
  tbl_summary(sort = list(everything() ~ "frequency")) %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

df <- tumor_mutation_avatar %>% 
  distinct(avatar_id, Hugo_Symbol, .keep_all = TRUE) %>% 
  mutate(KRAS = ifelse(Hugo_Symbol == "KRAS", "Yes", NA_character_)) %>% 
  mutate(NRAS = ifelse(Hugo_Symbol == "NRAS", "Yes", NA_character_)) %>% 
  mutate(FAM46C = ifelse(Hugo_Symbol == "FAM46C", "Yes", NA_character_)) %>% 
  mutate(DIS3 = ifelse(Hugo_Symbol == "DIS3", "Yes", NA_character_)) %>% 
  mutate(BRAF = ifelse(Hugo_Symbol == "BRAF", "Yes", NA_character_)) %>% 
  mutate(TP53 = ifelse(Hugo_Symbol == "TP53", "Yes", NA_character_)) %>% 
  mutate(RYR1 = ifelse(Hugo_Symbol == "RYR1", "Yes", NA_character_)) %>% 
  mutate(DNAH5 = ifelse(Hugo_Symbol == "DNAH5", "Yes", NA_character_)) %>% 
  mutate(LRP1B = ifelse(Hugo_Symbol == "LRP1B", "Yes", NA_character_)) %>% 
  mutate(TRAF3 = ifelse(Hugo_Symbol == "TRAF3", "Yes", NA_character_)) %>% 
  mutate(EGR1 = ifelse(Hugo_Symbol == "EGR1", "Yes", NA_character_)) %>% 
  mutate(SP140 = ifelse(Hugo_Symbol == "SP140", "Yes", NA_character_)) %>% 
  mutate(PRKD2 = ifelse(Hugo_Symbol == "PRKD2", "Yes", NA_character_)) %>% 
  mutate(CYLD = ifelse(Hugo_Symbol == "CYLD", "Yes", NA_character_)) %>% 
  mutate(RB1 = ifelse(Hugo_Symbol == "RB1", "Yes", NA_character_)) %>% 
  mutate(IRF4 = ifelse(Hugo_Symbol == "IRF4", "Yes", NA_character_)) %>% 
  mutate(CSMD3 = ifelse(Hugo_Symbol == "CSMD3", "Yes", NA_character_)) %>% 
  mutate(PTCHD3 = ifelse(Hugo_Symbol == "PTCHD3", "Yes", NA_character_)) %>% 
  mutate(AUTS2 = ifelse(Hugo_Symbol == "AUTS2", "Yes", NA_character_)) %>% 
  mutate(ABI3BP = ifelse(Hugo_Symbol == "ABI3BP", "Yes", NA_character_)) %>% 
  mutate(GRM7 = ifelse(Hugo_Symbol == "GRM7", "Yes", NA_character_)) %>% 
  mutate(PARP4 = ifelse(Hugo_Symbol == "PARP4", "Yes", NA_character_)) %>% 
  mutate(BCL7A = ifelse(Hugo_Symbol == "BCL7A", "Yes", NA_character_)) %>% 
  mutate(SPEF2 = ifelse(Hugo_Symbol == "SPEF2", "Yes", NA_character_)) %>%
  mutate(MYH13 = ifelse(Hugo_Symbol == "MYH13", "Yes", NA_character_)) %>% 
  mutate(BRWD3 = ifelse(Hugo_Symbol == "BRWD3", "Yes", NA_character_)) %>%
  mutate(MAX = ifelse(Hugo_Symbol == "MAX", "Yes", NA_character_)) %>% 
  mutate(RPL10 = ifelse(Hugo_Symbol == "RPL10", "Yes", NA_character_)) %>% 
  mutate(DDX17 = ifelse(Hugo_Symbol == "DDX17", "Yes", NA_character_)) %>%
  mutate(SAMHD1 = ifelse(Hugo_Symbol == "SAMHD1", "Yes", NA_character_)) %>%
  mutate(PLD1 = ifelse(Hugo_Symbol == "PLD1", "Yes", NA_character_)) %>% 
  mutate(ANKRD26 = ifelse(Hugo_Symbol == "ANKRD26", "Yes", NA_character_)) %>% 
  mutate(ATM = ifelse(Hugo_Symbol == "ATM", "Yes", NA_character_)) %>% 
  mutate(CCND1 = ifelse(Hugo_Symbol == "CCND1", "Yes", NA_character_)) %>% 
  mutate(SETD2 = ifelse(Hugo_Symbol == "SETD2", "Yes", NA_character_)) %>% 
  
  select("KRAS", "NRAS", "TP53", "DIS3", "BRAF", "LRP1B", "FAM46C",
         "TRAF3", "CSMD3", "SP140", "CYLD", "DNAH5", "RYR1", 
         "ATM", "MAX", 
         "IRF4", "BCL7A", "CCND1", "EGR1", "AUTS2", "PRKD2", "MYH13", "RB1",
         "PARP4", "SETD2", 
         "ANKRD26", "RPL10", "SAMHD1", "ABI3BP", "BRWD3",
         "GRM7", "PLD1",
         "SPEF2", "DDX17", "PTCHD3", raceeth, avatar_id) %>% 
  group_by(avatar_id) %>% 
  fill(everything(), .direction = "updown") %>% 
  ungroup() %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(-avatar_id) %>% 
  mutate(across(where(is.character), .fns = ~ replace_na(., "No"))) 

tbl <- df %>% 
  select(-raceeth) %>% 
  tbl_summary(sort = list(all_dichotomous() ~ "frequency")) %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

gt::gtsave(tbl, zoom = 1, paste0(path, "/TumorMuts/Output/Tables/Tumor Mutations in MM Avatar.pdf"))

tumor_mutation_avatar %>% 
  distinct(avatar_id, Hugo_Symbol, .keep_all = TRUE) %>% 
  select(Hugo_Symbol, raceeth) %>% 
  tbl_summary(by = raceeth,
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% 
  bold_labels() %>% add_p() %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

tbl <- df %>% 
  tbl_summary(by = raceeth,
              sort = list(everything() ~ "frequency"),
              type = list(all_categorical() ~ "dichotomous"),
              missing = "no") %>% 
  bold_labels() %>% add_p() %>% bold_p(t = .05) %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("**Each gene is counted once per patient**"))

gt::gtsave(tbl, zoom = 1, paste0(path, "/TumorMuts/Output/Tables/Tumor Mutations in MM Avatar by race.pdf"))



maf1 <- 
  read.maf(maf = paste0(path, "/TumorMuts/raw data/Nancy650AF.maf"), 
           vc_nonSyn=c("frameshift_deletion", "frameshift_insertion",
                       "nonframeshift_deletion", "nonframeshift_insertion",
                       "splicing", "stopgain_SNV", "stoploss_SNV", "nonsynonymous_SNV"))

oncoplot(maf1, genes = gene_list, colors = col, sortByAnnotation = TRUE)

maf1 <- 
  read.maf(maf = paste0(path, "/TumorMuts/raw data/Nancy650AF.maf"), 
           vc_nonSyn=c("frameshift_deletion", "frameshift_insertion",
                       "nonframeshift_deletion", "nonframeshift_insertion",
                       "splicing", "stopgain_SNV", "stoploss_SNV", "nonsynonymous_SNV"), 
           clinicalData = germline_patient_data)


disparities_maf <- subsetMaf(maf = maf1, tsb = c(germline_patient_data$Tumor_Sample_Barcode))


plotmafSummary(maf = disparities_maf, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)

# col = RColorBrewer::brewer.pal(n = 11, name = 'Paired')
col = c("yellow", "red", "black", "blue", "yellow", "darkolivegreen", "steelblue2", "darkorchid", "chartreuse3", "maroon1", "darkblue")
# col = viridis(11)
names(col) = c("frameshift_deletion", "frameshift_insertion", "frameshift_substitution", 
               "nonframeshift_deletion", "nonframeshift_insertion", "nonframeshift_substitution", 
               "nonsynonymous_SNV", "splicing", "stopgain_SNV", "stoploss_SNV", "synonymous_SNV")

# tiff(paste0(path, "/TumorMuts/Output/Figures/oncoplot by raceeth.tiff"), height = 600, width = 900)
# pdf(paste0(path, "/TumorMuts/Output/Figures/oncoplot by raceeth.pdf"), height = 6, width = 9)
oncoplot(disparities_maf, genes = gene_list, colors = col,  clinicalFeatures = "raceeth", sortByAnnotation = TRUE)
# dev.off()

oncoplot(disparities_maf, colors = col,  clinicalFeatures = "raceeth", sortByAnnotation = TRUE)



############# CH_calls_genes

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")

CHIP_stat <- read_csv(paste0(path, "/Nancy's working files/CH calls_WES_genes_12.10.20.csv")) %>% 
  mutate(patient_id = str_remove(patient_id, "_normal")) %>% 
  left_join(., patients_clean_tumor, by = c("patient_id" = "SLID_germline")) %>% 
  filter(CH_status == "CH")

median(CHIP_stat$VAF_normal)
range(CHIP_stat$VAF_normal)

length(unique(CHIP_stat$patient_id))

length(CHIP_stat[duplicated(CHIP_stat$patient_id),]$patient_id)

############
load(paste0(path, "/Mingxiang Teng/mmgene.rda"))


pval <- bind_cols(gene = gene35, pval = round(p35, 3))

mmgenes <- 
  bind_cols(gene = gene35, dat35) %>% 
  t() %>% as.data.frame() %>% janitor::row_to_names(1) %>% 
  select(c("BCL7A", "SPEF2", "ANKRD26")) %>% 
  bind_cols(sample = names, .) %>% 
  bind_cols(., raceeth = race) %>% 
  `rownames<-`(NULL) %>% 
  pivot_longer(cols = -c(sample, raceeth), names_to = "gene", values_to = "expression") %>% 
  mutate(expression = as.numeric(expression)) %>% 
  full_join(., pval, by = "gene") %>% 
  mutate(raceeth = case_when(
    raceeth == "White"                     ~ "White Non-Hispanic",
    raceeth == "Hispanic"                  ~ "Hispanic",
    raceeth == "Black"                     ~ "Black"
  )) %>% 
  mutate(raceeth = factor(raceeth, levels=c("White Non-Hispanic", "Hispanic", "Black"))) %>%
  mutate(gene = factor(gene, levels=c("BCL7A", "SPEF2", "ANKRD26"))) %>%
  drop_na(expression)
write_rds(mmgenes, "mmgenes.rds")
mmgenes %>% 
    ggplot(aes(x = raceeth, y = expression, fill = raceeth, color = raceeth))+
    geom_boxplot(
      # color = c("#03051AFF", "blue", "red"),
                 alpha = 0.3,
                 # fill = c("#03051AFF", "blue", "red")
      ) +
    scale_color_manual(values = c("#03051AFF", "blue", "red"))+
    scale_fill_manual(values = c("#03051AFF", "blue", "red"))+
    theme_classic()+
    theme(legend.position = "none", axis.title.x = element_blank())+
    facet_grid(.~ gene)
    
library(ggridges)

bind_cols(gene = gene35, dat35) %>% 
    t() %>% as.data.frame() %>% janitor::row_to_names(1) %>% 
    select(c("BCL7A", "SPEF2", "ANKRD26")) %>% 
    bind_cols(sample = names, .) %>% 
    bind_cols(., raceeth = race) %>% 
    `rownames<-`(NULL) %>% 
    pivot_longer(cols = -c(sample, raceeth), names_to = "gene", values_to = "expression") %>% 
    mutate(expression = as.numeric(expression)) %>% 
    full_join(., pval, by = "gene") %>% 
    mutate(raceeth = case_when(
      raceeth == "White"                     ~ "White Non-Hispanic",
      raceeth == "Hispanic"                  ~ "Hispanic",
      raceeth == "Black"                     ~ "Black"
    )) %>% 
    mutate(raceeth = factor(raceeth, levels=c("White Non-Hispanic", "Hispanic", "Black"))) %>%
    mutate(gene = factor(gene, levels=c("BCL7A", "SPEF2", "ANKRD26"))) %>%
    drop_na(expression) %>% 
  
  ggplot(aes(y=gene, x=expression,  fill=raceeth)) +
  geom_density_ridges(alpha=0.2, scale = 2) +
  scale_fill_manual(values = c("#03051AFF", "blue", "red"))+
  theme_ridges()+
  theme(# axis.text.y = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
  ) 








