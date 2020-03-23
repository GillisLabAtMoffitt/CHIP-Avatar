library(VennDiagram)
library(RColorBrewer)
###################################################################################################  I  ## Venn 1
# Restart from the Global_data
# Who had BMT or/and drugs in the germline available patient samples
colnames(Global_data)
Global_venn <- Global_data[,c("avatar_id", "TCC_ID", "Date_of_Birth", "date_of_diagnosis_1","disease_stage_1",
                              "number_of_bonemarrow_transplant_1", "number_of_bonemarrow_transplant_2","date_of_first_bmt_1", "date_of_second_bmt_1", "date_of_third_bmt_1", 
                              
                              "collectiondt.germline", "Disease_Status.germline", "collectiondt_1", "Disease_Status_1",
                              
                              "vital_status", "date_death", "date_last_follow_up", "last_date_available", 
                              
                              "prior_treatment_1", "prior_treatment_2",
                              "drug_start_date_1",
                              
                              "rad_start_date_1", "rad_start_date_2", "rad_stop_date_1", "rad_stop_date_2",                  
                              
                              "smoking_status", "alcohol_use",
                              
                              "bmi_at_dx_v2", "Gender", "Ethnicity", "Race", "versionMM_1")]

# nbr of germline collection
germ_available <-  Global_venn[which(!is.na(Global_venn$collectiondt.germline)),]
NROW(germ_available) #510
# nbr tcc id
NROW(which(!is.na(germ_available$TCC_ID))) # 503
# nbr birth
NROW(which(!is.na(germ_available$Date_of_Birth))) # 503
# nbr death
NROW(which(!is.na(germ_available$date_death))) # 80

# nbr had bmt1 
NROW(which(!is.na(germ_available$date_of_first_bmt))) # 240
bmtINgerm <- germ_available[which(!is.na(germ_available$date_of_first_bmt)),]
# nbr had drug1
NROW(which(!is.na(germ_available$drug_start_date))) # 412
drugINgerm <- germ_available[which(!is.na(germ_available$drug_start_date)),]
# nbr commun in bmt1 and drug
had_GERM_BMT_DRUGS <- germ_available[which(!is.na(bmtINgerm$drug_start_date)),] # 240
NROW(which(!is.na(drugINgerm$date_of_first_bmt))) # same



myCol1 <- brewer.pal(3, "Pastel1")
myCol2 <- brewer.pal(3, "Pastel2")

draw.triple.venn(nrow(germ_available), 
                 nrow(bmtINgerm),
                 nrow(drugINgerm),
                 n12 = nrow(bmtINgerm), n23 = nrow(had_GERM_BMT_DRUGS),
                 n13 = nrow(drugINgerm), n123 = nrow(had_GERM_BMT_DRUGS),
                 category = c("all germline", "had BMT1", "had drugs"), 
                 # col = "transparent" make cercle line transparent
                 fill = myCol1, # circle filling color
                 # alpha = c(.2, .3, .3), # circle filling transparency 1 = solide
                 cex = 1, fontface = "bold", fontfamily = "sans",
                 cat.col = c("darkgreen", "red", "blue"), # label color
                 cat.pos = c(-25,5,25), cat.dist = c(0,0,0),
                 cat.cex = 1, cat.fontface = "bold")


###################################################################################################  I  ## Venn 2




colnames(f)
toplotdate <- f

#write.csv(toplotdate, "dates.csv") #-------------------------------------------------------------------------------------Redo
toplotdate$last_date_deathorfollowup  <-  coalesce(toplotdate$date_death, toplotdate$date_last_follow_up) #---------------Redo
b <- toplotdate$last_date_deathorfollowup[!is.na(toplotdate$last_date_deathorfollowup)]

c <- toplotdate$Date_of_Birth[!is.na(toplotdate$Date_of_Birth)]
d <- toplotdate$drug_start_date[!is.na(toplotdate$drug_start_date)]
table <-matrix(
  c("nbr of patients born before death", "nbr of patients diag before death", "nbr of patients germline before drugs",
    "nbr of patients germline before bmt1","nbr of patients germline before bmt2", "nbr of patients germline before bmt3",
    "nbr of patients germline before drugs and bmt1", "death date available", "birth date available","combined drug date available",
    sum(str_count(toplotdate$check_birthVSdeath, "Yay!!!"), na.rm = TRUE),sum(str_count(toplotdate$check_diagVSbirthVSdeath, "Yay!!"), na.rm = TRUE),
    sum(str_count(toplotdate$germlineVSdrugs, "Great!!!"), na.rm = TRUE),
    sum(str_count(toplotdate$germlineVSbmt1, "Great!!!"), na.rm = TRUE),sum(str_count(toplotdate$germlineVSbmt2, "OK"), na.rm = TRUE),
    sum(str_count(toplotdate$germlineVSbmt3, "OK"), na.rm = TRUE),sum(str_count(toplotdate$germBEFOREdrugsBMT, "Super"), na.rm = TRUE), NROW(b),
    NROW(c),NROW(d)),
  ncol = 10, byrow=TRUE)
germline_dates <- as.table(table)
germline_dates
write.csv(germline_dates, "germline_compared_dates in V1and2.csv")

rm(a,b,c,d,e, table)


#########################################################################  VENN 

draw.triple.venn()






# nbr germ before T #----------------------------------------------------------------------make cleanup

# I want all the BMT 1 2 3 in 1 var
bmtINgerm <- germ_available[which(!is.na(germ_available$date_of_first_bmt)),] # need to add 2 and 3
# then look at the ones who had 
# germ before drugs
# germ before 1
# germ before 2
# germ before 3


NROW(which(germ_available$germlineVSdrugs == "OK")) # 112
# nbr Germ and BMT before T
NROW(which(germ_available$GandBmt1BEFOREdrug == "OK")) #1

# nbr bmt before T
NROW(which(germ_available$bmt1BEFOREtreat == "OK")) #3

draw.quad.venn(nrow(germ_available), 
               NROW(a), # germ before drugs
               NROW(b),  # germ before bmt1 germlineVSbmt1
               NROW(c),  # germ before bmt2
               n12= nrow(bmtINgerm), n23 = 240, n13 = nrow(drugINgerm), n123 = 240,
               category = c("all germline", "had prior BMT1", "had prior drugs"), 
               # col = "transparent" make cercle line transparent
               fill = myCol1, # circle filling color
               # alpha = c(.2, .3, .3), # circle filling transparency 1 = solide
               cex = 1, fontface = "bold", fontfamily = "sans",
               cat.col = c("darkgreen", "red", "blue"), # label color
               cat.pos = c(-25,5,25), cat.dist = c(0,0,0),
               cat.cex = 1, cat.fontface = "bold")



