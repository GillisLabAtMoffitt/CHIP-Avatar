



###################################################################################################  I  ## Venn 2


b <- Global_data$last_date_available[!is.na(Global_data$last_date_available)]
c <- Global_data$Date_of_Birth[!is.na(Global_data$Date_of_Birth)]
d <- Global_data$drug_start_date[!is.na(Global_data$drug_start_date)]
table <-matrix(
  c("nbr of patients born before death", "nbr of patients diag before death", "nbr of patients germline before drugs",
    "nbr of patients germline before bmt1","nbr of patients germline before bmt2", "nbr of patients germline before bmt3",
    "nbr of patients germline before drugs and bmt1", "death date available", "birth date available","combined drug date available",
    sum(str_count(Global_data$check_birthVSdeath, "Yay!!!"), na.rm = TRUE),sum(str_count(Global_data$check_diagVSbirthVSdeath, "Yay!!"), na.rm = TRUE),
    sum(str_count(Global_data$germlineVSdrugs, "Great!!!"), na.rm = TRUE),
    sum(str_count(Global_data$germlineVSbmt1, "Great!!!"), na.rm = TRUE),sum(str_count(Global_data$germlineVSbmt2, "OK"), na.rm = TRUE),
    sum(str_count(Global_data$germlineVSbmt3, "OK"), na.rm = TRUE),sum(str_count(Global_data$germBEFOREdrugsBMT, "Super"), na.rm = TRUE), NROW(b),
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



