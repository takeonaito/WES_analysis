setwd("/home/takeo")
# read thiopurine phenotype file and make colnames better
thio <- read_xlsx("./WES/data/Thiopurine toxicityforDalin_v2.xlsx")
colnames(thio) <- make.names(colnames(thio))

# delete duplicate IDs
thio1 <- thio %>% 
  distinct(LAB.ID,.keep_all = TRUE)

# read WES data information (key for IID and genetic ID)
wes <- read_xlsx("./WES/data/niddk_vcf_samples_biostats.xlsx",
                 col_types = c("text"))
colnames(wes) <- make.names(colnames(wes))


# left join and make colnames names better 
wes1 <- wes %>% 
  left_join(thio1,by = c("GeneticID" = "LAB.ID")) %>% 
  dplyr::rename("#IID" = Seq_ID) %>% 
  dplyr::select(-GeneticID,-AKA,-TelAviv_Controls,-comment) %>% 
  dplyr::rename(Leukopenia = Leukopenia..Yes.1..No.0) %>% 
  dplyr::rename(GI.Intolerance = GI.Intolerance..N.V..abd.pain..diarrhea.) %>% 
  dplyr::rename(Allergy = Allergy..rash..high.fever..hypersensitivity.rxn.) %>% 
  dplyr::rename(Flu.like = Flu.like.sx..fever..malaise..nausea..HA.)



# replace u and U into NA
wes1[ifelse(wes1 == "U",TRUE,FALSE)] <- -9
wes1[ifelse(wes1 == "u",TRUE,FALSE)] <- -9

# replace 0 and 1 into 1 and 2
wes1[ifelse(wes1 == 1,TRUE,FALSE)] <- 2
wes1[ifelse(wes1 == 0,TRUE,FALSE)] <- 1

write.table(wes1,file = "./WES/data/target.txt",quote = FALSE, sep = "\t",row.names = FALSE)


