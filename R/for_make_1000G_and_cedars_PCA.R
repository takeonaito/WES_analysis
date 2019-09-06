library(tidyverse)
library(readxl)
library(data.table)
library(readr)
# setwd("/home/takeo")

# read 1000G phenotype data
onek_samples <- read_tsv("./WES/data/1000G/igsr_samples.tsv")

colnames(onek_samples) <- make.names(colnames(onek_samples))

# filter only EUR, AFR and EAS
onek_samples <- onek_samples %>%
  filter(Superpopulation.code %in% c("EUR", "AFR", "EAS")) %>%
  mutate(Race = Superpopulation.code, Jewish = NA) %>% 
  mutate(Seq_ID = Sample.name) %>% 
  select(Seq_ID,Race,Jewish) 



# read cedars phenotype dataset 
# (Copy of Genetics 01_02_2019.xls file for information of All data)
pheno <- read_xls("./WES/data/Copy of Genetics 01_02_2019.xls",col_types = "text") 
colnames(pheno) <- make.names(colnames(pheno))
pheno <- pheno %>% 
  distinct(Genetic.ID, .keep_all = TRUE)

# read cedars vcf id file
# (niddk_vcf_samples_biostats.xlsx file for information of WES data)
wes <- read_xlsx("./WES/data/niddk_vcf_samples_biostats.xlsx",
                 col_types = c("text"))
colnames(wes) <- make.names(colnames(wes))

# delete subjects whose genetic ID are not available
wes <- wes %>% 
  drop_na(GeneticID)

# merge pheno and wes by genetic ID
wes1 <- wes %>% 
  left_join(pheno,by = c("GeneticID" = "Genetic.ID"))

# select necessary columns
namae <- names(wes1)
kouho <- namae[c(1,2,4,14,15,16,17,18,21,24)]

wes1 <- wes1 %>% 
  dplyr::select(kouho) %>% 
  dplyr::select(Seq_ID,Race,Jewish)



# merge 2 phenotype datasets (1000G and cedars)

gattai <- rbind(wes1,onek_samples)


#  read PCA value 
vec <- fread("./WES/filtering/hail/PCAmerged.eigenvec")
names(vec) <- make.names(names(vec))


# merge phenotype data (both 1000G and cedars) and PCA data
wes2 <- gattai %>% 
  left_join(vec,by = c("Seq_ID" = "IID")) %>%
  filter(Race %in% c("Caucasian","Asian","African American/ Black","AFR","EAS",
                     "EUR")) %>% 
  drop_na(PC1) 

# merge phenotype data (only 1000g) and PCA dataset
wessen <- onek_samples %>% 
  left_join(vec,by = c("Seq_ID" = "IID")) %>% 
  drop_na(Race) %>% 
  drop_na(PC1) 

# merge phenotype data (only cedars) and PCA dataset
wesc <- wes1 %>% 
  left_join(vec,by = c("Seq_ID" = "IID")) %>% 
  drop_na(Race) %>%
  filter(Race %in% c("Caucasian","Asian","African American/ Black")) %>%
  drop_na(PC1) 

# plot PCA
p <- ggplot(wes2,aes(PC1,PC2,color = Race)) + geom_point(size = 0.5)
plot(p)

p <-  ggplot(wessen,aes(PC1,PC2,color = Race)) + geom_point(size = 0.5)
plot(p)

p <- ggplot(wesc,aes(PC1,PC2,color = Race)) + geom_point(size = 0.5)
plot(p)
