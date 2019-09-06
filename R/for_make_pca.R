library(readxl)
setwd('/home/takeo')
# read Copy of Genetics 01_02_2019.xls file for information of All data
pheno <- read_xls("./WES/data/Copy of Genetics 01_02_2019.xls",col_types = "text") 
colnames(pheno) <- make.names(colnames(pheno))
pheno <- pheno %>% 
  distinct(Genetic.ID, .keep_all = TRUE)

# read niddk_vcf_samples_biostats.xlsx file for information of WES data
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
  dplyr::select(kouho)


# read PCA for WES and merge with wes by Seq_ID
vec <- fread("./WES/filtering/hail/PCAclean_rev.eigenvec")
names(vec) <- make.names(names(vec))


wes2 <- wes1 %>% 
  left_join(vec,by = c("Seq_ID" = "IID")) %>% 
  drop_na(Race) %>% 
  filter(Race %in% c("Caucasian","Asian","African American/ Black")) %>% 
  drop_na(PC1) 


wes3 <- wes1 %>% 
  left_join(vec,by = c("Seq_ID" = "IID")) %>% 
  filter(Race == "Caucasian") %>% 
  drop_na(Jewish) 

# plot PCA
p <- ggplot(wes2,aes(PC1,PC2,color = Race)) + geom_point(alpha = 1)
plot(p)
  



