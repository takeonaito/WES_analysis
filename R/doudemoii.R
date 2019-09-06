library(org.Hs.eg.db)
library(data.table)
library("org.Hs.eg.db")
kegg <- org.Hs.egPATH2EG
mapped <- mappedkeys(kegg)
kegg2 <- as.list(kegg[mapped])


gset <- read.gmt("/mnt/share6/FOR_Takeo/WES/SKAT/c2.cp.kegg.v6.2.symbols.gmt")

install.package('qusage')
remove.packages("mvtnorm")
install.packages('survival', lib='/home/takeo/R/x86_64-pc-linux-gnu-library/3.6') 

namae = fread('/mnt/share6/FOR_Takeo/WES/SKAT/VEOIBDlist',header = FALSE)
columns(org.Hs.eg.db)

symbols <- mapIds(org.Hs.eg.db, keys = namae$V1, keytype = "SYMBOL", column="ENTREZID")

data.frame(x = namae$V1, y = symbols)

kouho <- fread("/home/takeo/WES/data/admix_based_caucasian.txt",header = FALSE)

pheno <- fread("/mnt/share6/FOR_Takeo/WES/SKAT/SKAT_target_rev2.txt")

pheno1 <- pheno %>% 
  inner_join(kouho,by = c("IID" = "V1"))

pheno1$GeneticID <- str_replace(pheno1$GeneticID,"-","0")


table(pheno1$CDorHC)
table(pheno$CDorHC)

admix <- fread("/mnt/share6/FOR_Takeo/array_139/admix_with_jewish_nonjewish_prop_added.txt")


pheno2 <- pheno1 %>% 
  left_join(admix,by =c("GeneticID" = "FID"))


phenocon <- pheno2 %>% 
  filter(CDorHC == 1) %>% 
  select(IID.x,GeneticID,Caucasian.x,Caucasian.y)
