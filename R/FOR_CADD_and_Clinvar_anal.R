res <- fread("./WES/result/anal.anal.glm.logistic") %>% 
  arrange(P)
colnames(res) <- make.names(colnames(res))

dim(res)
31895475-31919861
15:88423463
merge2 %>% 
  filter(CHR == '15') %>% 
  filter(POS > 88423453 & POS < 88423473) %>% 
  arrange(P)

manhattan(res,chr = "X.CHROM",bp ="POS",p = "P",snp = "ID",ylim =c(0,10))


pheno <- read_tsv("/mnt/share6/FOR_Takeo/WES/SKAT/SKAT_target_rev2.txt")
pheno[pheno == -9] <- NA
Clinvar <- read_xlsx("/mnt/share6/FOR_Takeo/WES/VEOIBD/CLINVAR_based_risk_subjects.xlsx")
Clinvar <- Clinvar %>% 
  drop_na(GeneticID)

pheno1 <- pheno %>% 
  drop_na(anal)

pheno2 <- pheno1 %>% 
  left_join(Clinvar,by = "GeneticID")


pheno3 <- pheno2 %>% 
  mutate(allele_number = ifelse(is.na(number_of_alleles),0,number_of_alleles)) %>% 
  select(-number_of_alleles)



ggplot(pheno3,aes(x = allele_number)) + geom_histogram() + 
  scale_x_continuous(breaks = seq(0,5,1), labels = seq(0,5,1))


table(pheno3$anal,pheno3$allele_number)

fisher.test(table(pheno3$anal,pheno3$allele_number))

ggplot(pheno2,aes(x = as.factor(anal), y = number_of_alleles)) + geom_boxplot()


pheno3 <- pheno3 %>% 
  mutate(homo_number = ifelse(is.na(number_of_homo),0,number_of_homo)) %>% 
  select(-number_of_homo)

fisher.test(table(pheno3$anal,pheno3$homo_number,useNA = 'ifany'))


