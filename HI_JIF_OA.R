# # # # # # # # # # # # # # # # # #
#
# 3/20/25
# updated 4/4/2025
# Sarah B. 
# HI project Open Science  
# Identify H-index of PLoS journals and other journals in OA sample 
# Working from oa_PLoSIdentificationICSSI.R
#
# # # # # # # # # # # # # # # # #
library(readxl)
getwd()


# merge OpenAlex records of OA corpusids and our PloS identified data
s2orc <- read.csv("b2c66cd8-a639-47af-8a48-e8a74a8da0e3.csv", sep=",", header = TRUE)
full_text_s2orc_prediction_openaccess <- read_excel("C:/Users/sebratt/Downloads/full_text_s2orc_prediction_openaccess.xlsx")

hindex <- read.csv("[HI PJ]Datasharing_per_journal - Sheet1.csv", sep=",", header = TRUE)
journal <-as.data.frame(table(m$venue))
colnames(journal)[1] <-"journal.title"
jh <- merge(journal,hindex, by="journal.title")


# add column for OA data sharing journals (PLoS, BMJ) (TRUE/FALSE)
m$contains_PLOS <- grepl("*PLoS* |*PLOS*", m$venue)
m$contains_BMJ <- grepl("*BMJ*", m$venue)
m$contains_Science <-grepl("*Science* |*SCIENCE*", m$venue)
m$contains_PNAS <- grepl("*PNAS*", m$venue)
m$contains_Nature <- grepl("*Nature* |*NATURE*", m$venue)
table(m$contains_Nature) 
m$contains_GenesandDev <-grepl("*Genes & Development*", m$venue)
m$contains_Nucleic_Acids_Research <-grepl("**", m$venue)

d <-as.data.frame(subset(samp,samp$venue == "Nature"))
nrow(d)

# does not require data sharing
m$contains_Cell <- grepl("*Cell*", m$venue)
table(m$contains_Cell)
m$contains_Chemical_Reviews <- grepl("*Chemical Reviews*", m$venue)
table(m$contains_Chemical_Reviews)
m$contains_NEJ # we don't have
m$contains_BJP <- grepl("*British Journal of Pharmacology*", m$venue)
table(m$contains_BJP)
m$contains_Physical_Review_Letters <- grepl("*Physical Review Letter*", m$venue)
table(m$contains_Physical_Review_Letters)
m$contains_BioMed_Research_International <- grepl("*BioMed Research International*", m$venue)
table(m$contains_BioMed_Research_International)
m$contains_BiomedicalOptics <- grepl("*Biomedical Optics*", m$venue)
table(m$contains_BiomedicalOptics)
m$contains_Diabetes_Care <- grepl("*Diabetes Care*", m$venue)
table(m$contains_Diabetes_Care)
m$contains_IntensiveCareMedicine <- grepl("*Intensive Care Medicine*", m$venue)
table(m$contains_IntensiveCareMedicine)
m$contain_Lancet <- grepl("*Lancet*", m$venue)
table(m$contain_Lancet)
m$contains_SoftMatter <- grepl("*Soft Matter*", m$venue)
table(m$contains_SoftMatter)
m$contains_Molecular_biologyevolution <- grepl("*Molecular biology and evolution*", m$venue)
table(m$contains_Molecular_biologyevolution)
m$contains_NeuroImage <- grepl("*NeuroImage*", m$venue)
table(m$contains_NeuroImage)
m$contains_Reports_on_progress_in_physics <- grepl("*Reports on progress in physics*", m$venue)
table(m$contains_Reports_on_progress_in_physics)
m$contains_BMC_Bioinformatics <- grepl("*BMC Bioinformatics*", m$venue)
table(m$contains_BMC_Bioinformatics)
m$contains_IEEE_Transactions_Signal_Processing <- grepl("*IEEE Transactions on Signal Processing*", m$venue)
table(m$contains_IEEE_Transactions_Signal_Processing)
m$contains_Proceedings_IEEE <- grepl("*Proceedings of the IEEE*", m$venue)
table(m$contains_Proceedings_IEEE)
#m$contains_Brain <- grepl("*BRAIN*", m$venue)
#table(m$contains_Brain)

Data_Sharing_Requirement_journal <- subset(m, m$contains_PLOS == TRUE | m$contains_Science == TRUE | 
                                             m$contains_BMJ == TRUE | m$contains_Science == TRUE | 
                                             m$contains_Nature == TRUE)

nrow(Data_Sharing_Requirement_journal)

# We compare journals that we know compare data sharing and those that do not.
# We have been trying to find journals well-represented in the data
# Name of the journal and data sharing policy. If its expectations or for all of the journal
# URL to the data sharing policy and the code Y requires N weaker language around sharing such as expectations or suggest. 
# Journal JIF and SNIP 

not_Data_Sharing_Requirement <-subset(m, m$contains_Cell == TRUE | m$contains_Chemical_Reviews == TRUE | 
                                        m$contains_BJP == TRUE | m$contains_Physical_Review_Letters == TRUE | 
                                        m$contains_BioMed_Research_International == TRUE
                                      | m$contains_BiomedicalOptics == TRUE| m$contains_Diabetes_Care == TRUE
                                      | m$contains_IntensiveCareMedicine == TRUE | m$contains_SoftMatter == TRUE 
                                      | m$contains_Molecular_biologyevolution == TRUE | m$contains_NeuroImage
                                      | m$contains_Reports_on_progress_in_physics == TRUE| m$contains_BMC_Bioinformatics ==TRUE
                                      | m$contains_IEEE_Transactions_Signal_Processing == TRUE | m$contains_Proceedings_IEEE == TRUE)

nrow(not_Data_Sharing_Requirement)

(nrow(Data_Sharing_Requirement_journal)/ nrow(m))*100
(nrow(not_Data_Sharing_Requirement)/ nrow(m))*100

#table(plos$prediction)
No_Data_Sharing_Requirement_Journals <- round((table(not_Data_Sharing_Requirement$prediction)/nrow(not_Data_Sharing_Requirement))*100, 2)
Data_Sharing_Requirement_Journals <- round((table(Data_Sharing_Requirement_journal$prediction)/nrow(Data_Sharing_Requirement_journal))*100,2)


tab <- as.data.frame(rbind(No_Data_Sharing_Requirement_Journals, Data_Sharing_Requirement_Journals))

sum(nrow(Data_Sharing_Requirement_journal) + nrow(not_Data_Sharing_Requirement))/nrow(m)


# significance testing for categorical data. balanced data and 
# load the MASS package
library(MASS)        
print(str(survey))

# Create a data frame from the main data set.
stu_data = data.frame(survey$Smoke,survey$Exer)

# Create a contingency table with the needed variables.           
stu_data = table(survey$Smoke,survey$Exer) 
print(stu_data)




# applying chisq.test() function
print(chisq.test(stu_data))
# As the p-value 0.4828 is greater than the .05, we conclude that the 
# smoking habit is independent of the exercise level of the student and 
# hence there is a weak or no correlation between the two variables. 

# Chi-square test for independence 
# Prepare and format data 
not_Data_Sharing_Requirement$data_share_requirement <- "no data sharing requirement"
Data_Sharing_Requirement_journal$data_share_requirement <- "has data sharing requirement" 

nds <- not_Data_Sharing_Requirement[,c("data_share_requirement","prediction")]
yds <-Data_Sharing_Requirement_journal[,c("data_share_requirement","prediction")]

share_data <- rbind(yds,nds)
data <- table(share_data$data_share_requirement,share_data$prediction) 
options(scipen = 999)
print(chisq.test(data))

#Alternative view
A <- share_data[which(share_data$prediction=='A'),]
A_data <- table(A$data_share_requirement,A$prediction) 
print(chisq.test(A_data))

# None
None <- share_data[which(share_data$prediction=='None'),]
None_data <- table(None$data_share_requirement,None$prediction) 
print(chisq.test(None_data))

print(chisq.test(tab))
chisq.test(tab$A) #significant at p=<0.5 
chisq.test(tab$F) # 0.6097
chisq.test(tab$I) #  0.5779
chisq.test(tab$T) #0.8101
chisq.test(tab$None) #0.6868


# # # # # # # # # # # # # # # # # # 
# journal impact factor list
setwd("C:/Users/sebratt/Downloads")
scimago22 <- read.csv2("scimagojr 2022.csv", header=TRUE)
View(head(scimago22))
colnames(scimago22)


journals <- c("Nature","Science","New England Journal of Medicine","Lancet,The","Cell","Proceedings of the National Academy of Science of the United States of America",
              "Chemical Reviews","Journal of the American Medical Association", "Physical Review Letters","Circulation","Advanced Materials","Angewandte Chemie-International Edition", 
              "Nature Genetics", "Nucleic Acids Research", )

write.csv(journal, "journals_HI_OAsample.csv")













