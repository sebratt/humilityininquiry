# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# The data for the regression, but updated PLOS sample here.
# 15,000 says yeaeun for this data. 
# 8/6/2025
# Sarah BRatt
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(readxl)
getwd()
setwd("C:/Users/sebratt/Downloads")
# PloS data that we used for ICSSI (not filtered for medical etc fields)
#s2orc <- read.csv("C:/Users/sebratt/Downloads/dabe890b-0e25-4615-9446-994972bbac87.csv", sep=",", header = TRUE)
yeaeun15k <- read.csv2("C:/Users/sebratt/Downloads/plos_one_other_field_include_medicine_final_df_v3.csv", sep=",", header = TRUE)
View(head((yeaeun15k),n=500))

f <- as.data.frame(table(yeaeun15k$work_id))
summary(f$Freq)

sents_2519 <- yeaeun15k[yeaeun15k$work_id == 'https://openalex.org/W1991990493',]
sents_2300 <- yeaeun15k[yeaeun15k$work_id == 'https://openalex.org/W1963533937',]

View(sents_2300)
# Get unique values
unique_values <- unique(sents_2300$sentence)

# Count the number of unique values
count_unique <- length(unique_values) 

# What is the distribution of the number of sentences
df <- unique(yeaeun15k$num_sent)

df<-as.numeric(df)
summary(df)
plot(df)

less_than_100_sentences <- yeaeun15k[yeaeun15k$num_sent <100,]
less_than_unique_work_ids <-unique(less_than_100_sentences[, c("work_id")])
less_than_unique_sentences <-as.data.frame(unique(less_than_100_sentences$sentence))

hist(df, main="PLOS ONE dataset: # of sentences in each paper",xlab=
       "# sentences in a paper", ylab="# of papers")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Using the PLOS ONE 15428 paper sample, now we can calculate 
# paper-level HI and add regression confounds and such.

# STEP 1: low-hanging fruit.
# Open Alex work_ids can be connected to: 
#     -- ✅paper year 
#     -- ✅author country affiliations 
#     -- team size (number of authors) - group author_id by work_id to get team size 
#     -- ✅fields (already have this YAY -- they are MAG fields??) Level 0 concept

# 
# STEP 2: Calculate the HI measures at the paper-level
#     --Share of sentences that are F (float or %)
#     --Share of sentences that are T (float or %)
#     --Share of sentences that are A (float or %)
#     --Share of sentences that are I (float or %)
#     --Share of H sentences overall (float or %) 
#     (We don't have anti-H calculated for the PLOS ONE sample) 

yeaeun15k <- read.csv2("C:/Users/sebratt/Downloads/plos_one_other_field_include_medicine_final_df_v3.csv", sep=",", header = TRUE)
View(head((yeaeun15k),n=500))

yeaeun_grouped_sent_count <- yeaeun15k %>%
  group_by(work_id) %>%
  summarise(sentences_count = n())

# Group by paper and count total number of prediction is F,T,A,I
yeaeun15k_group <- yeaeun15k %>%
  group_by(work_id, prediction) %>%
  summarize(frequency = n())

unique_work_sent_YK <- yeaeun15k[which(!duplicated(c(yeaeun15k$work_id, yeaeun15k$num_sent))),]
sent_subset_YK <-unique_work_sent_YK[,c(1,4)]

colnames(unique_work_sent_YK)
merge_group <-merge(yeaeun15k_group, sent_subset_YK, by="work_id")
merge2_group <-merge(merge_group, yeaeun_grouped_sent_count, by ="work_id")
merge2_group$diff <-merge2_group$num_sent - merge2_group$sentences_count 

# Plot the distribution of # of papers 
sent_tab <- as.data.frame(table(merge2_group$num_sent))
plot(x=sent_tab$Var1,y=sent_tab$Freq)
hist(merge2_group$sentences_count)
summary(merge2_group$sentences_count)

# Subset for the papers with more than 100 sentences. 
# Let's say the average (scientific lol) sentence has 10 words. <- As this one does! 
# Assuming 10 words per sentence, 100 sentences would be 1000 words. Double-spaced pages: With standard formatting, 1000 words would be approximately 2 pages according to some word count resources
merge3_mas_que_100_words = subset(merge2_group, merge2_group$sentences_count>100)
nrow(merge2_group) -nrow(merge3_mas_que_100_words) #3416 paper difference
# the sentence count seems inaccurate, so can we group by workid and count the frequency? that will give us the sentence classification  
merge2_group$F_share <- round(merge2_group$frequency/merge2_group$sentences_count,2)


# STEP 3: Harder to get data 
#     -- ✅Retractions  per year
#     -- ✅datdper year(see Athena data!) as a proxy for repositories per year?
#     -- See Google sheets for additional IDVs: 
# https://docs.google.com/document/d/1md9b_DlN00YwyoNwBX5yADsSWEbmw_i8/edit?usp=sharing&ouid=112699537395856925800&rtpof=true&sd=true 

# get work ids 

PLOS_ONE_work_ids <- unique(yeaeun15k$work_id)
typeof(PLOS_ONE_work_ids)
setwd("C:/Users/sebratt/Downloads")
write.csv2(PLOS_ONE_work_ids, "PLOSONE_Work_IDS_v2.csv",row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # 
# 8/19/25 (Sunday)
#
# Got year and country variables from Athena !
# Now merge with....need to re-engineer the data so its at the paper level. 
# To do:
#   ✔️  Team size calculate
#   ✔️  Create a comma-sep column for country affiliation of authors 
#   ✔️  merge with Data set counts per year 
#   ✔️  merge with retractions per year 
#
# # # # # # # # # # # # # # # # # # # # # # 
plos_aws <-read.csv("C:/Users/sebratt/Downloads/PLOS_year_country.csv", sep=",", header = TRUE)
View(head(plos_aws))

u <- unique(plos_aws$work_id)
length(u)

tab <-as.data.frame(table(plos_aws$country_code))
year_tab <-as.data.frame(table(plos_aws$year))
plos_aws$work_id <-NULL

colnames(plos_aws)[3] <- "work_id"
#merger <- merge(plos_aws, yeaeun15k, by = "work_id")

# Get Team Size 
plos2_aws <- plos_aws %>%
  group_by(work_id) %>%
  summarise(country_code = str_c(country_code, collapse = ", "),
  author_id = str_c(author_id, collapse = ", ")) # I also do this for author in case we need this as a confound (career age is a predictor of citation impact, maybe humility expressions?)


fb<- plos_aws %>%
  group_by(work_id) %>%
  summarise(distinct_count = n_distinct(author_id))
colnames(fb)[2] <- "team_size"

plos2_aws <-merge(plos2_aws, fb, by="work_id")
plos2_aws$author_id[1] <- unique(plos2_aws$author_id[1])

first <- plos2_aws$country_code[1]
first <- list(first)

unique(first)
typeof(first)

d <- distinct(plos2_aws$country_code, country_code, .keep_all = TRUE)

getwd()
write.csv2(plos2_aws, "plos2_aws.csv",sep = ";",row.names = FALSE)

my_vector <- c(1, 2, 2, 3, 4, 4, 1)
unique_values <- unique(my_vector)
print(unique_values)
typeof(unique_values)

##############################################################
# Jeff Oliver and Sarah Stueve help with unique countries 
# 08/12/25

df <- plos2_aws
country_code_split <- strsplit(x = df$country_code,
                               split = ",")

df$country_code_distinct <- unlist(lapply(X = country_code_split,
                                          FUN = function(x) {
                                            return(paste0(unique(trimws(x)), 
                                                          collapse = ","))
                                          })
)

View(df)

# do the same unique operation for author_id
author_id_split <- strsplit(x = df$author_id,
                               split = ",")

df$author_id_distinct <- unlist(lapply(X = author_id_split,
                                          FUN = function(x) {
                                            return(paste0(unique(trimws(x)), 
                                                          collapse = ","))
                                          })
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Count of datasets per year (we only need 2006-2023)
# 
# Do we want cumulative datasets (do we want every year or aggregated? A: Every year, because) 
# We are trying to capture the influence on the rise of data sharing on
# data sharing requirements ('its in the air') and humility. Being more 
# transparent about scientific processes...
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
getwd()
athena_dataset_counts <- read.csv2("C:/Users/sebratt/Downloads/datasets_aws_yearly_1996_2025.csv", sep=",",header=TRUE)

                         
