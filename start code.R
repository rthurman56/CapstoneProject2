## Project 2 -- Group 4
## Claire Peterson, Kaleb Schulz, Tanner Thurman, and Rachel Youngquist

install.packages(c('ggplot2', 'ggmosaic', 'RColorBrewer', 'dplyr', 'tidytext', 'topicmodels', 'reshape2', 
                 'shinydashboard', 'shiny', 'randomForest', 'caret', 'rpart', 'rpart.plot'))

library(ggplot2) #for plotting
library(ggmosaic) #for mosaic plots
library(RColorBrewer) #for custom color palettes
library(dplyr) #for manipulating, aggregating, piping
library(tidytext) #for (tidy) text mining
library(topicmodels) #for LDA (topic modeling)
library(reshape2) #for reshaping data (long to wide or wide to long)
library(shinydashboard) #dashboard
library(shiny) #dashboard
library(randomForest) #random forest algorithm
library(caret) #classification trees
library(rpart) #classification trees
library(rpart.plot) #plotting trees

#read data in
brief_summaries <- read.csv("./data/brief_summaries.csv", header = TRUE)
designs <- read.csv("./data/designs.csv",header = TRUE)
interventions <- read.csv("./data/interventions.csv", header = TRUE)
studies <- read.csv("./data/studies.csv", header = TRUE)


str(brief_summaries)
str(designs)
str(interventions)
str(studies)


########################
## DATA PREPROCESSING ##
########################

brief_summaries$nct_id=as.character(brief_summaries$nct_id)
brief_summaries$description=as.character(brief_summaries$description)


designs$nct_id=as.character(designs$nct_id)
designs$allocation=as.character(designs$allocation)
designs$primary_purpose=as.character(designs$primary_purpose)
designs$intervention_model=as.character(designs$intervention_model)


interventions$nct_id=as.character(interventions$nct_id)
interventions$x=as.character(interventions$x)


studies$nct_id=as.character(studies$nct_id)
studies$overall_status=as.character(studies$overall_status)
studies$brief_title=as.character(studies$brief_title)
studies$start_dt=as.Date(studies$start_dt)
studies$startMonth=as.numeric(as.POSIXlt(studies$start_dt)$mon+1)
studies$startYear=as.numeric(as.POSIXlt(studies$start_dt)$year+1900)

#merging studies and brief summaries
study_summary <- merge(studies, brief_summaries, by = 'nct_id', all.x = T, all.y = T)
#merging study_summary with designs
study_summary_designs <- merge(study_summary, designs, by ='nct_id', all.x = T, all.y = T)
#merging study_summary_designs with interventions
study_summary_designs_interventions <- merge(study_summary_designs, interventions, by = 'nct_id', all.x = T, all.y = T)

# Removing X.x and X.y as these were just row identifiers for the respective datasets prior to the merge
# renaming the dataset to current_data 
current_data <- subset(study_summary_designs_interventions, select = -c(X.x, X.y))

# renaming the column named 'x' to reflect its decription (represents intervention type)
colnames(current_data)[colnames(current_data)=="x"] <- "intervention_type"

# creating column indicating whether or not the trial was terminated
# terminated trial = 1
# completed trial = 0
current_data$status_bin <- 1 #setting it always equal to 1
current_data$status_bin[current_data$overall_status == 'Completed'] <- 0 #when trial was completed, set to 0

# 1st: 3,  7,947

# create levels of enrollment
current_data$enrollment_level <- cut(current_data$enrollment, c(-1,21,43,80,199,999,67128927))


#fix phases
current_data$phase[current_data$phase=='Early Phase 1']='Phase 1'
current_data$phase[current_data$phase=='Phase 1/Phase 2']='Phase 1'
current_data$phase[current_data$phase=='Phase 2/Phase 3']='Phase 2'

# reordering phases
current_data$phasef <- factor(current_data$phase, levels = c('Phase 1', 'Phase 2', 'Phase 3', 'Phase 4', 'N/A'))

# reordering intervention type
# later on, we wanted to have our mosaic plot in order of termination rates so we added this (created new column that is only being used for visualizations)
current_data$intervention_type_factor <- factor(current_data$intervention_type, levels = c('Diagnostic Test', 'Behavioral', 'Dietary Supplement', 'Other', 'Not Listed', 'Genetic', 'Biological', 'Procedure', 'Device', 'Drug', 'Radiation'))


## clean allocation
current_data$allocation[current_data$allocation == 'Random Sample'] <- 'Randomized' #combining random sample with randomized

## rename blank values in allocation to "Not Listed"
current_data$allocation[current_data$allocation == ''] <- 'Not Listed' 
## rename blank values in intervention_model to "Not Listed"
current_data$intervention_model[current_data$intervention_model == ''] <- 'Not Listed' 
## rename NA values in intervention_type to 'Not Listed'
current_data$intervention_type[is.na(current_data$intervention_type)] <- 'Not Listed' 
## rename NA values in intervention_type_factor to 'Not Listed'
current_data$intervention_type_factor[is.na(current_data$intervention_type_factor)] <- 'Not Listed' 
## rename blank values in primary_purpose to 'Not Listed'
current_data$primary_purpose[current_data$primary_purpose == ''] <- 'Not Listed' 
## rename blank values in has_dmc to 'Not Listed'
# convert to character
current_data$has_dmc <- as.character(current_data$has_dmc)
current_data$has_dmc[is.na(current_data$has_dmc)] <- 'Not Listed' 

#########
## LDA ##
#########

#text mining part
tokens <- current_data %>% unnest_tokens(word, description)
#see first few rows - note reach row is now a single description (token)
head(tokens)
data(stop_words)

#adding more words to stop_words
my_stop_words <- data.frame(word = c("patient","patients","study", "studies", 
                                     "treatment", "treatments", "test", "tests", "day", "days", 
                                     "week", "weeks", "month", "months", "year", "years", "purpose",
                                     "clinical", "trial", "trials", "1", "2", "3",
                                     "4", "5", "6", "7", "8", "9"),
                            lexicon = "mine")
stop_words <- rbind(stop_words, my_stop_words)

#remove all rows consisting of a stop description
tokens_clean <- tokens %>% anti_join(stop_words)
tokens_count <- tokens_clean %>%
  #sorts from most frequent to least
  count(word, sort = TRUE) %>%
  #reorders the factor levels for the plot
  mutate(word = reorder(word,n))
#tokens has a row for every description in every review
#tokens_count has a row for every unique description.
#view the first 10 descriptions:
ggplot(data = tokens_count[1:10,]) +
  geom_col(aes(x=word, y=n)) +
  labs(x = "word", y = "Count")+
  coord_flip()


tokens_count <- tokens_clean %>%
  #include id so it counts within unique id
  count(nct_id, status_bin, word, sort = TRUE)%>%
  ungroup()
#tokens_count is a tidy data frame
#to do LDA, we need what is called a "Document Term Matrix" or DTM

dtm <- tokens_count %>%
  cast_dtm(nct_id, word, n)

#lda
lda_one <- LDA(dtm, k = 10, control = list(seed = 1234))


topics_one_word <- tidy(lda_one, matrix = "beta")

## NOTE: commenting out, used this for our purposes to keep the data generated by LDA consistent across laptops
#writing topics to csv to increase run time
# write.csv( topics_one_word, "./topics_two_word.csv")

#per-document-per-topic probabilities
documents <- tidy(lda_one, matrix = "gamma")
documents_w<- documents %>%
  select(document, topic, gamma) %>%
  dcast(document ~ topic, value.var = "gamma")
colnames(documents_w) <- c("nct_id", "BrainScan/Drug", "Care", "TrialExecution", "Cancer", "BloodDiseaseStudy", " QualityofLife", "Surgery", "DrugDosage", " Diabetes ", " BabyVaccine ")
lda_one_word <- merge(documents_w, current_data, by="nct_id", all = T)
str(lda_one_word)

#model probability of a  status_bin review based on topic1 probability
#logit(p) = beta_0 + beta_1*topic1
lda_one_word$overall_status <-I(lda_one_word$overall_status== 'Terminated')
m <- glm(status_bin ~ Topic1 , data = lda_one_word,
         family = binomial)
exp(coef(m))

tokens_tf_idf <- tokens_clean %>%
  count(nct_id, word, sort = TRUE)%>% #count number of times each word shows up in each review
  bind_tf_idf(word, nct_id, n)%>%#use above count (n) to calculate tf, idf, tf-idf
  arrange(desc(tf_idf))
head(tokens_tf_idf)

## NOTE: commenting out, used this for our purposes to keep the data generated by LDA consistent across laptops
#write files to csv for a speedy process
# write.csv(lda_one_word, "./lda_one_word.csv")

################
# LDA TWO WORD #
################

tokens <- current_data %>% unnest_tokens(word, description, token = "ngrams", n= 2)
#see first few rows - note reach row is now a single description (token)
head(tokens)

#adding more words to stop_words
two_stop_words <- data.frame(word = c("with that","patients with","patients that", "it is", 
                                      "is a", "will it", "of the", "this study", "will be", "of this", 
                                      "purpose of", "the purpose", "the trial", "it will", 
                                      "the treatment", "is to", "in the", "the study", "in patients",
                                      "to determine", "evaluate the", "to the", "study will", 
                                      "study is", "to evaluate", "the investigators", "on the", 
                                      "in the", "for that", "of a", "the safety", "and the", "for the",
                                      "in a", "treatment of", "efficacy of", "compared to", "assess the",
                                      "the efficacy", "determine the", "with a", "effects of", 
                                      "with the", "to compare", "safety of", "the effect", "this is",
                                      "at the", "safety and", "to assess", "effect of", "in this",
                                      "the primary", "to be", "has been", "compare the", "effectiveness of",
                                      "aim of", "to investigate", "such as", "associated with", "use of",
                                      "of patients", "as a", "for patients", "objective of", "by the",
                                      "and safety", "the aim", "who have", "and to", "patients who",
                                      "quality of", "patients will", "trial is", "the effects", "may be",
                                      "the effectiveness", "of life", "to a", "can be", "investigate the",
                                      "efficacy and", "determine whether", "study the", "subjects with",
                                      "from the", "dose of", "the first", "in treating", "study to",
                                      "during the", "that the", "is the", "in the", "in healthy", 
                                      "participants will", "designed to", "treatment with", "who are",
                                      "a single", "there is", "is an", "will also", "will receive",
                                      "have been", "study was", "and efficacy", "as well", "the use",
                                      "used in", "and tolerability", "risk of", "and a", "determine if",
                                      "well as", "based on", "subjects will", "primary objective",
                                      "was to", "after the", "one of", "the test", "to test", 
                                      "the objective", "in subjects", "treated with", "up to", "due to",
                                      "number of", "changes in", "used to", "in addition", 
                                      "in combination", "will have", "of treatment", "to study", 
                                      "have a", "test the", "of these", "combination with", "be used",
                                      "administration of", "treatment for", "is not", "is that", "for a",
                                      "and or", "of an", "investigators will", "compared with", "not a",
                                      "at least", "to see", "as the", "a new", "by a", "the most", 
                                      "the patient", "examine the", "this trial", "in order", "for this",
                                      "of two", "order to", "hypothesize that", "if the", "effective in",
                                      "we will", "is designed", "the main", "impact of", "into the", 
                                      "how well", "trial to", "this phase", "goal of", "this research",
                                      "using a", "related to", "is studying", "and after", "the hypothesis",
                                      "the impact", "aims to", "hypothesis is", "that is", "between the",
                                      "whether the", "on a", "the patients", "ways to", "according to",
                                      "there are", "stop the", "also be", "prior to", "people with",
                                      "part of", "patients in", "and will", "and in", "are to", "with type",
                                      "be a", "purpose this", "which is", "and other", "using the", 
                                      "together with", "with an", "lead to", "the following", "end of",
                                      "research study", "to demonstrate", "the risk", "the incidence",
                                      "studies have", "weeks of", "as an", "the current", "do not",
                                      "the two", "a phase", "duration of", "to find", "they will", "study in",
                                      "patients and", "been shown", "study aims", "be assessed", "the goal",
                                      "1 to", "with advanced", "improve the", "in an", "treatment in",
                                      "shown to", "effect on", "of care", "objective is", "assigned to",
                                      "more than", "results in", "result in", "the time", " measured by",
                                      "before and", "risk for", "treatment and", "a prospective", "that are",
                                      "safe and", "more effective", "to provide", "to develop", "the end",
                                      "of their", "the results", "able to", "level of", "period of", 
                                      "the feasibility", "participants with", "and their", "and is", 
                                      "the ability", "conducted in", "the long", "in which", "which is",
                                      "may help", "to prevent", "2 to", "the number", "assessment of",
                                      "patients are", "over the", "pain and", "study are", "and effectiveness",
                                      "as compared", "see if", "type of", "caused by", "is being", "to patients",
                                      "these patients", "to have", "that can", "at a", "are not", "the other",
                                      "about the", "from a", "is effective", "is associated", "work in",
                                      "this project", "with or", "function and", "the potential", 
                                      "well assess", "effects and", "or without", "cause of", "after a",
                                      "through the", "will compare" ,"shown that", "be conducted", 
                                      "is more", "participate in", "terms of", "effects on" ,"ability to",
                                      "in terms", "a study", "blood and", "cells and", "an open", "they are",
                                      "and blood", "the side", "over a", "works in", "used for", "it has",
                                      "months of", "to use", "has not", "a total", "and its", "be compared",
                                      "types of", "will provide", "objective to", "in these", "not been",
                                      "that many", "increase the", "treatment is", "prevalence of",
                                      "to treatment", "to an", "will examine", "results of", "combined with",
                                      "to improve", "of age", "the growth", "growth of", "the combination",
                                      "combination of", "of pain", "find out", "patients undergoing", "years of")
                             ,lexicon = "mine")

#remove all rows consisting of a stop description
tokens_clean <- tokens %>% anti_join(two_stop_words)
tokens_count <- tokens_clean %>%
  #sorts from most frequent to least
  count(word, sort = TRUE) %>%
  #reorders the factor levels for the plot
  mutate(word = reorder(word,n))
#Note: tokens has a row for every description in every review
#tokens_count has a row for every unique description.
#view the first 10 descriptions:
ggplot(data = tokens_count[1:10,]) +
  geom_col(aes(x=word, y=n)) +
  labs(x = "word", y = "Count")+
  coord_flip()

tokens_count <- tokens_clean %>%
  #include id so it counts within unique id
  count(nct_id, status_bin, word, sort = TRUE)%>%
  ungroup()
#tokens_count is a tidy data frame
#to do LDA, we need what is called a "Document Term Matrix" or DTM

dtm <- tokens_count %>%
  cast_dtm(nct_id, word, n)

#lda
lda_two <- LDA(dtm, k = 10, control = list(seed = 1234))

topics_two_word <- tidy(lda_two, matrix = "beta")

## NOTE: commenting out, used this for our purposes to keep the data generated by LDA consistent across laptops
#writing topics to csv to increase run time
# write.csv( topics_two_word, "./topics_two_word.csv")


#per-document-per-topic probabilities
documents <- tidy(lda_two, matrix = "gamma")
documents_w<- documents %>%
  select(document, topic, gamma) %>%
  dcast(document ~ topic, value.var = "gamma")
colnames(documents_w) <- c("nct_id", "HeartHealth", "TumorGrowth", "Hepatitis/StemCell", "Cancer", "PostCare", "BrainStudy", "Diabetes/Types", "DrugDosage", "PhysiologicalEffects", "TrialExecution")
lda_two_word <- merge(documents_w, current_data, by="nct_id", all = T)
str(lda_two_word)

#write files to csv for a speedy process
write.csv(lda_two_word, "./lda_two_word.csv")

## NOTE: commenting out, used this for our purposes to keep the data generated by LDA consistent across laptops
#wrote file to csv, will now read it in from local computer
#added a "_2" to file name to differentiate from created above
#in case different computers generate different data
# lda_one_word_2 <- read.csv("./lda_one_word.csv", header = T)
# lda_two_word_2 <- read.csv("./lda_two_word.csv", header = T)

###########################
#### BEST TOPIC COLUMN ####
###########################

#merge lab_lda with current_data to make one for random forest use
lda_two_word <- lda_two_word[,-c(1, 13:28)]
lda_one_word <- subset(lda_one_word[-c(1,13:29)])

data_lda <- merge(current_data, lda_two_word, by = 'nct_id', all.x = T, all.y = T)
data_lda <- merge(data_lda, lda_one_word, by = 'nct_id', all.x = T, all.y = T)

data_lda$allocation <- as.factor(data_lda$allocation.x)
data_lda$has_dmc <- as.factor(data_lda$has_dmc)
data_lda$primary_purpose <- as.factor(data_lda$primary_purpose)
data_lda$intervention_model <- as.factor(data_lda$intervention_model)
data_lda$intervention_type <- as.factor(data_lda$intervention_type)
data_lda$status_bin = as.factor(data_lda$status_bin)

for(i in 1:length(data_lda$nct_id)){
  maxOneWord = -1
  maxTwoWord = -1
  for(j in 1:length(data_lda)){
    if(j > 18 & j < 29){
      if(data_lda[i,j] > maxTwoWord){
        maxTwoWord <- data_lda[i,j]
        maxTwoCol <- j
      }
    }
    else if(j > 28 & j < 39){
      if(data_lda[i,j] > maxOneWord){
        maxOneWord <- data_lda[i,j]
        maxOneCol <- j
      }
    }
  }
  data_lda$OneWordTopic[i] <- colnames(data_lda)[maxOneCol]
  data_lda$TwoWordTopic[i] <- colnames(data_lda)[maxTwoCol]
}

data_lda$OneWordTopic <- as.factor(data_lda$OneWordTopic)
data_lda$TwoWordTopic <- as.factor(data_lda$TwoWordTopic)

####################
## RANDOM FORESTS ##
####################

smp_sz <- floor(nrow(data_lda)*.4)

first_idx <- sample(seq_len(nrow(data_lda)), size = smp_sz)

smol_df <- data_lda[first_idx,]

smp_sz2 <- floor(nrow(smol_df)*.7)

train_idx <- sample(seq_len(nrow(smol_df)), size = smp_sz2)

train.df <- smol_df[train_idx,]
test.df <- smol_df[-train_idx,]

myForest <- randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                         + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                         + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                         + DrugDosage + PhysiologicalEffects + TrialExecution,
                         data = train.df, 
                         type = "class", 
                         importance = TRUE)
myForest
#OOB rate: 9.76%

TunedForest1 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.71% BEST MODEL ^^^^^
TunedForest2 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.76%
TunedForest3 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.76%
TunedForest4 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.74%
TunedForest5 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.95%
TunedForest6 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.95%
TunedForest7 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.92%
TunedForest8 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.97%
TunedForest9 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)
#OOB rate: 9.95%

TunedForest10 = randomForest(status_bin ~ phasef + enrollment_level + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + Diabetes.Types
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)

#OOB rate: 10.23%



TunedForest11 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer.x + PostCare + BrainStudy + Diabetes.Types
                            + DrugDosage.x + PhysiologicalEffects + TrialExecution.x + BrainScan.Drug + Care + TrialExecution.y 
                            + Cancer.y + BloodDieseasStudy + QulaityofLife + Surgery + DrugDosage.y + Diabetes + BabyVaccine,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)

#OOB rate: 9.91%


#BEST ROC CURVE
TunedForest12 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                             + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                             + TumorGrowth + Hepatitis.StemCell + Cancer.x + PostCare + BrainStudy + Diabetes.Types
                             + DrugDosage.x + PhysiologicalEffects + TrialExecution.x + BrainScan.Drug + Care + TrialExecution.y 
                             + Cancer.y + BloodDieseasStudy + QulaityofLife + Surgery + DrugDosage.y + Diabetes + BabyVaccine,
                             data = train.df, 
                             ntree = 1000,
                             mtry = 5,
                             type = "class",
                             importance = TRUE)

#OOB rate:9.84%


TunedForest13 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                             + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                             + TumorGrowth + Hepatitis.StemCell + Cancer.x + PostCare + BrainStudy + Diabetes.Types
                             + DrugDosage.x + PhysiologicalEffects + TrialExecution.x + BrainScan.Drug + Care + TrialExecution.y 
                             + Cancer.y + BloodDieseasStudy + QulaityofLife + Surgery + DrugDosage.y + Diabetes + BabyVaccine,
                             data = train.df, 
                             ntree = 1000,
                             mtry = 6,
                             type = "class",
                             importance = TRUE)

#OOB rate: 9.8%

#Used the next package for plotting an ROC curve for Random Forest
install.packages("ROCR")
library(ROCR)

#create the prediction object to calculate true positive rate and false positive rate
#to plot them on the ROC curve
TunedForest12.pr = predict(TunedForest12,type="prob",newdata=test.df)[,2]
TunedForest12.pred = prediction(TunedForest12.pr, test.df$status_bin)
TunedForest12.perf = performance(TunedForest12.pred,"tpr","fpr")

#plot the curve
plot(TunedForest12.perf,main="ROC Curve for Random Forest 12",col=2,lwd=2)

#gray line which represents the random guess
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#feature importance plot
varImpPlot(TunedForest12)

#########################
## LOGISTIC REGRESSION ##
#########################
data_lda <- na.omit(data_lda)

#Make the "Drug" level the reference level since it is the most prominent
data_lda$intervention_type <- relevel(data_lda$intervention_type, ref = 7)
#set OneWordTopic ref level to Cancer.y since it has the most rows
data_lda$OneWordTopic <- relevel(data_lda$OneWordTopic, ref = 5)
#set TwoWordTopic ref level to Physiological Effects since it has the most rows
data_lda$TwoWordTopic <- relevel(data_lda$TwoWordTopic, ref = 8)

model1 <- glm(status_bin ~ enrollment_level + intervention_type + phasef + OneWordTopic + TwoWordTopic + enrollment_level*phasef, 
              data = data_lda, 
              family = binomial(link = logit))

summary(model1)

#exponentiate coefficients for interpretations
round(exp(coef(model1)), 3)

###############
## DASHBOARD ##
###############


# for our purposes to keep the data generated by LDA consistent across laptops
# ## read in topics_one_word and topics_two_word here
# topics_one_word_2 <- read.csv("./topics_one_word.csv", header = T)
# topics_two_word_2 <- read.csv("./topics_two_word.csv", header = T)

#status proportions by best one word topic
ggplot(data = data_lda)+
  geom_bar(aes(x = OneWordTopic, fill = overall_status), position = "fill")
#status proportions by best two word topic
ggplot(data = data_lda)+
  geom_bar(aes(x = TwoWordTopic, fill = overall_status), position = "fill")
#proportions of enrollment level by phase
ggplot(data = data_lda)+
  geom_bar(aes(x = phasef, fill = enrollment_level), position = "fill")

#turns topics into top_terms and names topics
#get a small data frame of the top 10 descriptions for each topic
top_terms_one_word <- topics_one_word_2 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_one_word

top_terms_one_word$topic <- factor(top_terms_one_word$topic,
                                   labels = c("BrainScan/Drug", "Care", "TrialExecution", "Cancer", "BloodDiseaseStudy", " QualityofLife", "Surgery", "DrugDosage", " Diabetes ", " BabyVaccine "))

#get a small data frame of the top 10 descriptions for each topic
top_terms_two_word <- topics_two_word_2 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_two_word

top_terms_two_word$topic <- factor(top_terms_two_word$topic,
                                   labels = c("HeartHealth", "Cancer", "PhysiologicalEffects", "Diabetes/Types", "PostPain", "HIV", "PostCare", "DrugDosage", "WeightLoss", "TrialExecution"))


# Begin Dashboard

# Fluid Rows

frow1 <- fluidRow(
  box(title = "Intervention Models and Overall Status"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput("plot1", height = 500))
)

frow2 <- fluidRow(
  #box(title = "test",plotOutput("plot3", height = 250), width = 12, collapsible = TRUE)
  box(title = "Interaction Between Phase and Enrollment"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput("plot3", height = 500))
)

frow3 <- fluidRow(
  box(title = "Enrollment Levels with Overall Status"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput("plot5", height = 500)) 
)

frow4 <- fluidRow(
  box(title = 'LDA - 1 word'
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput('plot6', height = 600))
)

frow5 <- fluidRow(
  box(title = 'LDA - 2 words'
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput('plot7', height = 600))
)

frow6 <- fluidRow(
  box(title = 'Intervention Types and Overall Status'
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput('plot2', height = 500))
)


# Menu to have 3 tabs
menus <-  sidebarMenu(
  menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")), #see tabItem below
  menuItem("Enrollment Level Plots", tabName = "enrollmentplots", icon = icon("dashboard")) #see tabItem below
  ,menuItem("LDA", tabName = 'lda', icon = icon('dashboard'))
)

# UI
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Clinical Trials"),
                    dashboardSidebar(menus),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                h2("Main Dashboard"),
                                frow1,
                                frow6
                        ),
                        tabItem(tabName = "enrollmentplots",
                                h2("Enrollment Levels"),
                                frow3,
                                frow2
                        ),
                        tabItem(tabName = 'lda',
                                h2("LDA Topics"),
                                frow4,
                                frow5
                                ))))

# Server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_model), fill = overall_status)) + 
      labs(x = 'Intervention Model', y = 'Overall Status', fill = 'Overall Status') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), 
            axis.text.y = element_text(size = 14), 
            legend.text = element_text(size = 14), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16))
  })
  output$plot2 <- renderPlot({
    ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_type_factor), fill = overall_status)) + 
      labs(x = 'Intervention Type', y = 'Overall Status', fill = 'Overall Status') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14), 
            legend.text = element_text(size = 14), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16))
  })
  output$plot3 <- renderPlot({
    ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) + geom_bar(position = 'fill') + 
      facet_wrap(~phasef) + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14), 
            legend.text = element_text(size = 14), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16))
  })
  output$plot5 <- renderPlot({
    ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) +
      geom_bar(position = 'fill') + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 14), 
            legend.text = element_text(size = 14), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16))
  })
  # these last two (plot 6 and plot 7) should work once the data is correct
  output$plot6 <- renderPlot({
    top_terms_one_word %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot7 <- renderPlot({
    top_terms_two_word %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
}

# Run Dashboard
shinyApp(ui, server)
