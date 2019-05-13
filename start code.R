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
brief_summaries=read.csv(file.choose(), header = TRUE)
designs=read.csv(file.choose(),header = TRUE)
interventions=read.csv(file.choose(), header = TRUE)
studies=read.csv(file.choose(), header = TRUE)


str(brief_summaries)
str(designs)
str(interventions)
str(studies)


##############################
## DATA CLEANING AND MERGES ##
##############################

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

# 1st: 0-21,     26,899
# 2nd: 22-43,    27,459
# 3rd: 44-80,    26,049
# 4th: 81-199,   26,686
# 5th: 200-999,  26,651
# 6th: 1000-end,  7,947

# create levels of enrollment
current_data$enrollment_level <- cut(current_data$enrollment, c(-1,21,43,80,199,999,67128927))


#fix phases
current_data$phase[current_data$phase=='Early Phase 1']='Phase 1'
current_data$phase[current_data$phase=='Phase 1/Phase 2']='Phase 1'
current_data$phase[current_data$phase=='Phase 2/Phase 3']='Phase 2'

current_data$phasef <- factor(current_data$phase, levels = c('Phase 1', 'Phase 2', 'Phase 3', 'Phase 4', 'N/A'))


## clean allocation
current_data$allocation[current_data$allocation == 'Random Sample'] <- 'Randomized' #combining random sample with randomized

## rename blank values in allocation to "Not Listed"
current_data$allocation[current_data$allocation == ''] <- 'Not Listed' 
## rename blank values in intervention_model to "Not Listed"
current_data$intervention_model[current_data$intervention_model == ''] <- 'Not Listed' 
## rename NA values in intervention_type to 'Not Listed'
current_data$intervention_type[is.na(current_data$intervention_type)] <- 'Not Listed' 
## rename blank values in primary_purpose to 'Not Listed'
current_data$primary_purpose[current_data$primary_purpose == ''] <- 'Not Listed' 
## rename blank values in has_dmc to 'Not Listed'
# convert to character
current_data$has_dmc <- as.character(current_data$has_dmc)
current_data$has_dmc[is.na(current_data$has_dmc)] <- 'Not Listed' 


#######################
## EXPLORATORY PLOTS ##
#######################

ggplot(data = current_data) +
  geom_bar(aes(x = intervention_model, fill = enrollment_level))

ggplot(data = current_data) +
  geom_bar(aes(x = overall_status, fill = enrollment_level), position = "fill")

ggplot(data = current_data) +
  geom_bar(aes(x = allocation, fill = enrollment_level), position = "fill")


# Claire playing around with categoricals -- will be deleting most

### Exploratory Plots with Intervention Model
# Intervention Model counts by facet wrap on status
ggplot(data = current_data, aes(x = intervention_model, color = intervention_model, fill = intervention_model)) +
  geom_bar() + facet_wrap(~overall_status, scales ='free') + 
  labs(x = 'Intervention Model', y = 'Count') + ggtitle("Completed/Terminated Trials by Intervention Model")

# stacked bar chart of each intervention model and their statuses (stacks)
ggplot(data = current_data, aes(x = intervention_model, color = overall_status, fill = overall_status)) +
  geom_bar() + labs(x = 'Intervention Model', y = 'Count') + ggtitle("Intervention Models with Overall Status")

# stacked bar chart of status with intervention models as stacks
ggplot(data = current_data, aes(x = overall_status, color = intervention_model, fill = intervention_model)) +
  geom_bar() + labs(x = 'Status', y = 'Count') + ggtitle("Status by Intervention Model")

### Exploratory Plots with Intervention Type
# Intervention Type counts by facet wrap on status
ggplot(data = current_data, aes(x = intervention_type, color = intervention_type, fill = intervention_type)) +
  geom_bar() + facet_wrap(~overall_status, scales ='free') + 
  labs(x = 'Intervention Type', y = 'Count') + ggtitle("Completed/Terminated Trials by Intervention Type")

# stacked bar chart of each intervention type and their statuses (stacks)
ggplot(data = current_data, aes(x = intervention_type, color = overall_status, fill = overall_status)) +
  geom_bar() + labs(x = 'Intervention Type', y = 'Count') + ggtitle("Intervention Types with Overall Status")

# stacked bar chart of status with intervention types as stacks
ggplot(data = current_data, aes(x = overall_status, color = intervention_type, fill = intervention_type)) +
  geom_bar() + labs(x = 'Status', y = 'Count') + ggtitle("Status by Intervention Type")

# get the completion rate by each of these #

### Exploratory Plots with Enrollment Level
# stacked bar chart of each enrollment level and their statuses (stacks)
ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) +
  geom_bar(position = 'fill') + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') + ggtitle("Enrollment Levels with Overall Status")

### Exploratory Plots with Phase
# consider grouping these back together and taking N/A out
# Phase counts by facet wrap on status
ggplot(data = current_data, aes(x = phase, color = phase, fill = phase)) +
  geom_bar() + facet_wrap(~overall_status, scales ='free') + 
  labs(x = 'Phase', y = 'Count') + ggtitle("Completed/Terminated Trials by Phase")

# stacked bar chart of each phase and their statuses (stacks)
ggplot(data = current_data, aes(x = phase, color = overall_status, fill = overall_status)) +
  geom_bar() + labs(x = 'Phase', y = 'Count') + ggtitle("Phases with Overall Status")

# stacked bar chart of status with phases as stacks
ggplot(data = current_data, aes(x = overall_status, color = phase, fill = phase)) +
  geom_bar() + labs(x = 'Phase', y = 'Count') + ggtitle("Status by Phase")

### Exploratory Plots with Allocation
# Allocation counts by facet wrap on status
ggplot(data = current_data, aes(x = allocation, fill = allocation)) +
  geom_bar() + facet_wrap(~overall_status, scales ='free') + 
  labs(x = 'Allocation', y = 'Count') + ggtitle("Completed/Terminated Trials by Allocation")
# stacked bar chart of each allocation and their statuses (stacks)
ggplot(data = current_data, aes(x = allocation, fill = overall_status)) +
  geom_bar() + labs(x = 'Allocation', y = 'Count') + ggtitle("Allocations with Overall Status")
# stacked bar chart of status with allocations as stacks
ggplot(data = current_data, aes(x = overall_status, fill = allocation)) +
  geom_bar() + labs(x = 'Allocation', y = 'Count') + ggtitle("Status by Allocation")

# end of claire playing around with categoricals #

# mosaic plot of Intervention Model and Status
ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_model), fill = overall_status)) + 
  labs(x = 'Intervention Model', y = 'Overall Status', fill = 'Overall Status') + ggtitle("Intervention Models and Overall Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mosaic plot of Intervention Type and Status
ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_type), fill = overall_status)) + 
  labs(x = 'Intervention Type', y = 'Overall Status', fill = 'Overall Status') + ggtitle("Intervention Type and Overall Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Facet wrap by phase: shows the changing proportion of statuses by enrollment for each
ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) + geom_bar(position = 'fill') + 
  facet_wrap(~phasef) + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Interaction Between Phase and Enrollment')
# showed to Follett


#####################
## SOME LDA ACTION ##
#####################

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
lda <- LDA(dtm, k = 10, control = list(seed = 1234))


topics_one_word <- tidy(lda, matrix = "beta")

#writing topics to csv to increase run time
write.csv( topics_one_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/topics_two_word.csv")

#get a small data frame of the top 10 descriptions for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#writing top terms to csv to increase run time
write.csv(top_terms_one_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_one_word.csv")

#per-document-per-topic probabilities
documents <- tidy(lda, matrix = "gamma")
documents_w<- documents %>%
  select(document, topic, gamma) %>%
  dcast(document ~ topic, value.var = "gamma")
colnames(documents_w) <- c("nct_id", "Topic1", "Topic2")
lab_lda <- merge(documents_w, current_data, by="nct_id", all = T)
str(lab_lda)

#model probability of a  status_bin review based on topic1 probability
#logit(p) = beta_0 + beta_1*topic1
lab_lda$overall_status <-I(lab_lda$overall_status== 'Terminated')
m <- glm(status_bin ~ Topic1 , data = lab_lda,
         family = binomial)
exp(coef(m))

tokens_tf_idf <- tokens_clean %>%
  count(nct_id, word, sort = TRUE)%>% #count number of times each word shows up in each review
  bind_tf_idf(word, nct_id, n)%>%#use above count (n) to calculate tf, idf, tf-idf
  arrange(desc(tf_idf))
head(tokens_tf_idf)

#write files to csv for a speedy process
lda_one_word <- write.csv(lda_one_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/lda_one_word.csv")
top_terms_one_word <- write.csv(top_terms_one_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_one_word.csv")

######################
#two word text mining#
######################

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
lda <- LDA(dtm, k = 10, control = list(seed = 1234))

topics_two_word <- tidy(lda, matrix = "beta")

#writing topics to csv to increase run time
write.csv( topics_two_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/topics_two_word.csv")

#get a small data frame of the top 10 descriptions for each topic
top_terms_two_word <- topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_two_word

top_terms_two_word$topic <- factor(top_terms_two_word$topic,
                          labels = c("HeartHealth", "TumorGrowth", "Hepatitis/StemCell", "Cancer", "PostCare", "BrainStudy", "Diabetes/Types", "DrugDosage", "PhysiologicalEffects", "TrialExecution"))

top_terms_two_word %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#writing top terms to csv to increase run time
write.csv(top_terms_two_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_two_word.csv")

#per-document-per-topic probabilities
documents <- tidy(lda, matrix = "gamma")
documents_w<- documents %>%
  select(document, topic, gamma) %>%
  dcast(document ~ topic, value.var = "gamma")
colnames(documents_w) <- c("nct_id", "HeartHealth", "TumorGrowth", "Hepatitis/StemCell", "Cancer", "PostCare", "BrainStudy", "Diabetes/Types", "DrugDosage", "PhysiologicalEffects", "TrialExecution")
lab_lda <- merge(documents_w, current_data, by="nct_id", all = T)
str(lab_lda)

#write files to csv for a speedy process
lda_two_word <- write.csv(lda_two_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/lda_two_word.csv")
top_terms_two_word <- write.csv(top_terms_two_word, "C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_two_word.csv")
### wrote file to csv, will now read it in from local computer
lda_one_word <- read.csv("C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/lda_one_word.csv", header = T)
top_terms_one_word <- read.csv("C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_one_word.csv", header = T)
lda_two_word <- read.csv("C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/lda_two_word.csv", header = T)
top_terms_two_word <- read.csv("C:/Users/Rachel Youngquist/Documents/GitHub/CapstoneProject2/top_terms_two_word.csv", header = T)

###########################
## THIS FOREST IS RANDOM ##
###########################

#merge lab_lda with current_data to make one for random forest use
lab_lda = lab_lda[,-c(1, 13:28)]
data_lda <- merge(current_data, lab_lda, by = 'nct_id', all.x = T, all.y = T)


data_lda$allocation <- as.factor(data_lda$allocation.x)
data_lda$has_dmc <- as.factor(data_lda$has_dmc)
data_lda$primary_purpose <- as.factor(data_lda$primary_purpose)
data_lda$intervention_model <- as.factor(data_lda$intervention_model)
data_lda$intervention_type <- as.factor(data_lda$intervention_type)
data_lda$status_bin = as.factor(data_lda$status_bin)



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

TunedForest1 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
TunedForest2 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
TunedForest3 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 500,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)
TunedForest4 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
TunedForest5 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
TunedForest6 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 750,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)
TunedForest7 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 3,
                            type = "class",
                            importance = TRUE)
TunedForest8 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 4,
                            type = "class",
                            importance = TRUE)
TunedForest9 = randomForest(status_bin ~ phasef + enrollment_level + has_dmc + allocation + startMonth 
                            + startYear + primary_purpose + intervention_model + intervention_type + HeartHealth
                            + TumorGrowth + Hepatitis.StemCell + Cancer + PostCare + BrainStudy + DiabetesTypes
                            + DrugDosage + PhysiologicalEffects + TrialExecution,
                            data = train.df, 
                            ntree = 1000,
                            mtry = 5,
                            type = "class",
                            importance = TRUE)




####################
## DASHBOARD TIME ##
####################

frow1 <- fluidRow(
  box(title = "Intervention Models and Overall Status"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 6
      ,plotOutput("plot1", height = 250)),
  box(title = "Intervention Types and Overall Status"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 6
      ,plotOutput("plot2", height = 250))
)

frow2 <- fluidRow(
  #box(title = "test",plotOutput("plot3", height = 250), width = 12, collapsible = TRUE)
  box(title = "Interaction Between Phase and Enrollment"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput("plot3", height = 250))
)

frow3 <- fluidRow(
  box(title = "Distribution of Enrollment by Phase"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 6
      ,plotOutput("plot4", height = 250)),
  box(title = "Enrollment Levels with Overall Status"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = 6
      ,plotOutput("plot5", height = 250)) 
)


frow4 <- fluidRow(
  box(title = 'LDA - 1 topic'
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput('plot6', height = 250))
)

frow5 <- fluidRow(
  box(title = 'LDA - 2 topics'
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,width = 12
      ,plotOutput('plot7', height = 250))
)

menus <-  sidebarMenu(
  menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")), #see tabItem below
  menuItem("Enrollment Level Plots", tabName = "enrollmentplots", icon = icon("dashboard")) #see tabItem below
  ,menuItem("LDA", tabName = 'lda', icon = icon('dashboard'))
)

checkboxes <- checkboxGroupInput("checkGroup",
                                 h3("Enrollment Level:"),
                                 choices = list("0-21" ,
                                                "22-43" ,
                                                "44-80" ,
                                                "81-199" ,
                                                "200-999" ,
                                                "1000+"),
                                 selected = c("(-1,21]" ,
                                              "(21,43]" ,
                                              "(43,80]" ,
                                              "(80,199]" ,
                                              "(199-999]" ,
                                              "(999,6.71e+07]" ))

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Clinical Trials"),
                    dashboardSidebar(
                      checkboxes,
                      menus),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                h2("dashboard tab content"),
                                frow1
                        ),
                        tabItem(tabName = "enrollmentplots",
                                h2("Enrollment Levels"),
                                frow3,
                                frow2
                                #frow4
                        ),
                        tabItem(tabName = 'lda',
                                h2("LDA Topics"),
                                frow4,
                                frow5

                                )
                        )))


server <- function(input, output) {
  # current_data2 <- reactive({
  # subset(current_data, enrollment_level %in% input$checkGroup)
  # }) 
  # if we get this working, then we should use current_data2() as the data for each plot in the server
  output$plot1 <- renderPlot({
    ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_model), fill = overall_status)) + 
      labs(x = 'Intervention Model', y = 'Overall Status', fill = 'Overall Status') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot2 <- renderPlot({
    ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_type), fill = overall_status)) + 
      labs(x = 'Intervention Type', y = 'Overall Status', fill = 'Overall Status') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot3 <- renderPlot({
    ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) + geom_bar(position = 'fill') + 
      facet_wrap(~phasef) + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot4 <- renderPlot({
    ggplot(data = current_data, aes(x = phase, fill = phase)) +
      geom_bar() + facet_wrap(~overall_status, scales ='free') + 
      labs(x = 'Phase', y = 'Count', fill = 'Phase')
  })
  output$plot5 <- renderPlot({
    ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) +
      geom_bar(position = 'fill') + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status')
  })
  output$plot6 <- renderPlot({
    top_terms_oneword_2 %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot7 <- renderPlot({
    top_terms_twoword_2 %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
}

shinyApp(ui, server)
