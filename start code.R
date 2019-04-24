library(ggplot2) #for plotting
library(RColorBrewer) #for custom color palettes
library(dplyr) #for manipulating, aggregating, piping
library(tidytext) #for (tidy) text mining
library(topicmodels) #for LDA (topic modeling)
library(reshape2) #for reshaping data (long to wide or wide to long)

#read data in
brief_summaries=read.csv(file.choose(), header = TRUE)
designs=read.csv(file.choose(),header = TRUE)
interventions=read.csv(file.choose(), header = TRUE)
studies=read.csv(file.choose(), header = TRUE)


str(brief_summaries)
str(designs)
str(interventions)
str(studies)


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

# summary of intervention type
summary(interventions$x)

# summary of trial status
summary(studies$overall_status)

#merging studies and brief summaries
study_summary <- merge(studies, brief_summaries, by = 'nct_id', all.x = T, all.y = T)
#merging study_summary with designs
study_summary_designs <- merge(study_summary, designs, by ='nct_id', all.x = T, all.y = T)
#merging study_summary_designs with interventions
study_summary_designs_interventions <- merge(study_summary_designs, interventions, by = 'nct_id', all.x = T, all.y = T)

# Removing X.x and X.y as these were just row identifiers for the respective datasets prior to the merge
# renaming the dataset to current_data 
current_data <- subset(study_summary_designs_interventions, select = -c(10,14))

# renaming the column named 'x' to reflect its decription (represents intervention type)
colnames(current_data)[colnames(current_data)=="x"] <- "intervention_type"

# histogram of enrollment -- extremely right skewed
hist(study_summary_designs_interventions$enrollment, breaks = )

# creating column indicating whether or not the trial was terminated
# terminated trial = 1
# completed trial = 0
current_data$status_bin <- 1 #setting it always equal to 1
current_data$status_bin[current_data$overall_status == 'Completed'] <- 0 #when trial was completed, set to 0

temp <- current_data[which(current_data$enrollment >=1000 & current_data$enrollment < Inf ),]
hist(temp$enrollment)

# 1st: 0-21,     26,899
# 2nd: 22-43,    27,459
# 3rd: 44-80,    26,049
# 4th: 81-199,   26,686
# 5th: 200-999,  26,651
# 6th: 1000-end,  7,947

# create levels of enrollment
current_data$enrollment_level <- cut(current_data$enrollment, c(-1,21,43,80,199,999,67128927))

ggplot(data = current_data) +
  geom_bar(aes(x = intervention_model, fill = enrollment_level))

ggplot(data = current_data) +
  geom_bar(aes(x = overall_status, fill = enrollment_level), position = "fill")

ggplot(data = current_data) +
  geom_bar(aes(x = allocation, fill = enrollment_level), position = "fill")
