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
current_data <- subset(study_summary_designs_interventions, select = -c(X.x, X.y))

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

#########################################################################################
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
 #########################################################################################

# Facet wrap by phase: shows the changing proportion of statuses by enrollment for each
ggplot(data = current_data, aes(x = enrollment_level, fill = overall_status)) + geom_bar(position = 'fill') + 
  facet_wrap(~phase) + labs(x = 'Enrollment', y = 'Proportion', fill = 'Overall Status') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Interaction Between Phase and Enrollment')

# showed to Follett

# mosaic plot of Intervention Model and Status
ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_model), fill = overall_status)) + 
  labs(x = 'Intervention Model', y = 'Overall Status', fill = 'Overall Status') + ggtitle("Intervention Models and Overall Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Things to clean:
### Allocation - Random sample needs to join Randomized
### Rename the blank intervention_model, intervention_type, allocation, and primary_purpose rows as "Not Listed" or "Unknown"
### Combine Phases as follows:
  ### Phase 1/Phase 2 -> Phase 1
  ### Phase 2/Phase 3 -> Phase 2
### Reorder phases using current_data$phasef <- factor(current_data$phase, levels = c('Phase 1', 'Phase 2', 'Phase 3', 'Phase 4', 'N/A'))

# mosaic plot of Intervention Type and Status
ggplot(data = current_data) + geom_mosaic(aes(x = product(overall_status, intervention_type), fill = overall_status)) + 
  labs(x = 'Intervention Type', y = 'Overall Status', fill = 'Overall Status') + ggtitle("Intervention Type and Overall Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#fix phases
current_data$phase[current_data$phase=='Early Phase 1']='Phase 1'
current_data$phase[current_data$phase=='Phase 1/Phase 2']='Phase 1'
current_data$phase[current_data$phase=='Phase 2/Phase 3']='Phase 2'

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



