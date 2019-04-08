library(ggplot2) #for plotting
library(RColorBrewer) #for custom color palettes
library(dplyr) #for manipulating, aggregating, piping
library(tidytext) #for (tidy) text mining
library(topicmodels) #for LDA (topic modeling)
library(reshape2) #for reshaping data (long to wide or wide to long)

brief_summaries=read.csv(choose.files(), header = TRUE)
designs=read.csv(choose.files(),header = TRUE)
interventions=read.csv(choose.files(), header = TRUE)
studies=read.csv(choose.files(), header = TRUE)

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












