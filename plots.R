#!/usr/bin/env Rscript

# Title     : Plots for the new quasi-contributors paper
# Objective : Create the plots of the new analyses
# Created by: Filipe FalcÃ£o, filipebatista@ic.ufal.br
# Created on: 25/05/18

#install.packages('ggplot2')
#install.packages('scales')
#install.packages('Hmisc')
#install.packages("reshape")
install.packages("gridExtra")

# Load required packages and suppress packages messages
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
library(gridExtra)
library(Hmisc)
library(reshape)

#' Create a factor feature for the intervals of a given numeric feature
#' in a given dataframe.
#'
#' @param input_data the input dataframe
#' @param feat the feature to create intervals
#'
#' @return the updated dataframe with a new feature for the intervals
create_intervals <- function(input_data, feat) {
  
  # Intervals range
  intervals <- c(0, 1, 30, 180, 365, +Inf)
  
  # Cut the input data
  group_feat_name <- paste0(feat, "_group")
  input_data[group_feat_name] <- cut(as.integer(input_data[[feat]]), intervals)
  return(input_data)
  
}


# Set the output dir
setwd("D:/Pesquisa/icse-2019/metrics/")

#TODO Documentation
#plot with frequency - individually
plot_data <- function(project, input_data, group_feat_name) {
  
  data_as_table <- table(input_data[group_feat_name])
  
  data_as_df <- as.data.frame(data_as_table)
  
  colnames(data_as_df) <- c(group_feat_name, "frequency")
  
  frequency <- (data_as_df$frequency/sum(data_as_df$frequency))*100
  
  data_as_df$frequency <- frequency
  
  #var1 <- colnames(data_as_df)[1]
  #var2 <- colnames(data_as_df)[2]
  
  #file_out <- paste0(group_feat_name, "_")
  #file_out <- paste0(file_out, project)
  #file_out <- paste0(file_out, ".png")
  
  #ggplot(na.omit(data_as_df), aes_string(x = var1, y = var2)) + geom_histogram(stat="identity") + ggsave(file_out)
  #ggplot(data_as_df, aes_string(x = var1, y = var2, fill = factor("A"))) + geom_bar(stat="identity") + xlab("\nType") +  ylab("Time\n") + guides(fill=FALSE) + theme_bw()
  
  return(data_as_df)
}

#TODO Documentation
#plot with frequency - individually
plot_data_stacked <- function(df1, df2, df3, df4, df5, group_feat_name, subtitle) {
  
  frequency_vector <- cbind(df1$frequency, df2$frequency, df3$frequency, df4$frequency, df5$frequency)
  
  projects_data <- as.data.frame(frequency_vector)
  
 
  
  colnames(projects_data) <- c("atom", "electron", "gitlfs", "hubot", "linguist")
  if(grepl("employee", group_feat_name)){
    rownames(projects_data) <- c("Voluntário", "Empregado")
  } else {
    rownames(projects_data) <- c("Primeiro dia", "Até um mês", "Até 6 meses", "Até um ano", "Mais de um ano")
  }
  #print(projects_data)
  projects_data_stacked <- melt(cbind(projects_data, Time = rownames(projects_data)), id.vars = c('Time'))
  #print(dias)

  colnames(projects_data_stacked) <- c("Time", "projects", "Frequencia")
  #print(projects_data_stacked)
  
  file_out <- paste0(group_feat_name, "_")
  file_out <- paste0(file_out, "stacked.png")
  
  ggplot(projects_data_stacked, aes(x = projects, y = Frequencia,fill = Time)) + 
    geom_bar(position = "fill", stat = "identity") +
    xlab(subtitle) +
    scale_y_continuous(labels = percent_format()) + ggsave(file_out)
}

# Features to cut
feats_to_cut <- c("days_opened", "days_between_first_and_last_comment", "days_first_comment_after_creation", "days_last_comment_before_closed")

for (feat in feats_to_cut) {
  
  # Cut feats
  atom_issues_info <- create_intervals(atom_issues_info, feat)
  electron_issues_info <- create_intervals(electron_issues_info, feat)
  gitlfs_issues_info <- create_intervals(gitlfs_issues_info, feat)
  hubot_issues_info <- create_intervals(hubot_issues_info, feat)
  linguist_issues_info <- create_intervals(linguist_issues_info, feat)
  
  atom_pulls_info <- create_intervals(atom_pulls_info, feat)
  electron_pulls_info <- create_intervals(electron_pulls_info, feat)
  gitlfs_pulls_info <- create_intervals(gitlfs_pulls_info, feat)
  hubot_pulls_info <- create_intervals(hubot_pulls_info, feat)
  linguist_pulls_info <- create_intervals(linguist_pulls_info, feat)
  
}

#Issues

# Plot 1: Histogram of days_opened
a <- plot_data("atom", atom_issues_info, 'days_opened_group')
b <- plot_data("electron", electron_issues_info, 'days_opened_group')
c <- plot_data("gitlfs", gitlfs_issues_info, 'days_opened_group')
d <- plot_data("hubot", hubot_issues_info, 'days_opened_group')
e <- plot_data("linguist", linguist_issues_info, 'days_opened_group')
plot_data_stacked(a, b, c, d, e, 'days_opened_group', "Quantidade de dias que Issues ficaram abertas")

# Plot 2: Histogram of days_between_first_and_last_comment
a <- plot_data("atom", atom_issues_info, 'days_between_first_and_last_comment_group')
b <- plot_data("electron", electron_issues_info, 'days_between_first_and_last_comment_group')
c <- plot_data("gitlfs", gitlfs_issues_info, 'days_between_first_and_last_comment_group')
d <- plot_data("hubot", hubot_issues_info, 'days_between_first_and_last_comment_group')
e <- plot_data("linguist", linguist_issues_info, 'days_between_first_and_last_comment_group')
plot_data_stacked(a, b, c, d, e, 'days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo comentário")

# Plot 2: Histogram of days_between_first_and_last_comment
a <- plot_data("atom", atom_issues_info, 'days_between_first_and_last_comment_group')
b <- plot_data("electron", electron_issues_info, 'days_between_first_and_last_comment_group')
c <- plot_data("gitlfs", gitlfs_issues_info, 'days_between_first_and_last_comment_group')
d <- plot_data("hubot", hubot_issues_info, 'days_between_first_and_last_comment_group')
e <- plot_data("linguist", linguist_issues_info, 'days_between_first_and_last_comment_group')
plot_data_stacked(a, b, c, d, e, 'days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo comentário")

# Plot 3: Histogram of days_first_comment_after_creation
a <- plot_data("atom", atom_issues_info, 'days_first_comment_after_creation_group')
b <- plot_data("electron", electron_issues_info, 'days_first_comment_after_creation_group')
c <- plot_data("gitlfs", gitlfs_issues_info, 'days_first_comment_after_creation_group')
d <- plot_data("hubot", hubot_issues_info, 'days_first_comment_after_creation_group')
e <- plot_data("linguist", linguist_issues_info, 'days_first_comment_after_creation_group')
plot_data_stacked(a, b, c, d, e, 'days_first_comment_after_creation_group', "Quantidade de dias entre a criação da Issue e o Primeiro comentário")

# Plot 6: Histogram of days_last_comment_before_closed
a <- plot_data("atom", atom_issues_info, 'days_last_comment_before_closed_group')
b <- plot_data("electron", electron_issues_info, 'days_last_comment_before_closed_group')
c <- plot_data("gitlfs", gitlfs_issues_info, 'days_last_comment_before_closed_group')
d <- plot_data("hubot", hubot_issues_info, 'days_last_comment_before_closed_group')
e <- plot_data("linguist", linguist_issues_info, 'days_last_comment_before_closed_group')
plot_data_stacked(a, b, c, d, e, 'days_last_comment_before_closed_group', "Quantidade de dias entre o Último comentário e o fechamento da Issue")

# Plot 7: Histogram of employee_open
a <- plot_data("atom", atom_issues_info, 'employee_open')
b <- plot_data("electron", electron_issues_info, 'employee_open')
c <- plot_data("gitlfs", gitlfs_issues_info, 'employee_open')
d <- plot_data("hubot", hubot_issues_info, 'employee_open')
e <- plot_data("linguist", linguist_issues_info, 'employee_open')
plot_data_stacked(a, b, c, d, e, 'employee_open', "Abertura de Issues por Empregados e Voluntários")

# Plot 8: Histogram of employee_closed
a <- plot_data("atom", atom_issues_info, 'employee_closed')
b <- plot_data("electron", electron_issues_info, 'employee_closed')
c <- plot_data("gitlfs", gitlfs_issues_info, 'employee_closed')
d <- plot_data("hubot", hubot_issues_info, 'employee_closed')
e <- plot_data("linguist", linguist_issues_info, 'employee_closed')
plot_data_stacked(a, b, c, d, e, 'employee_closed', "Fechamento de Issues por Empregados e Voluntários")

#Pull Requests

# Plot 1: Histogram of days_opened
a <- plot_data("atom", atom_pulls_info, 'days_opened_group')
b <- plot_data("electron", electron_pulls_info, 'days_opened_group')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'days_opened_group')
d <- plot_data("hubot", hubot_pulls_info, 'days_opened_group')
e <- plot_data("linguist", linguist_pulls_info, 'days_opened_group')
plot_data_stacked(a, b, c, d, e, 'days_opened_group', "Quantidade de dias que Pulls ficaram abertas")

# Plot 2: Histogram of days_between_first_and_last_comment
a <- plot_data("atom", atom_pulls_info, 'days_between_first_and_last_comment_group')
b <- plot_data("electron", electron_pulls_info, 'days_between_first_and_last_comment_group')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'days_between_first_and_last_comment_group')
d <- plot_data("hubot", hubot_pulls_info, 'days_between_first_and_last_comment_group')
e <- plot_data("linguist", linguist_pulls_info, 'days_between_first_and_last_comment_group')
plot_data_stacked(a, b, c, d, e, 'days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo comentário da Pull Request")

# Plot 2: Histogram of days_between_first_and_last_comment
a <- plot_data("atom", atom_pulls_info, 'days_between_first_and_last_comment_group')
b <- plot_data("electron", electron_pulls_info, 'days_between_first_and_last_comment_group')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'days_between_first_and_last_comment_group')
d <- plot_data("hubot", hubot_pulls_info, 'days_between_first_and_last_comment_group')
e <- plot_data("linguist", linguist_pulls_info, 'days_between_first_and_last_comment_group')
plot_data_stacked(a, b, c, d, e, 'days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo comentário da Pull Request")

# Plot 3: Histogram of days_first_comment_after_creation
a <- plot_data("atom", atom_pulls_info, 'days_first_comment_after_creation_group')
b <- plot_data("electron", electron_pulls_info, 'days_first_comment_after_creation_group')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'days_first_comment_after_creation_group')
d <- plot_data("hubot", hubot_pulls_info, 'days_first_comment_after_creation_group')
e <- plot_data("linguist", linguist_pulls_info, 'days_first_comment_after_creation_group')
plot_data_stacked(a, b, c, d, e, 'days_first_comment_after_creation_group', "Quantidade de dias entre a submissão da Pull Request e o Primeiro comentário da Pull Request")

# Plot 6: Histogram of days_last_comment_before_closed
a <- plot_data("atom", atom_pulls_info, 'days_last_comment_before_closed_group')
b <- plot_data("electron", electron_pulls_info, 'days_last_comment_before_closed_group')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'days_last_comment_before_closed_group')
d <- plot_data("hubot", hubot_pulls_info, 'days_last_comment_before_closed_group')
e <- plot_data("linguist", linguist_pulls_info, 'days_last_comment_before_closed_group')
plot_data_stacked(a, b, c, d, e, 'days_last_comment_before_closed_group', "Quantidade de dias entre o Último comentário e o merge da Pull Request")

# Plot 7: Histogram of employee_open
a <- plot_data("atom", atom_pulls_info, 'employee_open')
b <- plot_data("electron", electron_pulls_info, 'employee_open')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'employee_open')
d <- plot_data("hubot", hubot_pulls_info, 'employee_open')
e <- plot_data("linguist", linguist_pulls_info, 'employee_open')
plot_data_stacked(a, b, c, d, e, 'employee_open', "Submissões de Pull Requests por Empregados e Voluntários")

# Plot 8: Histogram of employee_closed
a <- plot_data("atom", atom_pulls_info, 'employee_merged')
b <- plot_data("electron", electron_pulls_info, 'employee_merged')
c <- plot_data("gitlfs", gitlfs_pulls_info, 'employee_merged')
d <- plot_data("hubot", hubot_pulls_info, 'employee_merged')
e <- plot_data("linguist", linguist_pulls_info, 'employee_merged')
plot_data_stacked(a, b, c, d, e, 'employee_merged', "Merge de Pull Requests por Empregados e Voluntários")

#Nature

atom_fep_median <- median(atom_nature_metrics$forward_engineering_percent)
atom_cep_median <- median(atom_nature_metrics$corrective_engineering_percent)
atom_rep_median <- median(atom_nature_metrics$reengineering_percent)
atom_unp_median <- median(atom_nature_metrics$uncategorized_percent)
atom_map_median <- median(atom_nature_metrics$management_percent)

atom_fep_mean <- mean(atom_nature_metrics$forward_engineering_percent)
atom_cep_mean <- mean(atom_nature_metrics$corrective_engineering_percent)
atom_rep_mean <- mean(atom_nature_metrics$reengineering_percent)
atom_unp_mean <- mean(atom_nature_metrics$uncategorized_percent)
atom_map_mean <- mean(atom_nature_metrics$management_percent)

atom_nature <- cbind(atom_fep_median, atom_cep_median, atom_rep_median, atom_unp_median, atom_map_median, atom_fep_mean, atom_cep_mean, atom_rep_mean, atom_unp_mean, atom_map_mean)

#
electron_fep_median <- median(electron_nature_metrics$forward_engineering_percent)
electron_cep_median <- median(electron_nature_metrics$corrective_engineering_percent)
electron_rep_median <- median(electron_nature_metrics$reengineering_percent)
electron_unp_median <- median(electron_nature_metrics$uncategorized_percent)
electron_map_median <- median(electron_nature_metrics$management_percent)

electron_fep_mean <- mean(electron_nature_metrics$forward_engineering_percent)
electron_cep_mean <- mean(electron_nature_metrics$corrective_engineering_percent)
electron_rep_mean <- mean(electron_nature_metrics$reengineering_percent)
electron_unp_mean <- mean(electron_nature_metrics$uncategorized_percent)
electron_map_mean <- mean(electron_nature_metrics$management_percent)

electron_nature <- cbind(electron_fep_median, electron_cep_median, electron_rep_median, electron_unp_median, electron_map_median, electron_fep_mean, electron_cep_mean, electron_rep_mean, electron_unp_mean, electron_map_mean)


#
gitlfs_fep_median <- median(gitlfs_nature_metrics$forward_engineering_percent)
gitlfs_cep_median <- median(gitlfs_nature_metrics$corrective_engineering_percent)
gitlfs_rep_median <- median(gitlfs_nature_metrics$reengineering_percent)
gitlfs_unp_median <- median(gitlfs_nature_metrics$uncategorized_percent)
gitlfs_map_median <- median(gitlfs_nature_metrics$management_percent)

gitlfs_fep_mean <- mean(gitlfs_nature_metrics$forward_engineering_percent)
gitlfs_cep_mean <- mean(gitlfs_nature_metrics$corrective_engineering_percent)
gitlfs_rep_mean <- mean(gitlfs_nature_metrics$reengineering_percent)
gitlfs_unp_mean <- mean(gitlfs_nature_metrics$uncategorized_percent)
gitlfs_map_mean <- mean(gitlfs_nature_metrics$management_percent)

#gitlfs_nature_metrics_volunter <- subset(gitlfs_nature_metrics, gitlfs_nature_metrics$site_admin == "false")

gitlfs_nature <- cbind(gitlfs_fep_median, gitlfs_cep_median, gitlfs_rep_median, gitlfs_unp_median, gitlfs_map_median, gitlfs_fep_mean, gitlfs_cep_mean, gitlfs_rep_mean, gitlfs_unp_mean, gitlfs_map_mean)

#
hubot_fep_median <- median(hubot_nature_metrics$forward_engineering_percent)
hubot_cep_median <- median(hubot_nature_metrics$corrective_engineering_percent)
hubot_rep_median <- median(hubot_nature_metrics$reengineering_percent)
hubot_unp_median <- median(hubot_nature_metrics$uncategorized_percent)
hubot_map_median <- median(hubot_nature_metrics$management_percent)

hubot_fep_mean <- mean(hubot_nature_metrics$forward_engineering_percent)
hubot_cep_mean <- mean(hubot_nature_metrics$corrective_engineering_percent)
hubot_rep_mean <- mean(hubot_nature_metrics$reengineering_percent)
hubot_unp_mean <- mean(hubot_nature_metrics$uncategorized_percent)
hubot_map_mean <- mean(hubot_nature_metrics$management_percent)

hubot_nature <- cbind(hubot_fep_median, hubot_cep_median, hubot_rep_median, hubot_unp_median, hubot_map_median, hubot_fep_mean, hubot_cep_mean, hubot_rep_mean, hubot_unp_mean, hubot_map_mean)

#

#linguist_nature_metrics_volunter <- subset(linguist_nature_metrics, linguist_nature_metrics$)

linguist_fep_median <- median(linguist_nature_metrics$forward_engineering_percent)
linguist_cep_median <- median(linguist_nature_metrics$corrective_engineering_percent)
linguist_rep_median <- median(linguist_nature_metrics$reengineering_percent)
linguist_unp_median <- median(linguist_nature_metrics$uncategorized_percent)
linguist_map_median <- median(linguist_nature_metrics$management_percent)

linguist_fep_mean <- mean(linguist_nature_metrics$forward_engineering_percent)
linguist_cep_mean <- mean(linguist_nature_metrics$corrective_engineering_percent)
linguist_rep_mean <- mean(linguist_nature_metrics$reengineering_percent)
linguist_unp_mean <- mean(linguist_nature_metrics$uncategorized_percent)
linguist_map_mean <- mean(linguist_nature_metrics$management_percent)

linguist_nature <- cbind(linguist_fep_median, linguist_cep_median, linguist_rep_median, linguist_unp_median, linguist_map_median, linguist_fep_mean, linguist_cep_mean, linguist_rep_mean, linguist_unp_mean, linguist_map_mean)

# Results Nature

results_nature <- rbind(atom_nature, electron_nature, gitlfs_nature, hubot_nature, linguist_nature)
colnames(results_nature) <- c("fep_median", "cep_median", "rep_median", "unp_median", "map_median", "fep_mean", "cep_mean", "rep_mean", "unp_mean", "map_mean")
rownames(results_nature) <- c("atom", "electron", "gitlfs", "hubot", "linguist")
write.table(results_nature)# grid.table(results_nature)

#png("projects_nature.png", height = 50*nrow(results_nature), width = 200*ncol(results_nature))
#grid.table(results_nature)
write.csv(results_nature, "projects_nature.csv")

#Spearman 
#TODO Tabela

p <- cor(x = as.numeric(atom_issues_info$days_opened), y = as.numeric(atom_issues_info$days_between_first_and_last_comment), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_opened), y = as.numeric(atom_issues_info$number_of_comments), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_opened), y = as.numeric(atom_issues_info$days_first_comment_after_creation), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_opened), y = as.numeric(atom_issues_info$days_last_comment_before_closed), use='complete.obs', method = "spearman")

p <- cor(x = as.numeric(atom_issues_info$days_between_first_and_last_comment), y = as.numeric(atom_issues_info$number_of_comments), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_between_first_and_last_comment), y = as.numeric(atom_issues_info$days_first_comment_after_creation), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_between_first_and_last_comment), y = as.numeric(atom_issues_info$days_last_comment_before_closed), use='complete.obs', method = "spearman")

p <- cor(x = as.numeric(atom_issues_info$number_of_comments), y = as.numeric(atom_issues_info$days_between_first_and_last_comment), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$number_of_comments), y = as.numeric(atom_issues_info$days_first_comment_after_creation), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$number_of_comments), y = as.numeric(atom_issues_info$days_last_comment_before_closed), use='complete.obs', method = "spearman")

p <- cor(x = as.numeric(atom_issues_info$days_first_comment_after_creation), y = as.numeric(atom_issues_info$days_between_first_and_last_comment), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_first_comment_after_creation), y = as.numeric(atom_issues_info$number_of_comments), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_first_comment_after_creation), y = as.numeric(atom_issues_info$days_last_comment_before_closed), use='complete.obs', method = "spearman")

p <- cor(x = as.numeric(atom_issues_info$days_last_comment_before_closed), y = as.numeric(atom_issues_info$days_between_first_and_last_comment), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_last_comment_before_closed), y = as.numeric(atom_issues_info$number_of_comments), use='complete.obs', method = "spearman")
p <- cor(x = as.numeric(atom_issues_info$days_last_comment_before_closed), y = as.numeric(atom_issues_info$days_first_comment_after_creation), use='complete.obs', method = "spearman")

