issues <- function(){
  
  # Set the output dir
  setwd(issues_plots_path)
  
  dormand_issues()

  # Plot 1: Histogram of days_opened
  format_issues_group('days_opened_group', "Quantidade de dias que Issues ficaram abertas", "issues")
  plot_data_stacked('days_opened_group', "Quantidade de dias que Issues ficaram abertas", "issues")
  
  # Plot 2: Histogram of days_between_first_and_last_comment
  format_issues_group('days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo coment?rio", "issues")
  plot_data_stacked('days_between_first_and_last_comment_group', "Quantidade de dias entre o primeiro e o ultimo coment?rio", "issues")
  
  # Plot 3: Histogram of days_first_comment_after_creation
  format_issues_group('days_first_comment_after_creation_group', "Quantidade de dias entre a cria??o da Issue e o Primeiro coment?rio", "issues")
  plot_data_stacked('days_first_comment_after_creation_group', "Quantidade de dias entre a cria??o da Issue e o Primeiro coment?rio", "issues")
  
  # Plot 6: Histogram of days_last_comment_before_closed
  format_issues_group('days_last_comment_before_closed_group', "Quantidade de dias entre o ?ltimo coment?rio e o fechamento da Issue", "issues")
  plot_data_stacked('days_last_comment_before_closed_group', "Quantidade de dias entre o ?ltimo coment?rio e o fechamento da Issue", "issues")
  
  # Plot 7: Histogram of employee_open
  format_data_to_stacked_plot(
    format_data(atom_issues_info, 'employee_open'),
    format_data(electron_issues_info, 'employee_open'),
    format_data(gitlfs_issues_info, 'employee_open'),
    format_data(hubot_issues_info, 'employee_open'),
    format_data(linguist_issues_info, 'employee_open'),
    'employee_open',
    "Quem abriu as Issues",
    "issues")
  
  plot_data_stacked('employee_open',
                    "Quem abriu as Issues",
                    "issues")
  
  # Plot 8: Histogram of employee_closed
  format_data_to_stacked_plot(
    format_data(atom_issues_info, 'employee_closed'),
    format_data(electron_issues_info, 'employee_closed'),
    format_data(gitlfs_issues_info, 'employee_closed'), 
    format_data(hubot_issues_info, 'employee_closed'), 
    format_data(linguist_issues_info, 'employee_closed'),
    'employee_closed',
    "Quem fechou as Issues",
    "issues")
  
  plot_data_stacked('employee_closed',
                    "Quem fechou as Issues",
                    "issues")
  
  
  
  
  
}


format_issues_group <- function(group, message, type){
  
  a <- format_data(atom_issues_info_employee, group)
  c <- format_data(electron_issues_info_employee, group)
  e <- format_data(gitlfs_issues_info_employee, group)
  g <- format_data(hubot_issues_info_employee, group)
  i <- format_data(linguist_issues_info_employee, group)
  
  b <- format_data(atom_issues_info_volunter, group)
  d <- format_data(electron_issues_info_volunter, group)
  f <- format_data(gitlfs_issues_info_volunter, group)
  h <- format_data(hubot_issues_info_volunter, group)
  j <- format_data(linguist_issues_info_volunter, group)
  
  format_data_to_stacked_grouped(a, b, c, d, e, f, g, h, i, j, group, message, type)

}


dormand_issues <- function(){
  
  atom_issues_dormand <- subset(atom_issues_info, atom_issues_info$days_opened_group == '(365,Inf]')
  electron_issues_dormand <- subset(electron_issues_info, electron_issues_info$days_opened_group == '(365,Inf]')
  gitlfs_issues_dormand <- subset(gitlfs_issues_info, gitlfs_issues_info$days_opened_group == '(365,Inf]')
  hubot_issues_dormand <- subset(hubot_issues_info, hubot_issues_info$days_opened_group == '(365,Inf]')
  linguist_issues_dormand <- subset(linguist_issues_info, linguist_issues_info$days_opened_group == '(365,Inf]')
  
  file_out_atom <- paste0(local_path, "metrics/subset/", "atom_issues_dormand.csv")
  file_out_electron <- paste0(local_path, "metrics/subset/", "electron_issues_dormand.csv")
  file_out_gitlfs <- paste0(local_path, "metrics/subset/", "gitlfs_issues_dormand.csv")
  file_out_hubot <- paste0(local_path, "metrics/subset/", "hubot_issues_dormand.csv")
  file_out_linguist <- paste0(local_path, "metrics/subset/", "linguist_issues_dormand.csv")
  
  write.csv(file = file_out_atom, atom_issues_dormand)
  write.csv(file = file_out_electron, electron_issues_dormand)
  write.csv(file = file_out_gitlfs, gitlfs_issues_dormand)
  write.csv(file = file_out_hubot, hubot_issues_dormand)
  write.csv(file = file_out_linguist, linguist_issues_dormand)
  
}

