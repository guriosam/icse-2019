nature <- function(){
  
  atom_nature_metrics <- load_data(paste0(local_path, "results_nature/atom_nature_metrics.csv"))
  electron_nature_metrics <- load_data(paste0(local_path, "results_nature/electron_nature_metrics.csv"))
  gitlfs_nature_metrics <- load_data(paste0(local_path, "results_nature/gitlfs_nature_metrics.csv"))
  hubot_nature_metrics <- load_data(paste0(local_path, "results_nature/hubot_nature_metrics.csv"))
  linguist_nature_metrics <- load_data(paste0(local_path, "results_nature/linguist_nature_metrics.csv"))

  atom_nature_metrics_empl <- subset(atom_nature_metrics, atom_nature_metrics$site_admin == "true")
  electron_nature_metrics_empl <- subset(electron_nature_metrics, electron_nature_metrics$site_admin == "true")
  gitlfs_nature_metrics_empl <- subset(gitlfs_nature_metrics, gitlfs_nature_metrics$site_admin == "true")
  hubot_nature_metrics_empl <- subset(hubot_nature_metrics, hubot_nature_metrics$site_admin == "true")
  linguist_nature_metrics_empl <- subset(linguist_nature_metrics, linguist_nature_metrics$site_admin == "true")
  
  atom_nature_metrics_vol <- subset(atom_nature_metrics, atom_nature_metrics$site_admin == "false")
  electron_nature_metrics_vol <- subset(electron_nature_metrics, electron_nature_metrics$site_admin == "false")
  gitlfs_nature_metrics_vol <- subset(gitlfs_nature_metrics, gitlfs_nature_metrics$site_admin == "false")
  hubot_nature_metrics_vol <- subset(hubot_nature_metrics, hubot_nature_metrics$site_admin == "false")
  linguist_nature_metrics_vol <- subset(linguist_nature_metrics, linguist_nature_metrics$site_admin == "false")
  

    #Atom
  atom_fep_median_empl <- median(atom_nature_metrics$forward_engineering_percent)
  atom_cep_median_empl <- median(atom_nature_metrics$corrective_engineering_percent)
  atom_rep_median_empl <- median(atom_nature_metrics$reengineering_percent)
  atom_unp_median_empl <- median(atom_nature_metrics$uncategorized_percent)
  atom_map_median_empl <- median(atom_nature_metrics$management_percent)
  
  atom_fep_mean<- mean(atom_nature_metrics$forward_engineering_percent)
  atom_cep_mean <- mean(atom_nature_metrics$corrective_engineering_percent)
  atom_rep_mean <- mean(atom_nature_metrics$reengineering_percent)
  atom_unp_mean <- mean(atom_nature_metrics$uncategorized_percent)
  atom_map_mean <- mean(atom_nature_metrics$management_percent)
  
  #Employee
  atom_fep_mean_empl <- mean(atom_nature_metrics_empl$forward_engineering_percent)
  atom_cep_mean_empl <- mean(atom_nature_metrics_empl$corrective_engineering_percent)
  atom_rep_mean_empl <- mean(atom_nature_metrics_empl$reengineering_percent)
  atom_unp_mean_empl <- mean(atom_nature_metrics_empl$uncategorized_percent)
  atom_map_mean_empl <- mean(atom_nature_metrics_empl$management_percent)
  
  #Volunters
  atom_fep_mean_vol <- mean(atom_nature_metrics_vol$forward_engineering_percent)
  atom_cep_mean_vol <- mean(atom_nature_metrics_vol$corrective_engineering_percent)
  atom_rep_mean_vol <- mean(atom_nature_metrics_vol$reengineering_percent)
  atom_unp_mean_vol <- mean(atom_nature_metrics_vol$uncategorized_percent)
  atom_map_mean_vol <- mean(atom_nature_metrics_vol$management_percent)
  
  atom_nature <- cbind(atom_fep_mean, atom_cep_mean, atom_rep_mean, atom_unp_mean, atom_map_mean)
  atom_nature_empl <- cbind(atom_fep_mean_empl, atom_cep_mean_empl, atom_rep_mean_empl, atom_unp_mean_empl, atom_map_mean_empl)
  atom_nature_vol <- cbind(atom_fep_mean_vol, atom_cep_mean_vol, atom_rep_mean_vol, atom_unp_mean_vol, atom_map_mean_vol)
  
  
  #Electron
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
  
  #Employee
  electron_fep_mean_empl <- mean(electron_nature_metrics_empl$forward_engineering_percent)
  electron_cep_mean_empl <- mean(electron_nature_metrics_empl$corrective_engineering_percent)
  electron_rep_mean_empl <- mean(electron_nature_metrics_empl$reengineering_percent)
  electron_unp_mean_empl <- mean(electron_nature_metrics_empl$uncategorized_percent)
  electron_map_mean_empl <- mean(electron_nature_metrics_empl$management_percent)
  
  #Volunters
  electron_fep_mean_vol <- mean(electron_nature_metrics_vol$forward_engineering_percent)
  electron_cep_mean_vol <- mean(electron_nature_metrics_vol$corrective_engineering_percent)
  electron_rep_mean_vol <- mean(electron_nature_metrics_vol$reengineering_percent)
  electron_unp_mean_vol <- mean(electron_nature_metrics_vol$uncategorized_percent)
  electron_map_mean_vol <- mean(electron_nature_metrics_vol$management_percent)
  
  electron_nature <- cbind(electron_fep_mean, electron_cep_mean, electron_rep_mean, electron_unp_mean, electron_map_mean)
  electron_nature_empl <- cbind(electron_fep_mean_empl, electron_cep_mean_empl, electron_rep_mean_empl, electron_unp_mean_empl, electron_map_mean_empl)
  electron_nature_vol <- cbind(electron_fep_mean_vol, electron_cep_mean_vol, electron_rep_mean_vol, electron_unp_mean_vol, electron_map_mean_vol)
  
  
  #Gitlfs
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
  
  #Employee
  gitlfs_fep_mean_empl <- mean(gitlfs_nature_metrics_empl$forward_engineering_percent)
  gitlfs_cep_mean_empl <- mean(gitlfs_nature_metrics_empl$corrective_engineering_percent)
  gitlfs_rep_mean_empl <- mean(gitlfs_nature_metrics_empl$reengineering_percent)
  gitlfs_unp_mean_empl <- mean(gitlfs_nature_metrics_empl$uncategorized_percent)
  gitlfs_map_mean_empl <- mean(gitlfs_nature_metrics_empl$management_percent)
  
  #Volunters
  gitlfs_fep_mean_vol <- mean(gitlfs_nature_metrics_vol$forward_engineering_percent)
  gitlfs_cep_mean_vol <- mean(gitlfs_nature_metrics_vol$corrective_engineering_percent)
  gitlfs_rep_mean_vol <- mean(gitlfs_nature_metrics_vol$reengineering_percent)
  gitlfs_unp_mean_vol <- mean(gitlfs_nature_metrics_vol$uncategorized_percent)
  gitlfs_map_mean_vol <- mean(gitlfs_nature_metrics_vol$management_percent)
  
  gitlfs_nature <- cbind(gitlfs_fep_mean, gitlfs_cep_mean, gitlfs_rep_mean, gitlfs_unp_mean, gitlfs_map_mean)
  gitlfs_nature_empl <- cbind(gitlfs_fep_mean_empl, gitlfs_cep_mean_empl, gitlfs_rep_mean_empl, gitlfs_unp_mean_empl, gitlfs_map_mean_empl)
  gitlfs_nature_vol <- cbind(gitlfs_fep_mean_vol, gitlfs_cep_mean_vol, gitlfs_rep_mean_vol, gitlfs_unp_mean_vol, gitlfs_map_mean_vol)
  
  #Hubot
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
  
  #Employee
  hubot_fep_mean_empl <- mean(hubot_nature_metrics_empl$forward_engineering_percent)
  hubot_cep_mean_empl <- mean(hubot_nature_metrics_empl$corrective_engineering_percent)
  hubot_rep_mean_empl <- mean(hubot_nature_metrics_empl$reengineering_percent)
  hubot_unp_mean_empl <- mean(hubot_nature_metrics_empl$uncategorized_percent)
  hubot_map_mean_empl <- mean(hubot_nature_metrics_empl$management_percent)
  
  #Volunters
  hubot_fep_mean_vol <- mean(hubot_nature_metrics_vol$forward_engineering_percent)
  hubot_cep_mean_vol <- mean(hubot_nature_metrics_vol$corrective_engineering_percent)
  hubot_rep_mean_vol <- mean(hubot_nature_metrics_vol$reengineering_percent)
  hubot_unp_mean_vol <- mean(hubot_nature_metrics_vol$uncategorized_percent)
  hubot_map_mean_vol <- mean(hubot_nature_metrics_vol$management_percent)
  
  hubot_nature <- cbind(hubot_fep_mean, hubot_cep_mean, hubot_rep_mean, hubot_unp_mean, hubot_map_mean)
  hubot_nature_empl <- cbind(hubot_fep_mean_empl, hubot_cep_mean_empl, hubot_rep_mean_empl, hubot_unp_mean_empl, hubot_map_mean_empl)
  hubot_nature_vol <- cbind(hubot_fep_mean_vol, hubot_cep_mean_vol, hubot_rep_mean_vol, hubot_unp_mean_vol, hubot_map_mean_vol)
  
  
  #Linguist
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
  
  #Employee
  linguist_fep_mean_empl <- mean(linguist_nature_metrics_empl$forward_engineering_percent)
  linguist_cep_mean_empl <- mean(linguist_nature_metrics_empl$corrective_engineering_percent)
  linguist_rep_mean_empl <- mean(linguist_nature_metrics_empl$reengineering_percent)
  linguist_unp_mean_empl <- mean(linguist_nature_metrics_empl$uncategorized_percent)
  linguist_map_mean_empl <- mean(linguist_nature_metrics_empl$management_percent)
  
  #Volunters
  linguist_fep_mean_vol <- mean(linguist_nature_metrics_vol$forward_engineering_percent)
  linguist_cep_mean_vol <- mean(linguist_nature_metrics_vol$corrective_engineering_percent)
  linguist_rep_mean_vol <- mean(linguist_nature_metrics_vol$reengineering_percent)
  linguist_unp_mean_vol <- mean(linguist_nature_metrics_vol$uncategorized_percent)
  linguist_map_mean_vol <- mean(linguist_nature_metrics_vol$management_percent)
  
  linguist_nature <- cbind(linguist_fep_mean, linguist_cep_mean, linguist_rep_mean, linguist_unp_mean, linguist_map_mean)
  linguist_nature_empl <- cbind(linguist_fep_mean_empl, linguist_cep_mean_empl, linguist_rep_mean_empl, linguist_unp_mean_empl, linguist_map_mean_empl)
  linguist_nature_vol <- cbind(linguist_fep_mean_vol, linguist_cep_mean_vol, linguist_rep_mean_vol, linguist_unp_mean_vol, linguist_map_mean_vol)
  
  results_nature_empl <- rbind(atom_nature_empl, electron_nature_empl, gitlfs_nature_empl, hubot_nature_empl, linguist_nature_empl)
  results_nature_vol <- rbind(atom_nature_vol, electron_nature_vol, gitlfs_nature_vol, hubot_nature_vol, linguist_nature_vol)
  
  results_nature <- rbind(atom_nature, electron_nature, gitlfs_nature, hubot_nature, linguist_nature)
  
  colnames(results_nature) <- c("Forward_Engineering_Percent_Mean", "Corrective_Engineering_Percent_Mean", "Reengineering_Percent_Mean",
                                "Uncategorized_Percent_Mean", "Management_Percent_Mean")
  
  colnames(results_nature_empl) <- c("Forward_Engineering_Percent_Mean", "Corrective_Engineering_Percent_Mean", "Reengineering_Percent_Mean",
                                "Uncategorized_Percent_Mean", "Management_Percent_Mean")
  
  
  colnames(results_nature_vol) <- c("Forward_Engineering_Percent_Mean", "Corrective_Engineering_Percent_Mean", "Reengineering_Percent_Mean",
                                "Uncategorized_Percent_Mean", "Management_Percent_Mean")
  
  
  rownames(results_nature) <- c("atom", "electron", "gitlfs", "hubot", "linguist")
  rownames(results_nature_empl) <- c("atom", "electron", "gitlfs", "hubot", "linguist")
  rownames(results_nature_vol) <- c("atom", "electron", "gitlfs", "hubot", "linguist")
  
  results_nature <- t(results_nature)
  
  results_nature <- format(results_nature, digits = 2)
  
  
  results_nature_empl <- t(results_nature_empl)
  
  results_nature_empl <- format(results_nature_empl, digits = 2)
  
  results_nature_vol <- t(results_nature_vol)
  
  results_nature_vol <- format(results_nature_vol, digits = 2)
  
  write.table(results_nature_empl)
  write.table(results_nature_vol)
  write.table(results_nature)
  #grid.table(results_nature)
  
  results_nature_empl
  results_nature_vol
  results_nature
  
  #png("projects_nature.png", height = 50*nrow(results_nature), width = 200*ncol(results_nature))
  #grid.table(results_nature)
  #write.csv(results_nature, "projects_nature.csv")
}
