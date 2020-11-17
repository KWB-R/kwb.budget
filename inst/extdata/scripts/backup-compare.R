# Get a complete file list -----------------------------------------------------
if (FALSE)
{
  system.time(
    file_info <- kwb.nextcloud::list_files(recursive = TRUE, full_info = TRUE)
  )

  nrow(file_info)
  
  target_folder <- "~/../Downloads/nextcloud-file-info"
    
  filename <- format(Sys.Date(), format = "nextcloud-files_%Y-%m-%d.csv")
  
  file <- file.path(target_folder, filename)

  write.csv2(file_info, file, row.names = FALSE)
  
  #kwb.nextcloud::create_folder("backups/root")
  kwb.nextcloud::upload_file(file, "backups/root")
  
  #   User      System verstrichen 
  # 136.25        2.16      870.66
  # 130.97        3.26      871.14 ~ 00:14:30

  files <- dir(target_folder, full.names = TRUE)
  
  snapshot_diff <- kwb.budget:::compare_file_info_files(files[2], files[3])
  View(snapshot_diff)
}

# Make a backup of the h2020_covid proposal ------------------------------------
if (FALSE)
{
  # Make a backup
  system.time(kwb.nextcloud::backup_cloud_folder("proposals/h2020_covid"))
  
  # List local backup files
  backup_files <- dir(
    "~/../Downloads/nextcloud-backups", 
    pattern = "h2020_covid_.*_meta\\.csv$", 
    full.names = TRUE
  )
  
  # Upload content file of the current backup to the cloud
  kwb.nextcloud::upload_file(
    file = tail(backup_files, 1), 
    target_path = "backups/proposals/h2020_covid"
  )
  
  #readme_file <- dir(backup_dir, "README", full.names = TRUE)
  #kwb.nextcloud::upload_file(readme_file, target_path)
}

# Compare two files containing file information --------------------------------
if (FALSE)
{
  diff_info <- kwb.budget:::compare_file_info_files(
    file_1 = backup_files[1],
    file_2 = rev(backup_files)[1]
  )
  
  View(diff_info)
}
