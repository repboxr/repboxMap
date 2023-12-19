# Extract numbers from Stata log files

example = function() {
  project_dir = "/home/rstudio/repbox/projects_reg/aejapp_2_4_8"
  project_dir = "~/repbox/projects_reg/aejpol_3_4_8"
  cmd_df = readRDS.or.null(file.path(project_dir, "repbox","regdb","stata_cmd.Rds"))[[1]]
  num_df = project_log_extract_num(project_dir)
  unique(num_df$cmd)
  unique(setdiff(cmd_df$cmd, ignore_log_stata_commands()))

  head(num_df)
  head(num_df$num_label[num_df$num_label != ""],100)

  rstudioapi::filesPaneNavigate(project_dir)
}


ignore_log_stata_commands = function() {
  c("use","u", "gen","g","generate", "drop","keep","set","replace","save","preserve","restore","import","graph","clear","append","foreach","if","egen","exit","sort","collapse","version","main","paths","macro","!perl","!cp","syntax","rename","writeln","guse","twoway","graphsTime","global","local","program","cd","merge","scalar","stop","xi","tsset","xtset","adopaths","destring","insheet","reshape","scatter","lab","label","quietly","log","forvalues","encode","recode","!rm","tempfile","addLabels","*","}","end")

}

get_stata_log_num = function(project_dir, create_if_missing = TRUE) {
  file = file.path(project_dir,"map","regdb","stata_log_num.Rds")
  if (file.exists(file)) {
    return(readRDS(file)[[1]])
  }
  project_log_extract_num(project_dir)

}

map_parcel_stata_log_num = function(project_dir, parcels=list(), verbose=TRUE) {
  restore.point("map_parcel_stata_log_num")
  log_df = load_stata_log_df(project_dir, parcels)
  if (is.null(log_df)) {
    if (verbose) cat("\nNo logs from running the supplement found. Did you call repbox_to_regdb from repboxDB?\n")
    return(parcels)
  }
  num_df = log_extract_numbers(log_df)

  regdb_load_specs(libs="repboxMap")
  regdb_check_data(num_df, "stata_log_num")

  parcels$stata_log_num = list(stata_log_num=num_df)

  dir = file.path(project_dir,"map","regdb")
  regdb_save_parcels(parcels["stata_log_num"], dir)
  parcels
}

load_stata_log_df = function(project_dir, parcels = NULL, verbose=TRUE) {
  restore.point("load_stat_log_df")
  parcels = regdb_load_parcels(project_dir, c("stata_cmd","stata_run_cmd","stata_run_log"), parcels)

  cmd_df = parcels$stata_cmd$stata_cmd
  run_df = parcels$stata_run_cmd$stata_run_cmd
  log_df = parcels$stata_run_log$stata_run_log

  if (is.null(cmd_df) | is.null(log_df)) return(NULL)

  log_df = left_join(run_df, log_df, by=c("artid","runid")) %>%
    left_join(select(cmd_df, -cmd, -cmdline) , by = c("artid","file_path","line"))
  log_df
}



log_extract_numbers = function(log_df) {
  restore.point("log_extract_numbers")

  ignore_commands = ignore_log_stata_commands()

  rows = which(!log_df$cmd %in% ignore_commands & !log_df$is_reg & !is.na(log_df$logtxt) & !log_df$logtxt=="")
  log_df = log_df[rows,]

  if (NROW(log_df)==0) return(NULL)

  #log_df$logtxt[is.na(log_df$logtxt)] = ""

  logtxt = merge.lines(log_df$logtxt)

  # temp1 = paste0("\n###################################\n",seq_rows(log_df),"\n###################################\n",log_df$logtxt, collapse="\n")
  # writeLines(temp1, "~/repbox/temp.txt")
#
#   txt3 = sep.lines(logtxt)
#   temp3 = paste0(seq_along(txt3),": ",txt3, collapse="\n")
#   writeLines(temp3, "~/repbox/temp3.txt")


  log_df$nchar = nchar(log_df$logtxt)
  inds = c(1,cumsum(log_df$nchar+1)+1)
  log_df$start = inds[-(NROW(log_df)+1)]
  log_df$end = inds[-1]-1

  #txt2 = stri_sub(logtxt,log_df$start, log_df$end)
  #writeLines(paste0("\n###################################\n",seq_rows(log_df),"\n###################################\n",txt2, collapse="\n"), "~/repbox/temp2.txt")


  #fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  # Update don't allow in Stata log space between - and number
  fp = "[+\\-−]?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  pos = stri_locate_all_regex(logtxt, fp,omit_no_match = TRUE)[[1]]
  if (NROW(pos)==0) return(NULL)
  num_str = stri_sub(logtxt,pos[,1],pos[,2]) %>% stri_replace_all_regex("[+,]","") %>% stri_replace_all_fixed("−","-")

  line_start_pos = c(1,stri_locate_all_fixed(logtxt,"\n")[[1]][,1]+1)
  #tab_line_pos = stri_locate_all_fixed(logtxt,fixed("\n----"))[[1]]

  num_line = findInterval(pos[,1], line_start_pos)

  #equal_pos = stri_locate_all_regex(logtxt,'[ ]+[=][ ]+')[[1]]

  # Typical F-statistic
  rx1 = "F[ ]?\\([ ]*[0-9]+[ ]*[,]?[ ]*[0-9]*[ ]*\\)"
  # General label
  rx2 = "([\\(\\)\\[\\]a-zA-Z0-9,><\\.\\:_]+[ ])*[a-zA-Z0-9,<>\\.\\:_\\(\\)\\[\\]]+"

  rx = paste0("((",rx1,")|(",rx2,"))[ ]+[=][ ]+")
  equal_pos = stri_locate_all_regex(logtxt,rx)[[1]]

  num_equal_ind = match(pos[,1], equal_pos[,2]+1)
  has_equal = !is.na(num_equal_ind)
  eq_pos = equal_pos[num_equal_ind[has_equal],,drop=FALSE]
  eq_labs = stri_sub(logtxt, eq_pos[,1], eq_pos[,2]) %>% trimws()
  num_label = rep("", length(num_str))
  num_label[has_equal] = eq_labs


  line_log_row = findInterval(line_start_pos, c(log_df$start, Inf))

  log_df$num_lines = tabulate(line_log_row,nbins=NROW(log_df))
  log_df$start_line = c(1, cumsum(log_df$num_lines)+1)[-(NROW(log_df)+1)]

  num_log_row = line_log_row[num_line]
  #all(num_log_row == findInterval(pos[,1], c(log_df$start, Inf)))

  num_df = tibble(
    num_start = pos[,1], num_end = pos[,2],
    num_str = num_str,
    log_df_row = num_log_row,
    num_line = num_line,
    num_label = num_label,
    after_equal = has_equal
  ) %>%
    mutate(
      num = as.numeric(num_str),
      num_deci = nchar(str.right.of(num_str,".",not.found = NA)) %>% na.val(0)
      #val_deci = nchar(str.right.of(num,".",not.found = NA)) %>% na.val(0)
    )



  log_df$log_df_row =seq_rows(log_df)
  num_df = left_join(num_df, select(log_df,log_df_row, artid, runid, cmd, line, orgline, start, start_line), by="log_df_row")

  num_df = num_df %>% mutate(
    row = num_line - start_line + 1,
    line_start = line_start_pos[num_line],
    ccol_start = num_start-line_start+1,
    ccol_end = num_end-line_start+1
  )
#
#   txt4 = paste0(num_df$num_line,": ", num_df$num_str, collapse="\n")
#   writeLines(txt4, "~/repbox/temp4.txt")


  cols = c("cmd","num_str","num","num_deci", "num_label", "row", "ccol_start","ccol_end",  "artid", "runid")

  num_df = num_df[, cols]
  num_df
}

