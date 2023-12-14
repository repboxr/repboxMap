helper_find_cmds = function() {
  projects.dirs = "~/repbox/projects_reg"
  cmd_df = all_projects_load_stata_cmd(projects.dir)
  unique(setdiff(cmd_df$cmd, ignore_log_stata_commands()))
}

all_projects_load_stata_cmd = function(projects.dir) {
  files = list.files(project.dirs,"stata_cmd.Rds",recursive = TRUE, full.names = TRUE)
  bind_rows(lapply(files, function(file) {
    readRDS(file)[[1]]
  }))

}
