parcels_add_tab_ref_to_run_df = function(parcels) {
  restore.point("parcels_add_tab_ref_to_run_df")
  parcels = regdb_load_parcels(project.dir, c("stata_run_cmd", "stata_cmd_tab_fig_ref"), parcels)

  run_df = parcels$stata_run_cmd$stata_run_cmd

  if (has.col(run_df, "tabid")) {
    return(parcels)
  }
  if (is.null(run_df)) return(parcels)

  ref_df = parcels$stata_cmd_tab_fig_ref$stata_cmd_tab_fig_ref %>%
    regdb_null_to_empty("stata_cmd_tab_fig_ref") %>%
    mutate(
      tabid = ifelse(ref_type=="tab", ref_id, NA_character_)
    )

  if (!is.null(run_df)) {
    run_df = run_df %>%
      left_join(ref_df %>% select(tabid, file_path, line), by=c("file_path","line"))
  }
  parcels$stata_cmd_tab_fig_ref$stata_cmd_tab_fig_ref = ref_df
  parcels$stata_run_cmd$stata_run_cmd = run_df

  parcels
}
