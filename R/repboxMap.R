example = function() {
  library(repboxMap)
  library(repboxArt)
  library(repboxHtml)
  project.dir = "~/repbox/projects_reg/aejapp_3_4_9"
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  #repbox_to_regdb(project.dir)

  #opts = repbox_art_opts(single_line_reg = FALSE)
  #art_update_project(project.dir, overwrite=FALSE,opts = opts)

  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  map_repbox_project(project.dir)

  repbox_project_html(project.dir)

  html.dir = file.path(project.dir,"reports")
  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate(project.dir)
}

map_repbox_project = function(project.dir, parcels=list(), opts = repbox_map_opts()) {
  restore.point("repbox_project_map")

  parcels = regdb_load_parcels(project.dir,c("art_reg","base_regcoef","base_core","art_tab_cell","base_regscalar"), parcels=parcels)


  cell_df = parcels$art_tab_cell$art_tab_cell
  if (NROW(cell_df)==0) {
    cat("\nNo mapping is performed because no tables were extracted from the article.\n")
    return(parcels)
  }

  # Creates stata_log_num
  parcels = map_parcel_stata_log_num(project.dir, parcels)

  # Combine standard and extra regression results
  parcels = make_reg_extra_reg_combined_parcels(project.dir, parcels)

  numa=match_project_all_num(project.dir, parcels)

  # Creates match_reg parcel with two tabs (match_reg and match_regstat)
  parcels = match_project_reg(project.dir, numa=numa, parcels=parcels, opts=opts)

  # Create map for all article table cells
  parcels = map_cells_and_blocks(project.dir,parcels, numa, opts=opts)

  invisible(parcels)
}
