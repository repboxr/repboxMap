# This code can be run after all other mapping code.
# It takes map_cell, art_reg, match_reg, art_small_reg
# as input and then fills the table map_reg that will be
# helpful for metastudies.

example = function() {
  project_dir = "~/repbox/projects_gha/aejapp_1_2_7"
  parcels = list()
}

repbox_store_map_reg = function(project_dir, parcels=list()) {
  restore.point("repbox_store_map_reg")
  parcels = repdb_load_parcels(project_dir, c("map_cell","match_reg","art_reg","art_small_reg"))

  map_cell = parcels$map_cell$map_cell

  if (is.null(map_cell)) return(parcels)

  match_reg = parcels$match_reg$match_reg
  if (is.null(match_reg)) return(parcels)

  art_big_reg = parcels$art_reg$art_reg
  art_small_reg = parcels$art_small_reg$art_small_reg


  if (NROW(art_small_reg)>0) {
    stop("We have art_small_reg please write code")
  } else {
    art_reg = art_big_reg %>%
      mutate(
        is_small_reg = FALSE,
        coef_pos = NA
      )
  }

  map_reg = map_cell %>%
    filter(!is.na(regid)) %>%
    filter(regid >0, !is.na(cmd), !is.na(variant)) %>%
    select(tabid, regid, cmd, mapid, runid, variant) %>%
    unique()

  map_reg = map_reg %>%
    left_join(match_reg, by=c("runid","regid","variant")) %>%
    left_join(art_reg,by=c("regid","artid","tabid")) %>%
    mutate(lang = "stata")

  colnames(map_reg)

  repdb_check_data(map_reg, "map_reg")

  parcels$map_reg = list(map_reg=map_reg)
  repdb_save_parcels(parcels["map_reg"], file.path(project_dir, "repdb"))
  parcels
}
