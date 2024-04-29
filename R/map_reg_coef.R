# For each map_reg generate the mapping on coefficient level
example = function() {
  project_dir = "~/repbox/projects_gha/aejapp_3_1_6"
  rstudioapi::filesPaneNavigate(project_dir)
  map_reg_coef(project_dir)
}

map_reg_coef = function(project_dir, parcels=list()) {
  restore.point("map_reg_coef")
  parcels = repboxDB::repdb_load_parcels(project_dir, c("map_reg","art_reg", "map_cell","regcoef"),parcels)

  map_cell = parcels$map_cell$map_cell

  if (is.null(map_cell)) return(parcels)


  map_cell = map_cell %>%
    filter(!is.na(cterm))

  if (NROW(map_cell)==0) return(parcels)


  map_reg = parcels$map_reg$map_reg
  # TO DO: Check why there are duplicates in map_reg!
  map_reg = map_reg[!duplicated(map_reg),]

  map_cell = left_join(map_cell, select(map_reg, regid, runid, step, variant, paren_type), by = c("regid", "runid", "variant"))
  map_cell = map_cell[!duplicated(map_cell),]

  # For some reason some cells are mapped to regressions but there is no entry in map_reg
  # TO DO: Explore why
  if (any(!is.na(map_cell$step))) {
    repbox_problem(type="map_cell_to_reg_without_map_reg",msg="We have entries in map_cell with regid > 0 but the regression is not included in map_reg. This is an RTutor bug that should be corrected.",fail_action = "msg",project_dir = project_dir)
    map_cell = filter(map_cell, !is.na(step))
  }


  regcoef = parcels$regcoef$regcoef

  long_map_coef = left_join(map_cell, regcoef, by=c("cterm","variant","step"))


  coef_df = long_map_coef %>% filter(num_type == 1) %>%
    select(tabid,regid,variant, mapid,runid, step, cterm,shown_term, code_label=label, coef_cellid=cellid, code_coef = coef, art_coef = num, coef_match_type = match_type)
  paren_df = long_map_coef %>% filter(num_type >= 2 & num_type <=5) %>%
    mutate(
      code_paren = case_when(
        paren_type=="se" ~ se,
        paren_type=="t" ~ t,
        paren_type=="p" ~ p,
        TRUE ~ NA_real_
      )
    ) %>%
    select(tabid,regid,variant, mapid,runid, step, cterm, paren_type, paren_cellid=cellid, code_paren, art_paren = num, paren_match_type = match_type)

  map_coef = full_join(coef_df, paren_df, by = c("tabid", "regid", "variant", "mapid", "runid", "step", "cterm"))

  art_regcoef = parcels$art_reg$art_regcoef
  map_coef = left_join(map_coef, select(art_regcoef,regid, coef_pos, coef_cellid ), by=c("regid","coef_cellid"))
  map_coef = left_join(map_coef, select(art_regcoef,regid, paren_coef_pos = coef_pos, paren_cellid ), by=c("regid","paren_cellid"))
  map_coef = map_coef %>% mutate(coef_pos = coalesce(coef_pos, paren_coef_pos))

  parcels$map_regcoef = list(map_regcoef=map_coef)
  repdb_save_parcels(parcels["map_regcoef"], file.path(project_dir, "repdb"))
  return(parcels)
}
