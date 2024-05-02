# For each map_reg generate the mapping on coefficient level
example = function() {
  project_dir = "~/repbox/projects_gha/aejapp_3_1_6"
  rstudioapi::filesPaneNavigate(project_dir)
  map_reg_coef(project_dir)
}

map_reg_coef = function(project_dir, parcels=list()) {
  restore.point("map_reg_coef")
  parcels = repboxDB::repdb_load_parcels(project_dir, c("map_reg","art_reg","regcoef"),parcels)

  map_reg = parcels$map_reg$map_reg
  if (is.null(map_reg)) return(parcels)

  # We completely ignore cell_map which does not
  # take into account that art_coef and art_paren in table must be mapped as
  # pairs with code_coef and code_paren
  art_coef = parcels$art_reg$art_regcoef %>%
    left_join_overwrite(map_reg[,c("tabid","regid","mapid","step","variant","paren_type","runid")], by=c("regid")) %>%
    filter(!is.na(paren), !is.na(coef), !is.na(tabid)) %>%
    mutate(
      coef_deci_step = 10^-coef_num_deci,
      coef_low = coef - 0.5*1.1*coef_deci_step,
      coef_high = coef + 0.5*1.1*coef_deci_step,

      paren_deci_step = 10^-paren_num_deci,
      paren_low = paren - 0.5*1.1*paren_deci_step,
      paren_high = paren + 0.5*1.1*paren_deci_step,
      ind = seq_len(n())
    ) %>%
    rename(art_coef = coef, art_paren = paren)


  deci_step = 10^-16
  code_coef = parcels$regcoef$regcoef %>%
    filter(!is.na(coef)) %>%
    semi_join(map_reg, by="step") %>%
    full_join(select(map_reg, step, regid, paren_type), by=c("step")) %>%
    mutate(
      paren = case_when(
        paren_type == "se" ~ se,
        paren_type == "t" ~ t,
        paren_type == "p" ~ p,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(paren)) %>%
    mutate(
      coef_low = coef - 0.5*1.1*deci_step,
      coef_high = coef + 0.5*1.1*deci_step,

      paren_low = paren - 0.5*1.1*deci_step,
      paren_high = paren + 0.5*1.1*deci_step,
      ind = seq_len(n())
    )

  # which(is.na(code_coef$coef_low))
  # which(is.na(art_coef$coef_low))
  # which(is.na(code_coef$paren))
  # which(is.na(art_coef$paren_low))


  art_coef_dt = as.data.table(art_coef)

  code_coef_dt = as.data.table(code_coef)

  # 1. Map by coef within each mapped regression
  keys = c("regid", "step","coef_low","coef_high")
  setkeyv(art_coef_dt, keys)
  setkeyv(code_coef_dt, keys)

  coef_ma = foverlaps(art_coef_dt, code_coef_dt,by.x = keys, by.y=keys,nomatch=NULL, which=TRUE) %>% as_tibble()
  coef_ma[[1]] = art_coef_dt[["ind"]][coef_ma[[1]]]
  coef_ma[[2]] = code_coef_dt[["ind"]][coef_ma[[2]]]


  # 2. map by paren within each mapped regression
  keys = c("regid", "step","paren_low","paren_high")
  setkeyv(art_coef_dt, keys)
  setkeyv(code_coef_dt, keys)
  paren_ma = foverlaps(art_coef_dt, code_coef_dt,by.x = keys, by.y=keys,nomatch=NULL, which=TRUE) %>% as_tibble()

  paren_ma[[1]] = art_coef_dt[["ind"]][paren_ma[[1]]]
  paren_ma[[2]] = code_coef_dt[["ind"]][paren_ma[[2]]]


  ma_ind = bind_rows(coef_ma, paren_ma)
  ma_ind = ma_ind[!duplicated(ma_ind),]

  ma = bind_cols(
    art_coef[ma_ind[[1]], c("tabid", "mapid", "regid","step","runid", "paren_type", "coef_pos", "art_coef","art_paren","coef_num_deci","paren_num_deci","coef_cellid","paren_cellid")],
    code_coef[ma_ind[[2]], c("coef","paren","cterm","variant","label","shown_term")]
    ) %>%
    rename(
      code_coef = coef,
      code_paren = paren,
      code_label = label
    ) %>%
    mutate(
      coef_deci_step =  10^-(coef_num_deci),
      paren_deci_step = 10^-(paren_num_deci),
      coef_deci_dist = abs(art_coef-code_coef) / coef_deci_step,
      paren_deci_dist = abs(art_paren-code_paren) / paren_deci_step,
      match_score = case_when(
        coef_deci_dist <= 0.51 & paren_deci_dist <= 0.51 ~ 1,
        coef_deci_dist <= 0.51 & paren_deci_dist <= 2 ~ 0.75,
        coef_deci_dist <= 2 & paren_deci_dist <= 0.51 ~ 0.75,
        coef_deci_dist <= 2 & paren_deci_dist <= 2 ~ 0.4,
        coef_deci_dist <= 0.51 & abs(code_coef) / coef_deci_step >= 0.51 ~ 0.5,
        paren_deci_dist <= 0.51 & abs(code_paren) / paren_deci_step >= 0.51  ~ 0.5
      )
    )

  ma = ma %>%
    group_by(step, cterm, regid) %>%
    filter(
      match_score == max(match_score)
    ) %>%
    ungroup()


  parcels$map_regcoef = list(map_regcoef=ma)
  repdb_save_parcels(parcels["map_regcoef"], file.path(project_dir, "repdb"))
  return(parcels)
}
