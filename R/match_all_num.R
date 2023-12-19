example = function() {
  library(repboxArt)
  project_dir = standardizePath("~/repbox/projects_reg/aejpol_3_4_8")
  #art_update_project(project_dir, overwrite=FALSE)

  match_project_all_num(project_dir)
}


match_project_all_num = function(project_dir,  parcels=NULL, verbose=TRUE) {
  restore.point("match_project_all_num")
  parcels = regdb_load_parcels(project_dir,c("art_tab_cell","base_regcoef","base_regscalar","stata_log_num"), parcels)

  if (is.null(parcels[[".reg"]])) {
    stop("Please first generate parcels$.reg by calling make_reg_extra_reg_combined_parcels")
  }

  names(parcels)

  cell_df = parcels$art_tab_cell$art_tab_cell
  if (is.null(cell_df)) return(NULL)

  regcoef = parcels$.reg$regcoef %>%
    regdb_null_to_empty("regcoef") %>%
    add_col(num_deci=10)
  regscalar = parcels$.reg$regscalar %>%
    regdb_null_to_empty("regscalar") %>%
    add_col(num_deci=10)

  stata_log_num = parcels$stata_log_num$stata_log_num  %>%
    regdb_null_to_empty("stata_log_num")

  a_df = cell_df %>% filter(type=="num") %>%
    select(num=num, num_deci=num_deci) %>%
    unique() %>%
    mutate(width = ifelse( abs(num) <= 20 & num_deci==0,0,1.1))

  c_df = bind_rows(
    select(stata_log_num, num=num, num_deci=num_deci),
    select(regscalar, num=scalar_val, num_deci),
    select(regcoef, num=coef, num_deci),
    select(regcoef, num=se, num_deci),
    select(regcoef, num=t, num_deci),
    select(regcoef, num=p, num_deci)
  ) %>%
    unique() %>%
    na.omit() %>%
    mutate(width = ifelse( abs(num) <= 20 & num_deci==0,0,1.1))



  #a_df = filter(a_df, num_deci > 0 | num > 10)
  #c_df = filter(c_df, num_deci > 0 | num > 10)

  ma_df = match_all_rounded(mult="all",
    xval=a_df$num, xdeci=a_df$num_deci, xwidth=a_df$width,
    yval=c_df$num, ydeci = c_df$num_deci,ywidth = c_df$width)

  numa = bind_cols(
    (a_df[ma_df[[1]],1:2]) %>% rename(a_num = num, a_deci=num_deci),
    (c_df[ma_df[[2]],1:2]) %>% rename(c_num = num, c_deci=num_deci)
  ) %>%
    mutate(dist = abs(a_num-c_num), deci_step = 10^-pmin(a_deci, c_deci),  deci_dist = dist / deci_step) %>%
    select(-dist) %>%
    # for integers require distance smaller than 1-eps
    filter(!(a_deci==0 & c_deci==0 & deci_dist >= 1-1e-12))

  numa
}
