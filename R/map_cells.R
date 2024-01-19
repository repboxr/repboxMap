# Is now moved to repboxMap

# Currently we just compare one (best) map using a simple heuristic
# There is considerable scope for improvement
map_cells_and_blocks = function(project_dir, parcels=NULL, numa, opts) {
  restore.point("map_cells_and_blocks")
  #stop()
  parcels = repdb_load_parcels(project_dir, c("art_reg","art_tab", "art_tab_cell","match_reg","match_regstat","base_regcoef","base_core","stata_run_cmd", "stata_cmd_tab_fig_ref"), parcels)

  if (is.null(parcels$.reg)) {
    parcels = make_reg_extra_reg_combined_parcels(project_dir, parcels)
  }




  # Add general matching info
  tab_df = parcels$art_tab[["art_tab"]]

  cell_df = parcels$art_tab_cell$art_tab_cell %>%
    filter(type=="num") %>%
    left_join(select(tab_df, tabid, tabpos), by="tabid") %>%
    mutate(.cell.row = seq_len(n()), cmd = "")

  # Define blocks
  cell_df = cell_df %>% mutate(
      block_str = paste0(tabid,";", panel_num, ";", num_row_block, ";", col)
    )
  cell_df$block = match(cell_df$block_str, unique(cell_df$block_str))
  # Find best maps for three cases:
  # i) small regression interpretation
  # ii) big regresson interpretation
  # iii) no regression interpretation
  org_cols = colnames(cell_df)
  cell_df = find_cells_reg_map(cell_df, parcels, small=TRUE, opts=opts)

  cols = c("is_best_reg_match","is_reg_match","reg_match_best_runid","reg_match_runid","reg_deci_dist","reg_cmd","reg_best_cmd")
  new_cols = paste0("small_", cols)

  cell_df = rename.cols(cell_df, cols, new_cols)
  cell_df = cell_df[,c(org_cols, new_cols)]
  cell_df = find_cells_reg_map(cell_df, parcels, small=FALSE, opts=opts)

  cell_df = find_cells_noreg_map(cell_df, parcels, opts)


  # 1. We determine for each table whether the big or small
  #    regression interpretation is better
  # tab_info = cell_df %>%
  #   group_by(tabid) %>%
  #   summarize(
  #     small_share_reg_match = mean(is.true(small_is_best_reg_match)),
  #     small_dupl_share = mean(duplicated(paste0(small_reg_match_runid,paren_type))),
  #     big_share_reg_match = mean(is.true(is_best_reg_match)),
  #     reg_type = ifelse(big_share_reg_match >= small_share_reg_match, "big", "small")
  #   )

  # We don't use Hungarian Algorithm anymore for small (or big) regressions
  # instead as heuristic duplicated rows reduce by a factor of 0.5 the
  # small regression fit when deciding between big or small
  # Alternative: rewrite Hungarian code to perform matching only on table level
  cell_df = cell_df %>%
    group_by(tabid) %>%
    mutate(
      small_share_reg_match = mean(is.true(small_is_best_reg_match)),
      small_dupl_share = mean(duplicated(paste0(small_reg_match_runid,paren_type))),
      big_share_reg_match = mean(is.true(is_best_reg_match)),

      reg_type = ifelse(big_share_reg_match >= small_share_reg_match - 0.5*small_dupl_share, "big", "small")
    )

  rows = which(cell_df$reg_type == "small")

  if (length(rows)>0) {
    cell_df[rows, cols] = cell_df[rows, new_cols]
    cell_df = remove.cols(cell_df, new_cols)
  }



  # Let us now determine best_match_type and best_match_chunkind
  # from reg and noreg matches
  cell_df = cell_df %>%
    group_by(block) %>%
    mutate(
      block_num_best_reg_match = sum(is_best_reg_match),
      block_num_reg_match = sum(is_reg_match),
      block_num_noreg_match = sum(!is.na(noreg_match_runid)),
      block_type = case_when(
        block_num_reg_match >= opts$reg_vs_noreg_map_factor*block_num_noreg_match  ~ "reg",
        TRUE ~ "noreg"
      )
    )

  # Identify for each row whether there is some regression coef / se cell
  # If not we determine for each row separately whether it shows regression
  # statistics or noreg numbers
  cell_df = cell_df %>%
    group_by(tabid, row) %>%
    mutate(
      row_has_reg_coef_se = block_type == "reg" & any(reg_num_type > 0 & reg_num_type <10)
    ) %>%
    mutate(
      block_type = case_when(
        row_has_reg_coef_se ~ block_type,
        # row without regression statistics
        !row_has_reg_coef_se & any(reg_num_type  >=10) ~ "reg",
        TRUE ~ "noreg"
      )
    ) %>%
    ungroup()



  #temp = filter(cell_df, regid==1)

  # See html_tab_num_cell.yml
  cell_df = cell_df %>% mutate(
    match_type = case_when(
      block_type == "reg" & is_best_reg_match & reg_deci_dist <= 0.501 ~ 1L,
      block_type == "reg" & is_best_reg_match & reg_deci_dist > 0.5 ~ 2L,
      block_type == "reg" & is_reg_match  & reg_deci_dist <= 0.501 ~ 3L,
      block_type == "reg" & is_reg_match  & reg_deci_dist > 0.5 ~ 4L,
      block_type == "reg" ~ 0L,
      block_type == "noreg" & noreg_deci_dist <= 0.501 ~ 5L,
      block_type == "noreg" & noreg_deci_dist > 0.5 ~ 6L,
      #block_type == "noreg" & !is.na(noreg_match_runid) & noreg_deci_dist <= 0.501 ~ 7L,
      TRUE ~ 0L
    ),
    best_runid = case_when(
      match_type == 0 ~ 0L,
      match_type <= 4 ~ reg_match_best_runid,
      match_type <= 7 ~ noreg_match_runid
    ),
    runid = case_when(
      match_type == 0 ~ 0L,
      match_type <= 4 ~ reg_match_runid,
      match_type <= 7 ~ noreg_match_runid
    ),
    variant = case_when(
      match_type == 0 ~ "",
      match_type <= 4 ~ reg_match_variant,
      match_type <= 7 ~ ""
    ),
    num_type = ifelse(block_type == "reg", reg_num_type, 0),
    regid = ifelse(block_type == "reg", regid, 0),
    deci_dist = ifelse(block_type == "reg", reg_deci_dist, noreg_deci_dist),
    cmd =  ifelse(block_type == "reg", reg_cmd, noreg_cmd),
    best_cmd =  ifelse(block_type == "reg", reg_best_cmd, noreg_cmd),
    mapid = 1
  )

  # block_df = cell_df %>%
  #   group_by(block, mapid, tabpos) %>%
  #   summarize(
  #     regid = first(regid),
  #     cmd = first(best_cmd),
  #     runid = ifelse(length(unique(na.omit(best_runid)))==1,unique(na.omit(best_runid)),0)
  #   )

  repdb_check_data(cell_df,"map_cell")

  parcels$map_cell = list(map_cell=cell_df)

  repdb_save_parcels(parcels[c("map_cell")],file.path(project_dir,"repdb"))


  parcels
}

# To do: match regstats
find_cells_reg_map = function(cell_df, parcels, small = FALSE, opts) {
  restore.point("find_cells_reg_map")
  #stop()
  if (small) {
    restore.point("find_cells_reg_map_small")
  } else {
    restore.point("find_cells_reg_map_big")
  }

  run_df = parcels$stata_run_cmd$stata_run_cmd
  reg_coef = parcels$.reg$regcoef
  if (NROW(reg_coef)==0 | NROW(parcels$art_reg$art_reg)==0 | NROW(parcels$match_reg$match_reg)==0) {
    cell_df$is_best_reg_match = FALSE
    cell_df$is_reg_match = FALSE
    cell_df$reg_match_best_runid = NA_integer_
    cell_df$reg_match_runid = NA_integer_
    cell_df$reg_match_variant = NA_character_
    cell_df$reg_match_best_variant = NA_character_
    cell_df$reg_deci_dist = NA_real_
    cell_df$reg_cmd = NA_character_
    cell_df$reg_best_cmd = NA_character_
    cell_df$regid = NA_integer_
    cell_df$reg_num_type = NA_integer_
    return(cell_df)
  }

  reg_coef = reg_coef %>%
    repdb_null_to_empty("regcoef") %>%
    left_join(select(parcels$.reg$reg, step, variant, runid), by=c("step","variant"))

  reg_df = parcels$art_reg$art_reg


  stat_df = parcels$art_reg$art_regstat %>%
    repdb_null_to_empty("art_regstat") #%>%
#    left_join(select(reg_df, regid, tabid), by="regid")

  coef_df = parcels$art_reg$art_regcoef #%>%
    #left_join(select(reg_df, regid, tabid), by="regid")


  cell_df = cell_df %>%
    # Match regid and determine
    left_join( select(coef_df, coef_pos, cellid=coef_cellid, coef_regid=regid), by=c("cellid")) %>%
    left_join( select(coef_df,paren_coef_pos=coef_pos,cellid=paren_cellid, paren_regid=regid), by=c("cellid")) %>%
    left_join( select(stat_df,cellid, stat_regid=regid, stat_name), by=c("cellid")) %>%
    mutate(
      regid = coalesce(coef_regid, paren_regid, stat_regid, 0),
      coef_pos = coalesce(coef_pos, paren_coef_pos, 0),
      reg_num_type = case_when(
        !is.na(coef_regid) ~ 1,
        !is.na(paren_regid) ~ 2,
        is.true(stat_name=="nobs") ~ 11,
        is.true(stat_name=="r2") ~ 12,
        !is.na(stat_regid) ~ 10,
        TRUE ~ 0
      )
    )


  if (small) {
    small_reg = parcels$art_reg$art_small_reg
    cell_df = cell_df %>%
      rename(big_regid = regid) %>%
      left_join_overwrite(select(small_reg, -artid), by=c("big_regid","coef_pos")) %>%
      # Big regressions with single row will be kept
      mutate(regid = ifelse(is.na(regid), big_regid, regid))
  }

  cell_df$is_best_reg_match = FALSE
  cell_df$is_reg_match = FALSE
  cell_df$reg_match_best_runid = NA_integer_
  cell_df$reg_match_runid = NA_integer_
  cell_df$reg_match_best_variant = NA_character_
  cell_df$reg_match_variant = NA_character_
  cell_df$reg_deci_dist = NA_real_



  # Match regression cells
  reg_ma = parcels$match_reg$match_reg

  # Filters correctly for both small==TRUE and small==FALSE
  reg_ma = semi_join(reg_ma, cell_df, by="regid")

  if (small) {
    reg_ma = filter(reg_ma, art_ncoef == 1)
  }

  # TO DO: Duplicate removal should be already be done
  # when creating matches, not only here
  reg_ma = reg_ma[!duplicated(select(reg_ma, regid, runid, variant, tabid_ref_match)),]


  # Apply correction for match of table reference from code comments
  reg_ma = reg_ma %>%
    mutate(match_score = ifelse(is.true(tabid_ref_match != 1L), match_score * opts$table_ref_differs_shrink_factor, match_score))


  #temp = filter(cell_df, tabid=="3")
  #temp = filter(reg_coef, runid==73)
  #temp = filter(reg_ma, regid==1)

  # TO DO: Check why for small reg some regressions have
  # a match score above 1

  # Find best matches
  do.hungarian = small
  do.hungarian = FALSE


  if (!do.hungarian) {
    # Just pick one as best runid
    # TO DO: Try to map to table references
    breg_ma = reg_ma %>%
      group_by(regid) %>%
      arrange(desc(match_score)) %>%
      slice(1) %>%
      ungroup()
  } else {
    # Even for small reg hungarian algo for whole artice together is not
    # so nice, see e.g. aejmac_3_3_4 Table 2
    # If we want to use it, we'd have to rewrite it to match only
    # on table level.

    # Match best article regression with code
    # regression using hungarian algorithm
    library(RcppHungarian)

    # For the case, we have different variants
    # pick for each regid, runid combination the
    # variant with highest match score
    reg_ma = reg_ma %>%
      group_by(regid, runid) %>%
      arrange(desc(match_score)) %>%
      slice(1) %>%
      ungroup()

    if (NROW(reg_ma)==0) {
      stop("Need to deal with empty reg_ma before calling HungarianRccp. Ideally this error should never occur because we check for empty reg matches already earlier.")
    }

    regids = unique(reg_ma$regid)
    runids = unique(reg_ma$runid)
    max_cost = max(reg_ma$match_score)
    cost_mat = matrix(max_cost, length(regids), length(runids))
    reg_ma$.cost.row = match(reg_ma$regid, regids)
    reg_ma$.cost.col = match(reg_ma$runid, runids)

    cost_mat[cbind(reg_ma$.cost.row, reg_ma$.cost.col)] = max_cost-reg_ma$match_score

    res = HungarianSolver(cost_mat)
    best_pairs = res$pairs %>% as.data.frame()
    names(best_pairs) = c(".cost.row", ".cost.col")
    breg_ma = semi_join(reg_ma, best_pairs, by = c(".cost.row", ".cost.col"))
  }


  reg_cell_df = cell_df %>%
    filter( regid > 0) %>%
    left_join(select(breg_ma, regid, best_runid=runid, best_variant=variant, reg_paren_type=paren_type), by="regid")


  # coef, se, t, p
  reg_paren_types = c("coef",unique(reg_cell_df$reg_paren_type)) %>% na.omit()
  ptype = "coef"
  ptype = "se"

  for (ptype in reg_paren_types) {
    if (ptype != "coef") {
      c_df = reg_cell_df %>%
        filter(reg_paren_type == ptype & paren_type %in% c("(","["))
    } else {
      c_df = reg_cell_df
    }

    rcoef = reg_coef[!is.na(reg_coef[[ptype]]), ]

    # Also include approximate matches
    ind_df = match_all_rounded(c_df$num,xwidth = 2*opts$approx_map_max_deci_dist, c_df$num_deci, rcoef[[ptype]], ydeci = 14, add_num=TRUE)

    numa = tibble(cellid = c_df$cellid[ind_df[[1]]], runid=rcoef$runid[ind_df[[2]]], variant = rcoef$variant[ind_df[[2]]],  deci_dist=ind_df$deci_dist)
    numa = left_join(numa, select(c_df, cellid, best_runid, best_variant), by="cellid") %>%
      mutate(is_best_runid = (runid==best_runid)) %>%
      arrange(desc(is_best_runid), deci_dist, runid)
    #best_numa = numa[is.true(numa$runid==numa$best_runid),]

    cellids = c_df$cellid
    numa_rows = match(cellids,numa$cellid)
    cell_df$reg_match_runid[cellids] = numa$runid[numa_rows]
    cell_df$reg_match_best_runid[cellids] = numa$best_runid[numa_rows]

    cell_df$reg_match_variant[cellids] = numa$variant[numa_rows]
    cell_df$reg_match_best_variant[cellids] = numa$best_variant[numa_rows]


    cell_df$reg_deci_dist[cellids] = numa$deci_dist[numa_rows]

    cell_df$is_best_reg_match[cellids] = cell_df$reg_match_runid[cellids] == cell_df$reg_match_best_runid[cellids]

    cell_df$is_reg_match[cellids] = cellids %in% numa$cellid
  }

  # Now also store info for regstat
  c_df = filter(cell_df, !is.na(stat_name))

  rows = c_df$.cell.row
  cell_df$is_reg_match[rows] = TRUE

  stat_map = parcels$match_regstat$match_regstat %>%
    repdb_null_to_empty("match_regstat")

  if (NROW(stat_map)>0) {
    stat_map$stat_deci_dist = ifelse(is.na(stat_map$deci_diff),0,stat_map$deci_diff)
    #stat_map = parcels$html$html_reg_stat_map
    best_stat_map = semi_join(stat_map, breg_ma, by=c("runid","regid","variant"))

    # To do: Think about variant
    # currently we ignore it
    best_c_df = c_df %>%
      left_join(select(best_stat_map, stat_name, regid, runid, stat_deci_dist, variant), by=c("stat_name","regid")) %>%
      filter(!is.na(runid))
    rows = best_c_df$cellid
    cell_df$is_best_reg_match[rows] = TRUE
    cell_df$reg_match_best_runid[rows] = cell_df$reg_match_runid[rows] = best_c_df$runid
    cell_df$reg_deci_dist[rows] = best_c_df$stat_deci_dist
  }

  cell_df$reg_cmd = run_df$cmd[cell_df$reg_match_runid]
  cell_df$reg_best_cmd = run_df$cmd[cell_df$reg_match_best_runid]

  cell_df
}


# Idea: For each block find command with most matches
find_cells_noreg_map = function(cell_df, parcels, opts) {
  restore.point("find_cells_noreg_map")

  num_df = parcels$stata_log_num$stata_log_num %>%
    arrange(num)
  do_nums = c(-Inf,num_df$num,Inf)
  any(is.na(do_nums))

  cell_df$noreg_match_runid = NA_integer_
  cell_df$noreg_deci_dist = NA_real_
  cell_df$noreg_cmd = NA_character_

  org_cell_df = cell_df
  # If a number has a percentage sign %, duplicate it in in cell_df
  # and divide number by 100 for alternative match:
  cell_df$cell_variant = ""
  cell_df$has_perc = stri_detect_fixed(cell_df$text,"%") & cell_df$type=="num"

  perc_cell_df = cell_df[cell_df$has_perc,]
  if (NROW(perc_cell_df)>0) {
    perc_cell_df = perc_cell_df %>% mutate(num = num / 100, num_deci = num_deci +2L, cell_variant="0.01")
    cell_df = bind_rows(cell_df, perc_cell_df) %>%
      arrange(cellid)
  }

  # To do: need to check with an article where we do actually have matches
  cell_df = cell_df %>% mutate(
    # Special rule for integers
    use_deci = case_when(
      num_deci == 0 & abs(num) <= 20 & !has_perc ~ 10L,
      #num_deci == 0 & abs(num) <= 10000 ~ 2L,
      TRUE ~ num_deci
    ),
    #num_low = num - 0.51 * 10^-use_deci,
    #num_high = num + 0.51 * 10^-use_deci,
    num_low = num - opts$approx_map_max_deci_dist * 10^-use_deci,
    num_high = num + opts$approx_map_max_deci_dist * 10^-use_deci,
    do_num_start_ind = findInterval(num_low,do_nums)+1,
    do_num_end_ind = findInterval(num_high, do_nums),
    has_match = do_num_end_ind >= do_num_start_ind
  ) %>% mutate(
    do_num_start_ind = ifelse(has_match, do_num_start_ind, -1),
    do_num_end_ind = ifelse(has_match, do_num_end_ind, -2)
  )


  c_df = filter(cell_df,do_num_start_ind>0)

  if (NROW(c_df)==0) return(cell_df %>% filter(cell_variant==""))


  # To match c_df with num_df we use match_all_rounded even
  # though we know do_num_start_ind and do_num_end_ind.
  # That is because I don't know a quick way to expand them
  # and probably the C code data.table code is faster even
  # though the algo is complexer.
  ind_df = match_all_rounded(c_df$num, c_df$num_deci, num_df$num, ydeci = 10,add_num = TRUE,xwidth = opts$approx_map_max_deci_dist*2)
  numa = tibble(cellid = c_df$cellid[ind_df[[1]]], cell_variant=c_df$cell_variant[ind_df[[1]]], runid=num_df$runid[ind_df[[2]]], deci_dist=ind_df$deci_dist)


  # We might have multiple matches for different cellid, runid combis
  # just keep the one with the smallest deci_diff
  numa = numa %>%
    group_by(runid, cellid, cell_variant) %>%
    arrange(deci_dist) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(is_approx = deci_dist >= 0.501)

  run_df = parcels$stata_run_cmd$stata_run_cmd

  # Alternative way to generate ranks
  numa = numa %>%
    left_join(select(c_df,-cmd), by=c("cellid","cell_variant")) %>%
    left_join(select(run_df, runid, cmd), by="runid")

  set01 = function(x) {
    x[x==0] = 1
    x
  }
  numa = numa_score = numa %>%
    group_by(block) %>%
    mutate(n_cell_block = n_distinct(cellid)) %>%
    group_by(tabpos, panel_num,row) %>%
    mutate(n_cell_row = n_distinct(cellid)) %>%
    group_by(block, runid) %>%
    mutate(n_chunk_block = n_distinct(cellid[!is_approx])) %>%
    group_by(block, cmd) %>%
    mutate(n_cmd_block = n_distinct(cellid[!is_approx])) %>%
    group_by(tabpos, panel_num,row, runid) %>%
    mutate(n_chunk_row = n_distinct(cellid[!is_approx])) %>%
    group_by(tabpos, panel_num,row, cmd) %>%
    mutate(n_cmd_row = n_distinct(cellid[!is_approx])) %>%
    # Now compute a score for each cellid, runid combi
    group_by(cellid, runid, cell_variant) %>%
    mutate(
      # score mainly by row consistency
      # score =
      #   ((n_chunk_row -1) / set01(n_cell_row-1)) +
      #   ((n_cmd_row -1) / set01(n_cmd_row-1))+
      #   0.1*(
      #   ((n_chunk_block -1) / set01(n_cell_block-1)) +
      #   ((n_cmd_block -1) / set01(n_cmd_block-1))
      #   )
      # mix of row and block consistency more cells
      # in row or block get higher weight
      score =
        ifelse(is_approx, 0.5,1)*
        ((n_chunk_row -1) / set01(n_cell_row-1))^2 *
        set01(n_cell_row-1)^0.5 +
        ((n_cmd_row -1) / set01(n_cmd_row-1))^2 *
        set01(n_cmd_row-1)^0.5 +
      ((n_chunk_block -1) / set01(n_cell_block-1))^2 *
        set01(n_cell_block-1)^0.5 +
        ((n_cmd_block -1) / set01(n_cmd_block-1))^2 *
        set01(n_cmd_block-1)^0.5
    )
  numa = numa %>%
    group_by(cellid) %>%
    arrange(desc(score),deci_dist, desc(n_cmd_block), desc(n_chunk_block), desc(n_cmd_row)) %>%
    slice(1) %>%
    ungroup()

  #temp = numa_score %>% filter(cellid %in% c(75)) %>% select(1,2,score,text,55:62)
  #temp = temp %>% filter(runid %in% c(23,28))

  rows = numa$cellid
  cell_df = org_cell_df

  cell_df$noreg_match_runid[rows] = numa$runid
  cell_df$noreg_deci_dist[rows] = numa$deci_dist
  cell_df$noreg_cmd[rows] = numa$cmd

  cell_df
}




# Idea: For each block find command with most matches
old.find_cells_noreg_map = function(cell_df, parcels, opts) {
  restore.point("find_cells_noreg_map")

  num_df = parcels$stata_log_num$stata_log_num %>%
    arrange(num)
  do_nums = c(-Inf,num_df$num,Inf)

  # To do: need to check with an article where we do actually have matches
  cell_df = cell_df %>% mutate(
    # Special rule for integers
    use_deci = case_when(
      num_deci == 0 & abs(num) <= 20 ~ 10L,
      #num_deci == 0 & abs(num) <= 10000 ~ 2L,
      TRUE ~ num_deci
    ),
    #num_low = num - 0.51 * 10^-use_deci,
    #num_high = num + 0.51 * 10^-use_deci,
    num_low = num - opts$approx_map_max_deci_dist * 10^-use_deci,
    num_high = num + opts$approx_map_max_deci_dist * 10^-use_deci,
    do_num_start_ind = findInterval(num_low,do_nums)+1,
    do_num_end_ind = findInterval(num_high, do_nums),
    has_match = do_num_end_ind >= do_num_start_ind
  ) %>% mutate(
    do_num_start_ind = ifelse(has_match, do_num_start_ind, -1),
    do_num_end_ind = ifelse(has_match, do_num_end_ind, -2)
  )




  cell_df$noreg_match_chunk_rank = NA_integer_
  cell_df$noreg_match_runid = NA_integer_
  cell_df$noreg_deci_dist = NA_real_
  cell_df$noreg_cmd = NA_character_

  c_df = filter(cell_df,do_num_start_ind>0)

  if (NROW(c_df)==0) return(cell_df)


  # To match c_df with num_df we use match_all_rounded even
  # though we know do_num_start_ind and do_num_end_ind.
  # That is because I don't know a quick way to expand them
  # and probably the C code data.table code is faster even
  # though the algo is complexer.
  ind_df = match_all_rounded(c_df$num, c_df$num_deci, num_df$num, ydeci = 10,add_num = TRUE)
  numa = tibble(cellid = c_df$cellid[ind_df[[1]]], runid=num_df$runid[ind_df[[2]]], deci_dist=ind_df$deci_dist)
  run_df = parcels$stata_run_cmd$stata_run_cmd

  # Determine how often each runid is found per block
  numa = left_join(numa, select(c_df, cellid, block), by="cellid")

  run_df = parcels$stata_run_cmd$stata_run_cmd

  block_df = numa %>%
    group_by(block, runid) %>%
    summarize(
      n_chunk = n()
    ) %>%
    left_join(select(run_df, runid, cmd), by="runid") %>%
    group_by(block, cmd) %>%
    mutate(
      n_cmd = sum(n_chunk)
    ) %>%
    group_by(block) %>%
    arrange(desc(n_cmd), desc(n_chunk)) %>%
    mutate(
      chunk_rank = seq_len(n())
    ) %>%
    ungroup()



  numa = left_join(numa, select(block_df, block, runid, chunk_rank, cmd), by=c("block","runid"))

  # Only pick for every cell the "best chunk"
  numa = numa %>%
    group_by(cellid) %>%
    arrange(chunk_rank, runid) %>%
    slice(1)

  rows = numa$cellid
  cell_df$noreg_match_chunk_rank[rows] = numa$chunk_rank
  cell_df$noreg_match_runid[rows] = numa$runid
  cell_df$noreg_deci_dist[rows] = numa$deci_dist
  cell_df$noreg_cmd[rows] = numa$cmd

  cell_df
}
