# Strategy: We will only store the aggregate match quality on
# regression level, not the information for the single
# coefficient matches. We will create another function
# that can compute the coefficient matches on demand
# That should work sufficiently quick and we don't need to
# worry about excessive storage size of our matching data

match_project_reg = function(project_dir, numa=NULL,  parcels=NULL, opts = repbox_map_opts(), verbose=TRUE) {
  restore.point("match_project_reg")

  parcels = regdb_load_parcels(project_dir,c("art_reg","base_regcoef","base_core", "art_tab_note_paren_type","base_extra_reg"), parcels)


  art_reg = parcels$art_reg[["art_reg"]]
  if (is.null(art_reg)) {
    if (verbose) cat("\nNo regressions extracted from article.\n")
    return(parcels)
  }
  art_regcoef = parcels$art_reg$art_regcoef
  if (NROW(art_regcoef)==0) {
    if (verbose)  cat(paste0("\n\tNo regression tables found in article of project ", project_dir))
    return(parcels)
  }


  art_regcoef$a.row = seq_rows(art_regcoef)

  art_regcoef = left_join(art_regcoef, select(art_reg, regid, art_ncoef = ncoef), by="regid")

  reg = parcels$.reg$reg
  if (is.null(reg)) {
    if (verbose) cat("\nNo regressions extracted from supplement.\n")
    return(NULL)
  }

  regcoef = parcels$.reg$regcoef
  regcoef$c.row = seq_rows(regcoef)
  regcoef = left_join(regcoef, select(reg, step, variant, code_ncoef = ncoef), by=c("step","variant"))

  if (NROW(regcoef)==0) {
    if(verbose) cat(paste0("\n\tNo regression results from supplement of project ", project_dir))
    return(parcels)
  }

  # Map all matched coefficients from article and supplement
  mdf1 =
    inner_join(art_regcoef %>% rename(a_coef=coef, a_coef_deci = coef_num_deci), numa, by=c(a_coef="a_num", a_coef_deci = "a_deci"))
  mdf1 = inner_join(regcoef %>% rename(c_label=label, c_coef=coef) %>% select(-artid), mdf1, by=c(c_coef="c_num")) %>%
    left_join(select(art_reg, regid, tabid), by="regid")

  # We can rule out that p-values are given in parenthesis
  # if in a table some value in the parenthesis is larger than 1
  # or negative
  p_mdf1 = mutate(mdf1, paren_type = "p", c_paren = p) %>%
    group_by(tabid) %>%
    filter(!any(is.true(paren>1 | paren < 0))) %>%
    ungroup()


  # 3 Versions for interpretation of value in ( ): se, t or p
  coef_mdf = bind_rows(
    mutate(mdf1, paren_type = "se", c_paren = se),
    mutate(mdf1, paren_type = "t", c_paren = t),
    p_mdf1
  )

  note_paren_type = parcels$art_tab_note_paren_type$art_tab_note_paren_type
  if (!is.null(note_paren_type)) {
    coef_mdf = left_join(coef_mdf, select(note_paren_type, tabid, note_paren_type=paren_type), by="tabid")

    coef_mdf = coef_mdf %>%
      filter(is.na(note_paren_type) | paren_type == note_paren_type)

    # For tables for which no paren type is specified
    # we give a bonus for paren_types that are more often used in
    # other tables
    type_bonus = note_paren_type %>%
      group_by(paren_type) %>%
      summarize(
        paren_type_bonus = n() / NROW(note_paren_type)
      ) %>%
      na.omit()
    coef_mdf = coef_mdf %>%
      left_join(type_bonus, by="paren_type") %>%
      mutate(paren_type_bonus = na.val(paren_type_bonus,0))

  } else {
    coef_mdf$paren_type_bonus = ifelse(coef_mdf$paren_type=="se",0.05,0)
  }


  # Match again all parentheses:
  # That is because we also want to have rows where just the parenthesis maps
  # but not the coefficient
  paren_mdf_a =
    inner_join(art_regcoef %>% rename(a_coef=coef, a_coef_deci=coef_num_deci), numa, by=c(paren="a_num", paren_num_deci = "a_deci"))

  regcoef2 = regcoef %>% rename(c_label=label, c_coef=coef) %>% select(-artid)

  paren_mdf = bind_rows(
    inner_join(regcoef2 ,paren_mdf_a, by=c(se="c_num")) %>% mutate(paren_type = "se", c_paren = se),
    inner_join(regcoef2 ,paren_mdf_a, by=c(t="c_num")) %>% mutate(paren_type = "t", c_paren = t),
    inner_join(regcoef2 ,paren_mdf_a, by=c(p="c_num")) %>% mutate(paren_type = "p", c_paren = p)
  )

  paren_mdf = anti_join(paren_mdf, coef_mdf , by=c("a.row","c.row"))

  mdf = bind_rows(coef_mdf, paren_mdf)
  #mdf = left_join(mdf, select(art_reg, regid, tabid), by="regid")

  #colnames(paren_mdf)
  #colnames(mdf)
  #setdiff(colnames(mdf),intersect(colnames(paren_mdf), colnames(mdf)))
  #setdiff(colnames(paren_mdf),intersect(colnames(paren_mdf), colnames(mdf)))
  #setequal(colnames(paren_mdf), colnames(mdf))


  # Compute match of coefficient and parentheses
  mdf = mdf %>% mutate(
    coef_deci_diff = abs(a_coef-c_coef) / 10^-a_coef_deci,
    paren_deci_diff = abs(paren-c_paren) / 10^-paren_num_deci,
    paren_type_bonus = na.val(paren_type_bonus, 0)
  )


  approx_dist = opts$approx_map_max_deci_dist

  # pair refers to a coef / paren pair
  mdf = mdf %>% mutate(
    pair_match_score = case_when(
      coef_deci_diff <= 0.501 & paren_deci_diff <= 0.501 ~ 1,
      # Single small rounding error
      coef_deci_diff <= 0.501 & paren_deci_diff <= approx_dist ~ 0.75,
      coef_deci_diff <= approx_dist & paren_deci_diff <= 0.501 ~ 0.75,
      # Double small rounding error
      coef_deci_diff <= approx_dist & paren_deci_diff <= approx_dist ~ 0.1,
      # Only one coefficient matches
      coef_deci_diff < 0.501 ~ 0.1,
      paren_deci_diff < 0.501 ~ 0.1,
      # Only one coefficient matches with a small rounding error
      coef_deci_diff <= approx_dist ~ 0.01,
      paren_deci_diff <= approx_dist ~ 0.01,
      TRUE ~ 0
    ),
    pair_match_type = case_when(
      coef_deci_diff <= 0.501 & paren_deci_diff <= 0.501 ~ "exact",
      # Single small rounding error
      coef_deci_diff <= 0.501 & paren_deci_diff <= approx_dist ~ "paren_approx",
      coef_deci_diff <= approx_dist & paren_deci_diff <= 0.501 ~ "coef_approx",
      # Double small rounding error
      coef_deci_diff <= approx_dist & paren_deci_diff <= approx_dist ~ "both_approx",
      # Only one coefficient matches
      coef_deci_diff < 0.501 ~ "just_coef",
      paren_deci_diff < 0.501 ~ "just_paren",
      # Only one coefficient matches with a small rounding error
      coef_deci_diff <= approx_dist ~ "just_coef_approx",
      paren_deci_diff <= approx_dist ~ "just_paren_approx",
      TRUE ~ "no_match"
    )
  )

  # Multiple (coef, se) pairs from code may
  # match a particular (coef, se) pair from the article
  # We will just keep one pair (per variant) with the highest points
  mdf = mdf %>%
    group_by(a.row, step, paren_type, variant) %>%
    arrange(desc(pair_match_score)) %>%
    slice(1) %>%
    ungroup()


  # Perform matchings for both the big regression and small regression
  # interpretation of an article's regression tables
  #temp = mdf %>% filter(step == 27)

  big_res = mdf_to_big_reg_matches(project_dir, mdf, parcels, opts)

  small_res = mdf_to_small_reg_matches(project_dir, mdf, parcels, opts, big_res=big_res)

  # Combine results and save
  ma_df = bind_rows(big_res$ma_df, small_res$ma_df)
  stat_df = bind_rows(big_res$stat_df, small_res$stat_df)

  # Add to ma_df info whether also table references fit
  # +1 same tabid
  # -1 different tabid
  # 0 at least one tabid is not well specified
  parcels = parcels_add_tab_ref_to_run_df(parcels)

  run_df = parcels$stata_run_cmd$stata_run_cmd
  ma_df = left_join_overwrite(ma_df, select(run_df, runid, code_tabid=tabid), by="runid") %>%
    mutate(tabid_ref_match = case_when(
      is.true(tabid > 0 & code_tabid > 0 & tabid==code_tabid) ~ 1L,
      is.true(tabid > 0 & code_tabid > 0 & tabid!=code_tabid) ~ -1L,
      TRUE ~ 0L
    ))


  parcels = regdb_save_match_reg(project_dir, ma_df, stat_df, parcels)
  parcels


}

# The big regression interpretation is the default interpretation of a regression table in an article. Each row corresponds to the coefficients of an explanatory variable.
mdf_to_big_reg_matches = function(project_dir, mdf, parcels, opts, from_small=FALSE) {
  restore.point("mdf_to_big_reg_matches")
  reg = parcels$.reg$reg

  # Store for each article and code regression combi
  # the total match score
  ma_df = mdf %>%
    # Our filter above may still keep multiple article code
    # combies for a single coefficient.
    # Just take the first one, so we can afterwards
    # sum up pair_match_scores over all article coefs
    group_by(a.row, c.row, paren_type) %>%
    slice(1) %>%
    # Now we perform the aggregation on regression level
    group_by(tabid, regid, step, variant, paren_type, art_ncoef, code_ncoef, paren_type_bonus) %>%
    summarize(
      match_score = sum(pair_match_score) / first(art_ncoef),
      num_exact = sum(pair_match_type=="exact"),
      num_coef_approx = sum(pair_match_type=="coef_approx"),
      num_paren_approx = sum(pair_match_type=="paren_approx"),
      num_both_approx = sum(pair_match_type=="both_approx"),
      num_just_coef = sum(pair_match_type=="just_coef"),
      num_just_paren = sum(pair_match_type=="just_paren"),
      num_just_coef_approx = sum(pair_match_type=="just_coef_approx"),
      num_just_paren_approx = sum(pair_match_type=="just_paren_approx"),
      num_no_match = first(art_ncoef) - num_exact - num_coef_approx - num_paren_approx - num_both_approx - num_just_coef - num_just_paren - num_just_coef_approx- num_just_paren_approx
    ) %>%
    group_by(regid) %>%
    mutate(
      art_best_match_score = suppressWarnings(max(match_score, na.rm = TRUE))
    ) %>%
    # compute best fit for different types of parenthesis (se, t, or p)
    # we assume that each table only has a single type of parenthesis
    group_by(tabid, paren_type) %>%
    mutate(
      # Is it ok to just take the sum here, even though we may have
      # multiple matches?
      tab_par_match_score = sum(match_score == art_best_match_score)*(1+paren_type_bonus)
    ) %>%
    group_by(tabid) %>%
    mutate(is_best_paren_type = tab_par_match_score == max(tab_par_match_score)) %>%
    ungroup() %>%
    # only keep best paren_type
    filter(is_best_paren_type)

  # Require a minimum match score to keep in data base
  # Be more lenient if there are fewer coefficients

  keep = rep(FALSE, NROW(ma_df))

  n_min_score = opts$reg_min_match_score_by_art_ncoef
  for (i in seq_along(n_min_score)) {
    keep = keep | (ma_df$match_score >= n_min_score[i] & ma_df$art_ncoef == i)
  }
  min_score = opts$reg_min_match_score
  keep = keep | (ma_df$match_score >= n_min_score[i] & ma_df$art_ncoef > length(n_min_score))

  ma_df = ma_df[keep,]

  #duplicated(select(ma_df, regid, step, variant))

  ma_df$coef_match_score = ma_df$match_score
  # Also map runid (better index than step for creating reports)
  ma_df = left_join_overwrite(ma_df, select(reg, step, runid) %>% unique(), by="step")


  if (from_small) return(ma_df)


  stat_df = match_reg_stats(project_dir, ma_df, parcels)

  if (!is.null(stat_df)) {
    stat_agg = stat_df %>%
      group_by(regid, runid) %>%
      summarize(
        art_nstat = n(),
        code_nfoundstat = sum(!is.na(matched)),
        n_matched_stat = sum(is.true(matched)),
        stat_match_score = ifelse(code_nfoundstat > 0, n_matched_stat / code_nfoundstat, NA_real_)
      )

    ma_df = left_join_overwrite(ma_df, stat_agg, by=c("regid","runid")) %>%
      mutate(match_score = ifelse(is.na(stat_match_score), coef_match_score,
                                  (coef_match_score * art_ncoef + stat_match_score * code_nfoundstat) /
                                    (art_ncoef + code_nfoundstat)
      ))
  } else {
    ma_df$art_nstat = 0
    ma_df$stat_match_score = 0
    ma_df$match_score = ma_df$coef_match_score
    ma_df$code_nfoundstat = 0
  }

  artid = basename(project_dir)
  ma_df$artid = artid

  # To do: check why sometimes duplicate rows exist
  ma_df = unique(ma_df)

  return(list(ma_df = ma_df, stat_df = stat_df))

}


# Small regressions interpret each row in the table as a separate regression while a big regression interprets each row as an explanatory variable
mdf_to_small_reg_matches = function(project_dir, mdf, parcels, opts, big_res) {
  restore.point("mdf_to_small_reg_matches")
  art_sreg = parcels$art_reg$art_small_reg

  # Convert matches to matches to small regressions
  small_mdf = mdf %>%
    rename(big_regid = regid) %>%
    left_join(select(art_sreg,-artid), by = c("big_regid","coef_pos")) %>%
    filter(!is.na(regid)) %>%
    mutate(art_ncoef = 1)

  # Rerun code for big_reg using the converted mdf
  # but don't yet map regression statistics
  ma_df = mdf_to_big_reg_matches(project_dir, mdf=small_mdf,parcels = parcels,opts = opts, from_small=TRUE)

  stat_df = big_res$stat_df %>%
    rename(big_regid = regid) %>%
    left_join(select(art_sreg,-artid), by = c("big_regid"))

  if (!is.null(stat_df)) {
    stat_agg = stat_df %>%
      group_by(regid, runid) %>%
      summarize(
        art_nstat = n(),
        code_nfoundstat = sum(!is.na(matched)),
        n_matched_stat = sum(is.true(matched)),
        stat_match_score = ifelse(code_nfoundstat > 0, n_matched_stat / code_nfoundstat, NA_real_)
      )

    ma_df = left_join_overwrite(ma_df, stat_agg, by=c("regid","runid")) %>%
      mutate(match_score = ifelse(is.na(stat_match_score), coef_match_score,
                                  (coef_match_score * art_ncoef + stat_match_score * code_nfoundstat) /
                                    (art_ncoef + code_nfoundstat)
      ))
  } else {
    ma_df$art_nstat = 0
    ma_df$stat_match_score = 0
    ma_df$match_score = ma_df$coef_match_score
  }

  return(list(ma_df = ma_df, stat_df = stat_df))
}



match_reg_stats = function(project_dir, ma_df, parcels=NULL) {
  restore.point("match_reg_stats")
  parcels = regdb_load_parcels(project_dir,c("art_reg","base_regscalar"), parcels)



  art_regstat = parcels$art_reg$art_regstat
  if (NROW(art_regstat)==0) return(NULL)
  reg = parcels$.reg$reg

  # Just essential key columns in ma_df
  m_df = select(ma_df, regid, runid, variant) %>% unique()
  m_df$c.reg.row = match(paste0(m_df$runid,"|", m_df$variant), paste0(reg$runid,"|", reg$variant))

  # Article regression statistics matched to ma_df
  stat_df = inner_join(m_df, select(art_regstat, regid, a_stat = stat_name, a_num = num, a_deci = num_deci), by="regid") %>% unique()

  obs_stat_df = filter(stat_df,a_stat=="nobs")
  noobs_stat_df = filter(stat_df, a_stat != "nobs")
  #if (NROW(obs_stat_df)>0) {
    obs_stat_df$c_stat = "nobs"
    obs_stat_df$c_num = reg$nobs[obs_stat_df$c.reg.row]
    obs_stat_df$matched = obs_stat_df$a_num == obs_stat_df$c_num

    # Alternatively try nobs_org
    # that is not correct, but if it matches
    # most likely we have identified the correct
    # regression
    rows = !obs_stat_df$matched

    obs_stat_df$c_num[rows]=reg$nobs_org[obs_stat_df$c.reg.row[rows]]
    obs_stat_df$c_stat[rows] = "nobs_org"
    obs_stat_df$matched[rows] = obs_stat_df$a_num[rows] == obs_stat_df$c_num[rows]
  #}

  # Other stats than nobs

  # Match general regscalars
  def_df = get_regstat_map_def()
  regscalar = parcels$base_regscalar$regscalar %>%
    filter(!is.na(scalar_val)) %>%
    left_join(def_df, by="scalar_name") %>%
    semi_join(art_regstat, by="stat_name") %>%
    left_join(reg %>% select(step, runid),by="step")

  # We use foverlaps to match
  # regscalar and art_regstat
  # accounting for rounding errors

  art_regstat = art_regstat %>% mutate(
    a_num_low = num - 1.01*10^-num_deci,
    a_num_high = num + 1.01*10^-num_deci
  ) %>%
    rename(a_num=num, a_deci=num_deci)

  regscalar = regscalar %>% mutate(
    c_num_low = scalar_val - 1e-10,
    c_num_high = scalar_val + 1e-10
  ) %>% mutate(c_num = scalar_val)

  a_keys = c("stat_name", "a_num_low", "a_num_high")
  c_keys = c("stat_name", "c_num_low", "c_num_high")

  # Don't use setDT. This has strange side effects! Extremely hard to debug
  #dt_x = setDT(art_regstat[,c(a_keys,"a_num","a_deci","regid")], key = a_keys)
  #dt_y = setDT(regscalar[,c(c_keys, "c_num","variant","runid")], key=c_keys)

  dt_x = as.data.table(art_regstat[,c(a_keys,"a_num","a_deci","regid")])
  setkeyv(dt_x, a_keys)
  dt_y = as.data.table(regscalar[,c(c_keys, "c_num","variant","runid")])
  setkeyv(dt_y, c_keys)

  ol_df = foverlaps(dt_x, dt_y,by.x = a_keys, by.y=c_keys,nomatch=NULL, which=FALSE) %>% as_tibble()

  stat_df = left_join(noobs_stat_df %>% select(-a_num, -a_deci), ol_df, by=c(a_stat="stat_name","variant","runid","regid"))

  stat_df$deci_diff = abs(stat_df$a_num - stat_df$c_num) / abs(10^-(stat_df$a_deci))
  stat_df$matched = stat_df$deci_diff < 0.501
  stat_df$c_stat = stat_df$a_stat

  bind_rows(obs_stat_df, stat_df) %>% mutate(found = is.true(matched))
}


regdb_save_match_reg = function(project_dir, ma_df, stat_df, parcels=lit()) {
  restore.point("regdb_save_match_reg")
  if (NROW(stat_df)>0) {
    stat_df$artid = basename(project_dir)
    stat_df$stat_name = stat_df$a_stat
  } else {
    stat_df = NULL
  }
  regdb_check_data(stat_df, "match_regstat")

  regdb_check_data(ma_df, "match_reg")
  parcels$match_reg =  list(match_reg=ma_df)
  parcels$match_regstat = match_regstat=list(match_regstat=stat_df)
  regdb_save_parcels(parcels[c("match_reg","match_regstat")], dir = file.path(project_dir, "map","regdb"))

  parcels
}





regdb_save_match_reg = function(project_dir, ma_df, stat_df, parcels=lit()) {
  restore.point("regdb_save_match_reg")
  if (NROW(stat_df)>0) {
    stat_df$artid = basename(project_dir)
    stat_df$stat_name = stat_df$a_stat
  } else {
    stat_df = NULL
  }
  regdb_check_data(stat_df, "match_regstat")

  regdb_check_data(ma_df, "match_reg")
  parcels$match_reg =  list(match_reg=ma_df)
  parcels$match_regstat = match_regstat=list(match_regstat=stat_df)
  regdb_save_parcels(parcels[c("match_reg","match_regstat")], dir = file.path(project_dir, "map","regdb"))

  parcels
}


compare_ma_df_art_reg = function(art_reg, ma_df) {
  best_ma_df = ma_df %>%
    group_by(regid) %>%
    filter(match_score == max(match_score)) %>%
    summarize(
      match_score = max(match_score),
      num_code_regs = n()
    )

  art_reg = left_join_overwrite(art_reg, best_ma_df, by="regid") %>%
    mutate(num_code_regs = na.val(num_code_regs,0))
}

get_regstat_map_def = function() {
  # Alternatively try to save a csv
  li = list(
    adj_r2 = c("r2_a", "ar2"),
    F = c("F"),
    pseudo_r2 = c("r2_p"),
    r2_o = c("r2_o"),
    r2_w = c("r2_w"),
    r2_b = c("r2_b"),
    ncluster = "N_clust",
    loglik = c("ll"),
    pbar= c("pbar")
  )

  lapply(seq_along(li), function(i) {
    tibble(stat_name=names(li)[i], scalar_name = li[[i]])
  }) %>% bind_rows()
}
