repbox_map_opts = function(
  reg_min_match_score_by_art_ncoef=c(0.1, 0.1, (0.75+2*0.1-0.0001)/3, (0.75 + 3*0.1 - 0.0001)/4),
  # if there are more coefficients
  reg_min_match_score = 0.358,
  approx_map_max_deci_dist = 2, # measured in decis
  table_ref_differs_shrink_factor = 0.9,
  reg_vs_noreg_map_factor = 0.8,
  reg_ignore_missing_data = TRUE,
  noreg_ignore_missing_data = TRUE
) {
 list(
   reg_min_match_score_by_art_ncoef=reg_min_match_score_by_art_ncoef,
   reg_min_match_score = reg_min_match_score,
   approx_map_max_deci_dist = approx_map_max_deci_dist,
   table_ref_differs_shrink_factor = table_ref_differs_shrink_factor,
   reg_vs_noreg_map_factor = reg_vs_noreg_map_factor,
   reg_ignore_missing_data = reg_ignore_missing_data,
   noreg_ignore_missing_data = noreg_ignore_missing_data
  )

}
