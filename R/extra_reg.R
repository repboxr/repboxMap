make_reg_extra_reg_combined_parcels = function(project.dir, parcels=NULL) {
  restore.point("make_reg_extra_reg_combined_parcels")
  parcels = regdb_load_parcels(project.dir,c("base_regcoef","base_core", "base_extra_reg","base_regscalar","base_regstring"), parcels)

  regcoef = parcels$base_regcoef$regcoef
  reg = parcels$base_core$reg
  regscalar = parcels$base_regscalar$regscalar
  regstring = parcels$base_regstring$regstring

  extra = parcels$base_extra_reg
  if (is.null(extra) | length(extra)==0) {
    parcels$.reg = list(.extra.variants = NULL, reg=reg, regcoef=regcoef, regscalar=regscalar, regstring=regstring)
    return(parcels)
  }

  eregcoef = extra$regcoef
  variants = unique(eregcoef$variant)

  copy_df = get_reg_variants_copy_tabs(variants)
  extra_step_var = extra$regcoef %>%
    select(step, variant) %>%
    unique()

  reg_copy = extra_step_var %>%
    semi_join(copy_df %>% filter(table=="reg"), by="variant") %>%
    left_join(select(reg,-variant), by="step")

  regscalar_copy = extra_step_var %>%
    semi_join(copy_df %>% filter(table=="regscalar"), by="variant") %>%
    left_join(select(regscalar,-variant), by="step")

  regstring_copy = extra_step_var %>%
    semi_join(copy_df %>% filter(table=="regstring"), by="variant") %>%
    left_join(select(regstring,-variant), by="step")

  parcels$.reg = list(
    .extra.variants = variants,
    reg = bind_rows(reg, reg_copy, extra[["reg"]]),
    regcoef = bind_rows(regcoef, extra$regcoef),
    regscalar = bind_rows(regscalar, regscalar_copy, extra$regscalar),
    regstring = bind_rows(regstring, regstring_copy, extra$regstring)
  )
  parcels
}

get_reg_variants_copy_tabs = function(variants) {
  variant_defs = load_variant_defs()
  #variant_defs = variant_defs[variants]
  lapply(variants, function(v) {
    v_def = variant_defs[[v]]
    own_tabs = v_def$own_tables
    copy_tabs = setdiff(c("reg","regcoef","regscalar","regstring"), own_tabs)
    if (length(copy_tabs)==0) return(NULL)
    tibble(variant=v, table=copy_tabs)
  }) %>% bind_rows()
}

load_variant_defs = function() {
  file = system.file("variants/variants.yml", package = "repboxDB")
  variant_defs = yaml::yaml.load_file(file)
}
