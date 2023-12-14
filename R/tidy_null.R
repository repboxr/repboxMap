forbid_tidy_null = function(forbid=TRUE) {
  options(forbid.tidy.null = forbid)
}
allow_tidy_null = function(forbid=FALSE) {
  options(forbid.tidy.null = forbid)
}

tidy_NULL = function(fun, forbid.null = isTRUE(getOption("forbid.tidy.null"))) {
  if (forbid.null) {
    stop(paste0(fun," cannot be called with NULL argument."))
  }
  return(NULL)

}

rename.NULL = function(...) tidy_NULL("rename")
select.NULL = function(...) tidy_NULL("select")
filter.NULL = function(...) tidy_NULL("filter")
arrange.NULL = function(...) tidy_NULL("arrange")
mutate.NULL = function(...) tidy_NULL("mutate")
transmute.NULL = function(...) tidy_NULL("transmute")
group_by.NULL = function(...) tidy_NULL("")
summarize.NULL = function(...) tidy_NULL("")
summarise.NULL = function(...) tidy_NULL("")


pivot_wider.NULL = function(...) tidy_NULL("")
pivot_longer.NULL = function(...) tidy_NULL("")
unnest.NULL = function(...) tidy_NULL("")
nest.NULL = function(...) tidy_NULL("")


left_join.NULL = function(...) tidy_NULL("")
inner_join.NULL = function(...) tidy_NULL("")
full_join.NULL = function(...) tidy_NULL("")
right_join.NULL = function(...) tidy_NULL("")
