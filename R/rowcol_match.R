# Ideas for more advanced matching of numbers using similiarity structures

example.matching = function() {
  project.dir = "/home/rstudio/statabox/supp/aejapp_vol_6_issue_3_article_7"

  project.dir = "~/repbox/projects/testsupp"
  repbox.dir = paste0(project.dir, "/repbox")

  rstudioapi::filesPaneNavigate(paste0(project.dir,"/repbox/www_rstudio"))
  rstudioapi::filesPaneNavigate("~/repbox/repbox/R")


  tabs.df = readRDS(paste0(repbox.dir,"/arttab.Rds"))
  ma = readRDS(paste0(repbox.dir,"/matched_tabs.Rds"))

  loc.df = tabs.df$loc.df[[1]]
  num.df = ma$num.df
  res  = rowcol.match(loc.df, num.df = num.df)
  roco.li = rowcol.matches(tabs.df)

  cora.df = lapply(roco.li, rowcol.to.cora.df) %>% bind_rows()

}

rowcol.to.cora.df = function(roco) {
  restore.point("rowcol.to.cora.df")
  cora.df = roco$tot.sum

  keep.mdf = roco$keep.mdf

  cora.df$mdf = lapply(seq_len(NROW(cora.df)), function(i) {
    if (cora.df$is.regcmd[i]) {
      rows = keep.mdf$is.regcmd
    } else {
      rows = which(keep.mdf$cmd == cora.df$cmd[i])
    }
    mdf = keep.mdf[rows, c("re.row", "tabid","arow","acol","cmd","is.regcmd")]
    mdf
  })

  cora.df

}

rowcol.matches = function(tabs.df, ma.df) {
  restore.point("rowcol.matches")
  num.df = ma$num.df

  res = prepare.num.df.for.matching(num.df)
  num.df = res$num.df; num.dt = res$num.dt


  res.li = lapply(seq_len(NROW(tabs.df)), function(i) {
    rowcol.match(tabs.df$loc.df[[i]], num.dt=num.dt, num.df=num.df)
  }) %>% unlist(recursive = FALSE)
  res.li
}


rowcol.match = function(loc.df, num.dt=NULL, num.df=NULL) {
  restore.point("rowcol.match")
  if (is.null(num.dt)) {
    res = prepare.num.df.for.matching(num.df)
    num.df = res$num.df; num.dt = res$num.dt
  }

  res = prepare.loc.df.for.matching(loc.df)
  loc.df = res$loc.df; loc.dt = res$loc.dt

  mdf = find.num.dt.loc.dt.matches(num.dt, loc.dt)

  # Assume a column comes from the same call
  res = col.same.call.matches(mdf,combine.reg = TRUE)
  res$tot.sum
  res$cmd.sum
  res$best.counter

  res$keep.mdf = semi_join(mdf, res$best.counter, by=c("counter","acol"))
  col.same.call = res

  # Assume a row comes from the same call
  res = row.same.call.matches(mdf,combine.reg = TRUE)
  res$keep.mdf = semi_join(mdf, res$best.counter, by=c("counter","arow"))

  row.same.call = res

  list(col.same.cal=col.same.call, row.same.call=row.same.call)
}


prepare.loc.df.for.matching = function(loc.df) {
  restorepoint::restore.point("prepare.loc.df.for.matching")

  lines = sort(unique(loc.df$line))
  loc.df = loc.df %>% ungroup() %>%
    filter(type=="num") %>%
    mutate(loc.row = 1:n()) %>%
    mutate(
      shown.deci = nchar(str.right.of(num.str,".",not.found=rep("", n())))
    ) %>%
    mutate(
      num.lower = num.val - ifelse(shown.deci>0,10^(-shown.deci),0),
      num.upper = num.val + ifelse(shown.deci>0,10^(-shown.deci),0)
    ) %>%
    mutate(
      # a stands for article (crow would be row in code table)
      arow = match(line, lines),
      acol = col
    ) %>%
    arrange(arow, arow)

  # Specify cell format classes
  loc.df = loc.df %>%
    mutate(
      paren.type = case_when(
        has.paren ~ "(",
        has.bracket ~ "[",
        has.curley ~ "{",
        has.perc ~ "%",
        TRUE ~ ""
      ),
      base.class = paste0(paren.type,shown.deci),
      aclass = base.class
    ) %>%
    # If cell below has parentheses, add to class
    group_by(acol) %>%
    mutate(
      n.col.cells = n(),
      has.paren.below = is.true(lead(arow)==arow+1 & !lead(paren.type) %in% c("","%")),
      aclass = paste0(aclass, ifelse(has.paren.below,paste0("|B",lead(base.class)),""))
    ) %>%
    group_by(arow) %>%
    mutate(
      n.row.cells = n(),
      has.paren.right = is.true(lead(arow)==arow+1 & !lead(paren.type) %in% c("","%")),
      aclass = paste0(aclass, ifelse(has.paren.right,paste0("|R",lead(base.class)),""))
    )  %>%
    group_by(arow, aclass) %>%
    mutate(
      n.row.class.cells = n()
    ) %>%
    group_by(acol, aclass) %>%
    mutate(
      n.col.class.cells = n()
    ) %>%
    group_by(aclass) %>%
    mutate(
      n.class.cells = n(),
      n.class.rows = n_distinct(arow),
      n.class.cols = n_distinct(acol)
    ) %>%
    ungroup() %>%
    mutate(n.tab.cells = n())


  # Show a test matrix
  nrow = max(loc.df$arow)
  ncol = max(loc.df$acol)
  m = matrix("",nrow, ncol)
  inds = cbind(loc.df$arow, loc.df$acol)
  m[inds] = paste0(loc.df$big.str,"  ", loc.df$aclass)


  loc.dt =  loc.df %>%
    rename(anum = num.val, adeci=shown.deci, anum.str=num.str, aline=line) %>%
    as.data.table()
  setkeyv(loc.dt, c("num.lower","num.upper"))


  list(loc.df=loc.df, loc.dt=loc.dt)
}

prepare.num.df.for.matching = function(num.df) {
  num.df = num.df %>%
    ungroup() %>%
    mutate(
      num.row = 1:n()
    ) %>%
    mutate(
      cclass = paste0(
        cmd,
        # Number comes after an equal sign
        ifelse(after_equal,"=",""),
        # Assume numbers in table are right formated and always
        # end at same position
        ifelse(num.table.part >1,paste0("T",line.pos.end),"")
      )
    ) %>%
    group_by(counter) %>%
    mutate(cnum.counts = n()) %>%
    ungroup()

  num.dt = num.df %>%
    ungroup() %>%
    mutate(num.val2 = num.val) %>%
    as.data.table()
  setkeyv(num.dt, c("num.val","num.val2"))
  list(num.df=num.df, num.dt = num.dt)
}


find.num.dt.loc.dt.matches = function(num.dt, loc.dt) {
  restore.point("fin.num.dt.loc.dt.matches")
  library(dplyr)

  mdt = foverlaps(x=num.dt, y=loc.dt,type="within", mult="all", nomatch = NULL)

  mdf = as_tibble(mdt)

  mdf = mdf %>%
    mutate(
      exact_match = round(num.val, adeci) == anum
    )

  mdf

}


row.same.call.matches = function(mdf, max.missing.share = 0.4,max.missing.num = 1,..., same.call = "row") {
  restore.point("row.same.call.matches")

  # Check same commands in PDF row
  counter.sum = mdf %>%
    group_by(tabid,donum, arow, cmd,is.regcmd, counter,cnum.counts, n.tab.cells) %>%
    summarize(
      counter.loc.row.matched = n_distinct(loc.row)
    ) %>%
    group_by(tabid,donum, arow, cmd, n.tab.cells) %>%
    mutate(
      is.best.cmd.counter = (counter.loc.row.matched == max(counter.loc.row.matched))
    ) %>%
    ungroup() %>%
    arrange(arow, desc(counter.loc.row.matched),is.regcmd, cnum.counts)

  # Let us try to find good cmd sets
  cmd.sum = counter.sum %>%
    group_by(tabid, donum, arow, cmd, is.regcmd, n.tab.cells) %>%
    summarize(
      best.matched = first(counter.loc.row.matched)
    )

  tot.sum = cmd.sum %>%
    group_by(tabid, donum, cmd, is.regcmd, n.tab.cells) %>%
    summarize(
      same.call = same.call,
      nrows = n(),
      match.num = sum(best.matched),
      missing.num = first(n.tab.cells)-match.num,
      missing.share = missing.num / first(n.tab.cells)
    ) %>%
    arrange(missing.num) %>%
    ungroup() %>%
    mutate(is.best = (missing.num == min(missing.num))) %>%
    ungroup() %>%
    #filter(is.best | missing.num <= max.missing.num | missing.share <= max.missing.share)
    filter(missing.num <= max.missing.num | missing.share <= max.missing.share)

  #return(list(tot.sum=tot.sum, cmd.sum=cmd.sum))


  best.counter = tot.sum %>%
    left_join(counter.sum, by=c("tabid", "donum","cmd"),suffix=c("",".ignore")) %>%
    filter(is.best.cmd.counter)

  list(tot.sum=tot.sum, best.counter=best.counter)
}




row.class.same.call.matches = function(mdf,   max.missing.share = 0.2, max.missing.num = 2,...,  same.call="col") {
  restore.point("row.class.same.call.matches")

  # Check same commands in PDF row
  counter.sum = mdf %>%
    group_by(tabid,donum, arow,  n.class.cells, n.class.rows, aclass, cmd,is.regcmd, counter,cnum.counts) %>%
    summarize(
      counter.loc.row.matched = n_distinct(loc.row)
    ) %>%
    group_by(tabid,donum, arow, aclass, cmd) %>%
    mutate(
      is.best.cmd.counter = (counter.loc.row.matched == max(counter.loc.row.matched))
    ) %>%
    ungroup() %>%
    arrange(arow, desc(counter.loc.row.matched),is.regcmd, cnum.counts)

  # Let us try to find good cmd sets
  cmd.sum = counter.sum %>%
    group_by(tabid, donum, arow,n.class.cells, n.class.rows, aclass, cmd, is.regcmd) %>%
    summarize(
      best.matched = first(counter.loc.row.matched)
    )

  class.sum = cmd.sum %>%
    group_by(tabid, donum, aclass, n.class.cells, n.class.rows, cmd, is.regcmd) %>%
    summarize(
      same.call = same.call,
      nrows = n(),
      match.num = sum(best.matched),
      missing.num = first(n.class.cells)-match.num,
      missing.share = missing.num / first(n.class.cells)
    ) %>%
    arrange(aclass, missing.num) %>%
    group_by(aclass) %>%
    mutate(is.best = (missing.num == min(missing.num))) %>%
    ungroup() %>%
    filter(missing.num <= max.missing.num | missing.share <= max.missing.share)


  best.counter = class.sum %>%
    group_by(tabid, aclass) %>%
    left_join(counter.sum, by=c("aclass","donum","cmd"),suffix=c("",".ignore")) %>%
    filter(is.best.cmd.counter)


  list(tot.sum=class.sum, best.counter=best.counter)
}


col.same.call.matches = function(mdf, max.missing.share = 0.5,max.missing.num = 20, combine.reg = FALSE, same.call="col") {
  restore.point("col.same.call.matches")

  if (combine.reg) {
    mdf$org.cmd = mdf$cmd
    mdf$cmd[mdf$is.regcmd] = "REGRESSION"
  }

  #unique(mdf$donum)
  #mdf = filter(mdf, donum=="table3", is.regcmd)

  # Check same commands in PDF acol
  counter.sum = mdf %>%
    group_by(tabid,donum, acol, cmd,is.regcmd, counter,cnum.counts, n.tab.cells) %>%
    summarize(
      counter.loc.col.matched = n_distinct(loc.row)
    ) %>%
    group_by(tabid,donum, acol, cmd, n.tab.cells) %>%
    mutate(
      is.best.cmd.counter = (counter.loc.col.matched == max(counter.loc.col.matched))
    ) %>%
    ungroup() %>%
    arrange(acol, desc(counter.loc.col.matched),is.regcmd, cnum.counts)

  # Let us try to find good cmd sets
  cmd.sum = counter.sum %>%
    group_by(tabid, donum, acol, cmd, is.regcmd, n.tab.cells) %>%
    summarize(
      best.matched = first(counter.loc.col.matched)
    )

  tot.sum = cmd.sum %>%
    group_by(tabid, donum, cmd, is.regcmd, n.tab.cells) %>%
    summarize(
      same.call = same.call,
      nrows = n(),
      match.num = sum(best.matched),
      missing.num = first(n.tab.cells)-match.num,
      missing.share = missing.num / first(n.tab.cells)
    ) %>%
    arrange(missing.num) %>%
    ungroup() %>%
    mutate(is.best = (missing.num == min(missing.num))) %>%
    ungroup() %>%
    #filter(is.best | missing.num <= max.missing.num | missing.share <= max.missing.share)
    filter(missing.num <= max.missing.num | missing.share <= max.missing.share)

  #return(list(tot.sum=tot.sum, cmd.sum=cmd.sum))


  best.counter = tot.sum %>%
    #group_by(aclass) %>%
    left_join(counter.sum, by=c("tabid", "donum","cmd"),suffix=c("",".ignore")) %>%
    filter(is.best.cmd.counter)

  list(tot.sum=tot.sum, cmd.sum=cmd.sum, best.counter=best.counter)
}


