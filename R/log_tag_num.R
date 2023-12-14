# Old functions: need to be updated

make.tab.span.txt = function(tab, tloc.df) {
  restore.point("make.tab.span.txt")

  tloc.df = tloc.df %>%
    mutate(set.span = type=="num") %>%
    group_by(tabid) %>%
    mutate(
      bg.color = case_when(
        !set.span ~ NA_character_,
        # No match in do logs at all
        num.sup.row == 0 ~ "#000",
        # No match in best matching do file
        !is.true(matches.best.donum) ~ "#ccc",
        TRUE ~ NA_character_
      ),
      tab.loc.row = 1:n()
    ) %>%
    ungroup()

  # Since we only have position on the line, we must
  # replace for each line separately
  li.df = tloc.df %>%
    mutate(tloc.row = seq_len(NROW(tloc.df))) %>%
    # Must be arranged in this fashion when given
    #arrange(line, start,end) %>%
    group_by(line) %>%
    summarize(
      from.mat = list(cbind(start, end)),
      replace = list(paste0(
        "<span id='tabnum-",tabid,"-",tloc.row, "'",
        ifelse(is.na(bg.color),
               "class='tabnum' ",
               "class='tabnum-grey' "
        ),
        "style='color: ", color,";",
        ifelse(!is.na(bg.color),
               paste0(" background-color: ", bg.color,";"),
               ""),"'",
        ">", str,"</span>"))
    )

  txt = sep.lines(tab$tab.source.txt)
  lines = li.df$line - tab$source.start.line+1
  txt[lines] = stri_sub_all_replace(txt[lines],from=li.df$from.mat,replacement = li.df$replace)
  merge.lines(txt)
}



tag.numbers.in.text = function(txt, id.prefix, pos=NULL, num.str=NULL, class="num-span") {
  restore.point("tag.numbers.in.text")

  if (is.null(pos)) {
    fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
    pos = str_locate_all(txt, fp)[[1]]
  }
  if (NROW(pos)==0) return(txt)
  if (is.null(num.str))
    num.str = str.at.pos(txt,pos)

  counter = seq_along(num.str)

  prefix = paste0("<span id='", id.prefix,"-", counter,"' class='", class,"'>")
  new.str = paste0(prefix, num.str, "</span>")
  str.replace.at.pos(txt, pos, new = new.str)
}
