
# Bootstrap tabset panels for none-shiny html pages
repboxTabSetPanel = function(id=paste0("tabset_",random.string()), tabids=paste0("tab_",random.string(length(contents))), tabnames, contents, type=c("tabs", "pills")[1], active = tabids[1], ul.class="", fix.top=FALSE) {
  restore.point("repboxTabSetPanel")
  # # navList div
  # head = paste0('<div class="container flex-wrap">
  # <ul id="',id,'" class="nav nav-tabs flex-wrap', ul.class,'" role="tablist">')
  # inner = paste0(
  #   '<li', ifelse(tabids==active,' class="active">','>'),
  #   '<a href="#',tabids,'" role="tab" data-toggle="tab">',tabnames,'</a>',
  #   '</li>', collapse="\n")
  # foot = paste0('</ul></div>')
  # navList=paste0(head,"\n",inner,"\n", foot)

  head = paste0('
  <ul id="',id,'" class="nav nav-',type,' ',if (fix.top) 'navbar-fixed-top ', ul.class,'" role="tablist">')
  inner = paste0(
    '<li', ifelse(tabids==active,' class="active">','>'),
    '<a href="#',tabids,'" role="tab" data-toggle="tab">',tabnames,'</a>',
    '</li>', collapse="\n")
  foot = paste0('</ul>')
  navList=paste0(head,"\n",inner,"\n", foot)


  # content div
  head = paste0('<div class="tab-content">')
  inner = paste0(
    '<div class="tab-pane ', ifelse(tabids==active,' active',''),'"',
    ' id = "',tabids,'">\n',
    contents,
    '\n</div>',
    collapse="\n")
  foot = paste0("</div>")
  content = paste0(head,"\n",inner,"\n", foot)

  paste0(navList,"\n", content)
}


repair.id = function(id) {
  id = gsub("[^a-zA-Z0-9_]","_",id)
}

repbox_html_table = function(id="",df,header=colnames(df), class="table-mini table table-striped", extra=NULL) {
  restore.point("repbox_html_table")
  str = paste.df.cols(df,sep="</td><td>")
  inner.tab = paste0("<tr><td>",str,"</td></tr>", collapse="\n")
  if (!is.null(header)) {
    str = paste0(header, collapse="</th><th>")
    head.tab = paste0(
      '<thead>\n',
      paste0("<tr><th>",str,"</th></tr>", collapse="\n"),
      '\n</thead>'
    )
  } else {
    head.tab = NULL
  }
  tab = paste0('<table', if(!is.null(id)) paste0(' id="',id,'" '),if(!is.null(class)) paste0(' class="',class,'" '),extra,'>', head.tab,'\n<tbody>\n',inner.tab,'\n</tbody>\n</table>')
  HTML(tab)
}
