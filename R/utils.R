select = dplyr::select

example = function() {
  xval = c(0.8,0.84, 0.003)
  yval = c(0.81, 0.844, 0.02)
  xdeci = c(1,2,3)
  ydeci = c(2,3,1)
  match_all_rounded(xval, xdeci, yval, ydeci, width=1.1)
}

# Match values accounting for different rounding
# Width is relative
match_all_rounded = function(xval, xdeci, yval, ydeci, xwidth=1,ywidth=1, mult="all", add_num=FALSE) {
  restore.point("match_all_rounded")
  xdeci_step = 10^-xdeci
  xlow = xval - 0.5*xwidth*xdeci_step
  xhigh = xval + 0.5*xwidth*xdeci_step

  ydeci_step = 10^-ydeci
  ylow = yval-0.5*ywidth*ydeci_step
  yhigh = yval + 0.5*ywidth*ydeci_step

  dt_x = data.table(ind_x = seq_along(xval), xlow=xlow, xhigh=xhigh,key = c("xlow","xhigh"))
  dt_y = data.table(ind_y = seq_along(yval), ylow=ylow, yhigh=yhigh,key = c("ylow","yhigh"))

  if (NROW(dt_y)>NROW(dt_x)) {
    ol = foverlaps(dt_y, dt_x, mult=mult, which=TRUE, nomatch=NULL)
    res = tibble(ind_x=dt_x$ind_x[ol[[2]]],ind_y = dt_y$ind_y[ol[[1]]])
  } else {
    ol = foverlaps(dt_x, dt_y, mult=mult, which=TRUE, nomatch=NULL)
    res = tibble(ind_x=dt_x$ind_x[ol[[1]]],ind_y = dt_y$ind_y[ol[[2]]])
  }
  if (add_num) {
    xdeci_step = rep(xdeci_step, length.out=length(xval))
    ydeci_step = rep(ydeci_step, length.out=length(yval))

    res$num_x = xval[res$ind_x]
    res$num_y = yval[res$ind_y]
    res$deci_dist = abs(res$num_x - res$num_y)/(pmax(xdeci_step[res$ind_x], ydeci_step[res$ind_y]))

  }
  res
}

