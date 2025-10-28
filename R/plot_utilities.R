one_plot <- function(x, xax = FALSE, yax = FALSE, param, zrange,
                     type,
                     return_mean = FALSE){
  #my_pallete <- colorRampPalette(c("#3A2449", "#c4d7f2", "#f0f7ee"))
  
  x <- x[x$parameter == param,]
  x <- x[order(x$nseason, x$nsite),]
  x <- x[order(x$nsite, decreasing = FALSE),]
  yval <- sort(unique(x$nseason))
  xval <- sort(unique(x$nsite))
  resmat <- matrix(
    x[,type, drop = TRUE],
    ncol = length(unique(x$nseason)),
    nrow = length(unique(x$nsite)),
    byrow = TRUE
  )
  if(any(resmat>zrange[2])){
    resmat[resmat>zrange[2]] <- zrange[2]
  }

    my_pallete <- pals::ocean.thermal(100)

  bbplot::blank(
    xlim = range(xval),
    ylim = range(yval),
    bty = "l"
  )
  bbplot::axis_blank(side = 1, at = sort(unique(x$nsite)), minor = FALSE, tck = -0.03)
  bbplot::axis_blank(side = 2, at = sort(unique(x$nseason)), minor = FALSE, tck = -0.03)
  if(xax){
    bbplot::axis_text(
      text = range(xval),
      side = 1,
      line = 1,
      at = range(xval)
    )
  }
  if(yax){
    bbplot::axis_text(
      text = yval[-c(2,4)],
      side = 2,
      line = 1,
      at = yval[-c(2,4)],
      las = 1
    )
  }
  image(
    x = xval,
    y = yval ,
    z = resmat,
    zlim = zrange,
    col = my_pallete,
    xlab = "",
    ylab = "",
    xaxt = 'n',
    yaxt = 'n',
    add = TRUE
  )
  box(which = "plot", bty = "l", lwd = 1)
  if(return_mean){
    return(mean(x[,type]))
  }
  
}
