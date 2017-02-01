
# Boxplot -------------------------------

box_plot <- function(data, xvar, yvar, xlab="", ylab="") {
  ggplot2::ggplot(data, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_jitter(ggplot2::aes_string(colour = xvar, alpha = 0.4), width = 0.33) +
    ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar), 
                          outlier.shape = NA) +
    ggplot2::stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                          fun.data = function(x){
                            return(c(y = median(x), ymin = median(x), ymax = median(x)))
                          }) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(), 
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey 30"), #angle = 45
                   axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
}

# Scatterplot -------------------------------------------

scatter_plot = function(data, xvar, yvar, xlab='', ylab='', line = TRUE, facet = FALSE) {
  myplot <- ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=yvar)) +
    ggplot2::geom_point(colour = "#0db7c4", size = 1) + #mapping=aes(color=mcr_status)
    ggplot2::theme_minimal() + 
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), 
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey50"), #angle = 45
                   axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  if(line == TRUE & facet == FALSE) {
    myplot +
      ggplot2::geom_smooth(method = lm, colour = "grey50")
  } else if(line == TRUE & facet == TRUE) {
    myplot +
      ggplot2::geom_smooth(method = lm, colour = "grey50") +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else if(line == FALSE & facet == TRUE) {
    myplot +
      ggplot2::facet_grid(~fVN, switch = "x")
  } else {
    myplot
  }
}

# Histogram -----------------------------------------------

histo_plot = function(data, variable, bin, xlab='') {
  ggplot2::ggplot(data, ggplot2::aes_string(x=variable)) +
    ggplot2::geom_histogram(binwidth=bin,
                            colour='#0db7c4', fill='#0db7c4') +
    ggplot2::xlab(xlab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), 
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "grey"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey"),
                   axis.text.x = ggplot2::element_text(colour = "grey 30"), #angle = 45
                   axis.title = ggplot2::element_text(size = 10)) 
}