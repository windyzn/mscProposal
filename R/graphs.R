# Boxplot -------------------------------
box_plot <- function(data, xvar, yvar, xlab="", ylab="") {
  ggplot2::ggplot(data, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(colour = xvar, fill = xvar)) +
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

# scale_x_discrete(labels = paste(levels(ds_base$xvar), 
#                                 "\n(N=", table(ds_base$xvar), ")", sep = "")) +



# box_plot = function(data, xvar, yvar, xlab='', ylab='') {
#   ggplot(data, aes(x=xvar, y=yvar)) +
#     geom_boxplot() +
#     # scale_x_discrete(limits=xcat) +
#     xlab(xlab) +
#     ylab(ylab) +
#     theme_bw()
# }