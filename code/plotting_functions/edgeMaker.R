# Function to generate paths between each connected node
edgeMaker <- function(whichRow, len = 100, curved = TRUE){
  fromC <- layoutCoordinates[adj_list[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adj_list[whichRow, 2], ]  # Terminus
  
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Group <- paste(adj_list[whichRow, 1:2], collapse = ">")
  return(edge)
}
require(ggplot2)

my_theme <- theme_bw() +
  #scale_color_brewer(palette = "Paired") +
  theme(legend.position = "none",
        text = element_text(family = "Times"),
        plot.margin=grid::unit(c(4,1,-4,0),"mm"),
        panel.margin=grid::unit(c(0,0,0,0),"mm"),
        panel.border = element_rect(colour = "grey10", size = 0.5),
        # panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        plot.title = element_text(hjust = 0.05, vjust = -1.5, size = 9),
        panel.grid = element_blank())