library(VennDiagram)

plot.venn <- function(x,width=3,height=3){
    v1 <- venn.diagram(x,filename=NULL)
    grid.newpage()
    grid.draw(v1)
}
