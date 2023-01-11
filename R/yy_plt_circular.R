#' Process a TSV ourput from STRING
#'
#'
#' @param input_df,grouping_col,col_2split Process STRING data
#' @return processed dataframe
#' @export

yy_lengthen_comma <- function(input_df,grouping_col, col_2split){
  return_df = data.frame()
  input_df = as.data.frame(input_df)
  for (row_i in 1:nrow(input_df)){
    list = strsplit(input_df[row_i,col_2split],split = ",")[[1]]
    group_name = input_df[row_i,grouping_col]
    add_df = data.frame(group = rep(group_name,length(list)), list = list)
    return_df = rbind(return_df, add_df)
  }
  return_df
}

#' Make a circular plot from yy_lengthen_comma item
#'
#'
#' @param df,out_name,grouping Linear model from base R
#' @return Forest plot, based on ggplot2
#' @export

yy_circle_plt =function(df,out_name,grouping){
  #df = df[order(df$logFC),] <- we can order before, not included in function

  pdf(out_name,width = 5,height = 5)
  circos.par(start.degree = 90)
  chordDiagram(df,
               annotationTrack = c("grid"), big.gap = 10,directional = 1,
               grid.col = grouping,
               link.sort = TRUE)

  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + 1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))}, bg.border = NA)
  circos.clear()
  dev.off()
}
