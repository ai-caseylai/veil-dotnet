library(data.table)
library(dplyr)
library(lubridate)
library(googleVis)
library(ggplot2)
library(scales)
library(ggrepel)
library(forcats)
library(xts)

plotPieStatic <- function(dataset, y, group) {
  dataset[,group] <- fct_reorder(dataset[,get(group)], dataset[,get(y)])
  dataset <- dataset[order(dataset[,get(y)], decreasing = TRUE), ]
  pieLabels <- data.table(x.breaks = seq(1.25, 1.25, length.out = length(unique(dataset[,get(group)]))),
                          y.breaks = cumsum(dataset[,get(y)]) - dataset[,get(y)]/2,
                          value = dataset[,get(y)],
                          labels = paste(dataset[,get(group)], dataset[,get(y)], percent(dataset[,get(y)]/sum(dataset[,get(y)])), sep='\n'),
                          description = dataset[,get(group)])
  
  g <- ggplot(dataset, aes(x = 1, y = get(y), fill = get(group)))
  g <- g + geom_bar(stat="identity", color='black')
  g <- g + coord_polar(theta='y', start = 0, direction = 1)
  g <- g + guides(fill=guide_legend(override.aes=list(colour=NA)))
  g <- g + geom_label_repel(data = pieLabels, aes(x = x.breaks, y = y.breaks, 
                                                  label = labels, fill = description),
                            box.padding = 1, size = 2.75, show.legend = FALSE, inherit.aes = FALSE)
  g <- g + labs(fill = group)
  g <- g + theme_minimal() + theme(axis.ticks = element_blank(), axis.title = element_blank(),
                                   axis.text.y = element_blank(), axis.text.x = element_blank(),
                                   panel.grid = element_blank(),
                                   legend.margin = margin(0, 0, 0, 0))
  g <- g + scale_fill_brewer(palette = "Set3", direction = -1)
  return(g)
}

gvisResize <- function(gVisObj) {
  # Resize the chart when the window resized
  gVisObj[["html"]][["chart"]][["jsDisplayChart"]] <- paste(
    "\n// Re-draw the graph on window resize\n", 
    "window.addEventListener('resize', drawChart", gVisObj[["chartid"]], ");\n", 
    gVisObj[["html"]][["chart"]][["jsDisplayChart"]], sep = "")
  return(gVisObj)
}

plotPieInteractive <- function(dataset, y, group, width = 640, height = 640) {
  pie <- gvisPieChart(dataset, labelvar = as.character(dataset[, get(group)]),
                      options = list(width = width,
                                     height = height,
                                     chartArea = "{width: '90%', height: '90%'}",
                                     tooltip = "{trigger: 'selection'}",
                                     backgroundColor = "transparent",
                                     sliceVisibilityThreshold = 0,
                                     legend = "{alignment: 'center'}"))
  return(gvisResize(pie))
}

plotBarPerGroupStatic <- function(dataset, x, y, group = NULL, 
                                  y.labels.angle = 0) {
  if (!is.null(group)) {
    # Set data
    g <- ggplot(dataset, aes(x = get(x), y = get(y), group = get(group), 
                             color = get(group), fill = get(group)))
    # Set labels
    g <- g + labs(x = x, y = y, color = group, fill = group)
    g <- g + geom_col(position = position_dodge())
  } else {
    # Set data
    g <- ggplot(dataset, aes(get(x), get(y)))
    # Set labels
    g <- g + labs(x = x, y = y)
    # Plot the bar from data
    g <- g + geom_col(position = position_dodge(), fill = "blue", colour = "blue")
  }
  # Expand the y-axis limit for printing the value at the end of bar
  g <- g + geom_text(aes(y = get(y) * 1.1, label = ""))
  
  # Print the value at the end of each bar
  if(is.integer(dataset[,get(y)])) {
    g <- g + geom_text(aes(label = get(y)), position = position_dodge(width=0.9), 
                       hjust = -0.05)
  } else {
    g <- g + geom_text(aes(label = sprintf("%0.2f", round(get(y), digits = 2))), 
                       position = position_dodge(width=0.9), 
                       hjust = -0.05)
  }
  g <- g + theme_minimal()
  # Set y labels angle and size
  if (y.labels.angle != 0) {
    g <- g + theme(axis.text.y = element_text(size = 10,
                                              angle = y.labels.angle, 
                                              vjust = 0.9))
  } else {
    g <- g + theme(axis.text.y = element_text(size = 10))
  }
  # Flip the coordinate
  g <- g + coord_flip()
  return(g)
}

plotBarPerGroupInteractive <- function(dataset, x, y, group = NULL, 
                                       logScale = FALSE, width = "auto", 
                                       height = 640) {
  if (logScale) {
    logScale <- "logScale: true"
  } else {
    logScale <- "logScale: false"
  }
  optionList <- list(width = width,
                     height = height,
                     chartArea = "{width: '50%', height: '70%'}",
                     backgroundColor = "transparent",
                     legend = "{alignment: 'center'}",
                     bar = "{groupWidth: '90%'}",
                     hAxis = paste("{", 
                                   "title: '", y, "', ", 
                                   "viewWindowMode: 'maximized', ",
                                   logScale,
                                   "}", sep = ""),
                     vAxis = paste("{title: '", x, "'}", sep = ""))
  if (!is.null(group)) {
    dcastFormula <- as.formula(paste(x, "~", group, sep = ""))
    plotData <- dcast(dataset, dcastFormula, value.var = y)
    optionList$legend <- paste("{alignment: 'center', ",
                               "}", sep = "")
    bar <- gvisBarChart(plotData, xvar = x, options = optionList)
  } else {
    optionList$legend <- "{position: 'none'}"
    bar <- gvisBarChart(dataset, xvar = x, yvar = y, options = optionList)
  }
  return(gvisResize(bar))
}

plotColumnPerGroupStatic <- function(dataset, x, y, group = NULL, x.labels.angle = 0) {
  if (!is.null(group)) {
    # Set data
    g <- ggplot(dataset, aes(x = get(x), y = get(y), group = get(group), 
                             color = get(group), fill = get(group)))
    # Set labels
    g <- g + labs(x = x, y = y, color = group, fill = group)
    g <- g + geom_bar(stat = "identity")
  } else {
    # Set data
    g <- ggplot(dataset, aes(get(x), get(y)))
    # Set labels
    g <- g + labs(x = x, y = y)
    g <- g + geom_bar(stat = "identity", fill = "blue", colour = "blue")
  }
  # Print the value at the end of each bar
  if(is.integer(dataset[,get(y)])) {
    g <- g + geom_text(aes(label = get(y)), position = position_dodge(width=0.9), 
                       vjust = -0.1)
  } else {
    g <- g + geom_text(aes(label = sprintf("%0.2f", round(get(y), digits = 2))), 
                       position = position_dodge(width=0.9), 
                       vjust = -0.1)
  }
  g <- g + theme_minimal()
  # Set x labels angle and size
  if (x.labels.angle != 0) {
    g <- g + theme(axis.text.x = element_text(size = 12, 
                                              angle = x.labels.angle, 
                                              vjust = 0.75))
  } else {
    g <- g + theme(axis.text.x = element_text(size = 12))
  }
  return(g)
}

plotColumnPerGroupInteractive <- function(dataset, x, y, group = NULL,
                                          logScale = FALSE, width = "auto", 
                                          height = 640) {
  if (logScale) {
    logScale <- "logScale: true"
  } else {
    logScale <- "logScale: false"
  }
  optionList <- list(width = width,
                     height = height,
                     chartArea = "{width: '50%', height: '70%'}",
                     backgroundColor = "transparent",
                     legend = "{alignment: 'center'}",
                     bar = "{groupWidth: '90%'}",
                     vAxis = paste("{", 
                                   "title: '", y, "', ", 
                                   "viewWindowMode: 'maximized', ",
                                   logScale,
                                   "}", sep = ""),
                     hAxis = paste("{title: '", x, "'}", sep = ""))
  if (!is.null(group)) {
    dcastFormula <- as.formula(paste(x, "~", group, sep = ""))
    plotData <- dcast(dataset, dcastFormula, value.var = y)
    optionList$legend <- paste("{alignment: 'center', ",
                               "}", sep = "")
    columnBar <- gvisColumnChart(plotData, xvar = x, options = optionList)
  } else {
    optionList$legend <- "{position: 'none'}"
    columnBar <- gvisColumnChart(dataset, xvar = x, yvar = y, options = optionList)
  }
  return(gvisResize(columnBar))
}

plotCalendarHeatmap <- function(dataset, datevar, numvar, title,
                                width = "auto", height = "auto") {
  if (!is.Date(dataset[[datevar]]) || !is.POSIXt(dataset[[datevar]])) {
    dataset[[datevar]] <- as.Date(dataset[[datevar]])
  }
  calender <- gvisCalendar(dataset, datevar = datevar, numvar = numvar,
                      options=list(title=title, width = width))
  return(gvisResize(calender))
}