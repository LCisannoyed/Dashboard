
#-------------------------
# For the Item.Side of the wright Map
#-------------------------

itemClassic.LC<- function (thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", 
                                      show.axis.logits = "R", oma = c(0, 0, 0, 5), cutpoints = NULL, 
                                      ...) 
{
  Nbins <- function(thr, itemRange) {
    breaks <- seq(from = itemRange[1], to = itemRange[2], 
                  length.out = 25)
    if (min(thr, na.rm = TRUE) < min(breaks)) 
      breaks <- c(min(thr, na.rm = TRUE), breaks)
    if (max(thr, na.rm = TRUE) > max(breaks)) 
      breaks <- c(breaks, max(thr, na.rm = TRUE))
    return(breaks)
  }
  binItems <- function(level, labelMat, cutMat) {
    paste(sort(labelMat[cutMat == level]), collapse = " | ")
  }
  thr <- as.matrix(thr)
  nI <- dim(thr)[1]
  nL <- dim(thr)[2]
  if (is.null(yRange)) {
    yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))
    yA <- (yRange[2] - yRange[1]) * 0.1
    yRange <- yRange + c(-yA, yA)
  }
  par(oma = oma)
  par(mgp = c(1, 0.2, 0))
  plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, 
       ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = 0.8, 
       font.lab = 3)
  box(bty = "o")
  usr <- par("usr")
  par(mgp = c(3, 1, 0))
  if (show.axis.logits == "R" | show.axis.logits == TRUE) {
    axis(4, las = 1, cex.axis = 0.7, font.axis = 2)
    mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
  }
  else if (show.axis.logits == "L") {
    axis(2, las = 1, cex.axis = 0.7, font.axis = 2)
    mtext(axis.logits, side = 2, line = 1.5, cex = 0.8, font = 3)
  }
  if (!is.null(cutpoints)) {
    cutLines(cutpoints, ...)
  }
  item.hist <- hist(thr, plot = FALSE, breaks = Nbins(thr, 
                                                      yRange))
  itemBinLocations <- item.hist$mids
  bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
  item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2), 
                          ybottom = item.hist$mids * 0, xright = item.hist$mids + 
                            (bin.size/2), ytop = item.hist$counts)
  
  #item.labels <- matrix(rep(formatC(1:nI, digits = 1, format = "d", 
  #                                  flag = "0"), nL), ncol = nL)
  #item.labels <- t(apply(item.labels, 1, paste, c(1:nL), sep = "."))
  
  #LC edit.
  # removing a perfect/zero item ruined item numbering
  # use the item names in thr instead 
  # also, drop the ".1" added to the end because AEAS items are always dichot. 
  item.labels<-matrix(row.names(thr))
  item.labels <- t(apply(item.labels,1, paste, "", sep = ""))
  
  binnedItems <- matrix(cut(thr, breaks = Nbins(thr, yRange), 
                            labels = c(1:length(item.hist[, 1] + 1))), ncol = nL)
  binnedList <- unlist(lapply(1:length(itemBinLocations), binItems, 
                              item.labels, binnedItems))
  text(cbind(0, itemBinLocations), labels = binnedList, pos = 4, 
       offset = 1 * 15/nI, cex = 0.65)
}

####################################

item.side.LC<- function (thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", 
                           show.axis.logits = "R", oma = c(0, 0, 0, 5), cutpoints = NULL, 
                           ...) 
{

  Nbins <- function(thr, itemRange) {
    breaks <- seq(from = itemRange[1], to = itemRange[2], 
                  length.out = 25)
    if (min(thr, na.rm = TRUE) < min(breaks)) 
      breaks <- c(min(thr, na.rm = TRUE), breaks)
    if (max(thr, na.rm = TRUE) > max(breaks)) 
      breaks <- c(breaks, max(thr, na.rm = TRUE))
    return(breaks)
  }
  thr <- as.matrix(thr)
  nI <- dim(thr)[1]
  nL <- dim(thr)[2]
  if (is.null(yRange)) {
    yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))
    yA <- (yRange[2] - yRange[1]) * 0.1
    yRange <- yRange + c(-yA, yA)
  }

  item.hist <- hist(thr, plot = FALSE, breaks = Nbins(thr, 
                                                      yRange))
  itemBinLocations <- item.hist$mids
  bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
  item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2), 
                          ybottom = item.hist$mids * 0, xright = item.hist$mids + 
                            (bin.size/2), ytop = item.hist$counts)
  
  binnedItems <- matrix(cut(thr, breaks = Nbins(thr, yRange), 
                            labels = c(1:length(item.hist[, 1] + 1))), ncol = nL)

  thrWithBins <- cbind(thr,binnedItems)
  thrWithBins[,2]
  
  bin_xleft<- unlist(unname(lapply(thrWithBins[,2], function(x){
    item.hist[x,]$xleft
  })))
  bin_xright<- unlist(unname(lapply(thrWithBins[,2], function(x){
    item.hist[x,]$xright
  })))
  bin_xright
  
  thrWithBins <- cbind(thrWithBins,bin_xleft,bin_xright)
  
  thrWithBins <- data.frame(thrWithBins)
  thrWithBins$item<-row.names(thrWithBins)
  thrWithBins$ypoint<-bin_xleft
  thrWithBins %<>% 
    group_by(V2) %>%
    arrange(.$V2)
  
  thrWithBins %<>% 
    group_by(V2) %>%
    arrange(.$item) %>% 
    mutate(xpoint = 1:n())
  thrWithBins
  
  thrWithBins
  #ggplot(thrWithBins, aes(xpoint, ypoint, label=item)) + 
  #  geom_point()+
  #  geom_text(aes(label=item),hjust=0, vjust=0) +
  #  labs(x = "", y= "") + 
  #  theme(
  #    axis.text.x = element_blank(),
  #    axis.text.y = element_blank(),
  #    axis.ticks = element_blank(),
  #    panel.background = element_rect(fill="white",colour = "black"),
  #    panel.grid.major = element_blank(),
  #    panel.grid.minor = element_blank())
}

