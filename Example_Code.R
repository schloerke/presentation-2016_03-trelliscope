library(trelliscope); library(datadr); library(housingData)
library(magrittr); library(dplyr); library(tidyr); library(ggplot2)

# divide housing data by county and state
divide(housing, by = c("county", "state")) %>%
  drFilter(function(x){nrow(x) > 10}) ->
  # drFilter(function(x){nrow(x) > 120}) ->
  byCounty

# calculate the min and max y range
byCounty %>%
  drLapply(function(x){
    range(x[,c("medListPriceSqft", "medSoldPriceSqft")], na.rm = TRUE)
  }) %>%
  as.list() %>%
  lapply("[[", 2) %>%
  unlist() %>%
  range() ->
  yRanges


# for every subset 'x', calculate this information
priceCog <- function(x) {
   zillowString <- gsub(" ", "-", do.call(paste, getSplitVars(x)))
   list(
      slopeList = cog(
        coef(lm(medListPriceSqft ~ time, data = x))[2],
        desc = "list price slope"
      ),
      meanList = cogMean(x$medListPriceSqft),
      meanSold = cogMean(x$medSoldPriceSqft),
      nObsList = cog(
        length(which(!is.na(x$medListPriceSqft))),
        desc = "number of non-NA list prices"
      ),
      zillowHref = cogHref(
        sprintf("http://www.zillow.com/homes/%s_rb/", zillowString),
        desc = "zillow link"
      )
   )
}


# for every subset 'x', generate this plot
latticePanel <- function(x) {
  x %>%
    select(time, medListPriceSqft, medSoldPriceSqft) %>%
    gather(key = "variable", value = "value", medListPriceSqft, medSoldPriceSqft, -time) %>%
    ggplot(aes(x = time, y = value, color = variable)) +
      geom_smooth() +
      geom_point() +
      ylim(yRanges) +
      labs(y = "Price / Sq. Ft.") +
      theme(legend.position = "bottom")
}

# make this display
makeDisplay(
  byCounty,
  group   = "fields",
  panelFn = latticePanel,
  cogFn   = priceCog,
  name    = "list_vs_time_ggplot",
  desc    = "List and sold priceover time w/ggplot2",
  conn    = vdbConn("vdb", autoYes = TRUE)
)

# make a second display
latticePanelLM <- function(x) {
  x %>%
    select(time, medListPriceSqft, medSoldPriceSqft) %>%
    gather(key = "variable", value = "value", medListPriceSqft, medSoldPriceSqft, -time) %>%
    ggplot(aes(x = time, y = value, color = variable)) +
      geom_smooth(method = "lm") +
      geom_point() +
      ylim(yRanges) +
      labs(y = "Price / Sq. Ft.") +
      theme(legend.position = "bottom")
}
makeDisplay(
  byCounty,
  group   = "fields",
  panelFn = latticePanelLM,
  cogFn   = priceCog,
  name    = "list_vs_time_ggplot_lm",
  desc    = "List and sold priceover time w/ggplot2 with lm line",
  conn    = vdbConn("vdb")
)


view()
