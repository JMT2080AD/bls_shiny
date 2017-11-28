crp <- colorRampPalette(c("red",
                          "orange",
                          "green",
                          "blue"))



library(extrafont)
## font_import()
loadfonts()
ggplot(dataAll) +
    geom_line(aes(year, value, group = Industry, colour = Industry, linetype = Industry)) +
    theme_bw() +
    scale_color_manual(values=crp(length(unique(dataAll$Industry)))) + 
    scale_linetype_manual(values=rep(c("solid", "dotdash", "dashed"), 4)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
    labs(title = "Average Weekly Income",
         subtitle = "All Industry Groups Average Weekly Income San Fransisco-Oakland-Hayward",
         caption = "Source: Bureau of Labor Statistics",
         x = "Year",
         y = "Average Weekly Income in Dollars")

ggplot(dataCon) +
    geom_line(aes(year, value, group = Industry, colour = Industry, linetype = Industry)) +
    theme_bw() +
    scale_color_manual(values=crp(length(unique(dataCon$Industry)))) + 
    scale_linetype_manual(values=rep(c("solid", "dotdash", "dashed"), 4)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
    labs(title = "Average Weekly Income",
         subtitle = "Select Trades Average Weekly Income San Fransisco-Oakland-Hayward",
         caption = "Source: Bureau of Labor Statistics",
         x = "Year",
         y = "Average Weekly Income in Dollars")
