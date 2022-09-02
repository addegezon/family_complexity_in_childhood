library(readr)
library(dplyr)
library(plotly)

drops <- read_csv("Documents/skola/master/work/analysis/05_tables/drops.csv")
drops_kids <- read_csv("Documents/skola/master/work/analysis/05_tables/drops_children.csv")
drops_kids <- drops_kids[-c(1),] 

# Plot women funnel
women.fig <- plot_ly() 

women.fig <- women.fig %>%
  
  add_trace(type = "funnel",
            
            y = drops$Stage,
            x = drops$Total,
            textposition = "inside",
            texttemplate = "%{value:,.0f}",
            opacity = 0.65) 

women.fig <- women.fig %>%
  
  layout(title = 'Data funnel for adult sample', yaxis = list(categoryarray = drops$Stage))
women.fig

# Plot kids funnel
kids.fig <- plot_ly() 

kids.fig <- kids.fig %>%
  
  add_trace(type = "funnel",
            
            y = drops_kids$Stage,
            x = drops_kids$Total,
            textposition = "inside",
            texttemplate = "%{value:,.0f}",
            opacity = 0.65) 

kids.fig <- kids.fig %>%
  
  layout(title = 'Data funnel for child sample', yaxis = list(categoryarray = drops_kids$Stage))
kids.fig