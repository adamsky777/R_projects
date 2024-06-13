
# Create a data frame with the values
data <- data.frame(
  Category = c("Isolated work days", "Isolated days off"),  Value = c(635, 137)
)

# Create the bar plot and store the bar midpoints
bar_midpoints <- barplot(data$Value, 
                         names.arg = data$Category, 
                         col = "gray", 
                         main = "Isolated workdays / Isolated days off",
                         ylim = c(0, max(data$Value) + 79))

# Add text above the bars
text(x = bar_midpoints, 
     y = data$Value + 10, 
     labels = data$Value, 
     pos = 3)







