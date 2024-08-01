# Load necessary library
library(plotly)

# Function to create a half gauge chart
create_half_gauge <- function(value, max_value = 100, title = "Half Gauge Chart") {
  # Calculate percentage and angle
  percentage <- value / max_value
  angle <- 180 * percentage
  
  # Define the gauge data
  gauge_data <- data.frame(
    type = c('Used', 'Remaining'),
    value = c(percentage, 1 - percentage)
  )
  
  # Create the half gauge plot using plotly
  p <- plot_ly(
    gauge_data, 
    labels = ~type, 
    values = ~value, 
    type = 'pie',
    hole = 0.4,  # Size of the hole in the center
    rotation = 90,  # Rotate the pie chart to start from the top
    textinfo = 'none',
    showlegend = FALSE
  ) %>%
    layout(
      title = list(text = title, x = 0.5, xanchor = 'center'),
      shapes = list(
        list(
          type = 'path',
          path = paste0('M 0 0 L 1 0 A 1 1 0 ', ifelse(angle > 180, 1, 0), ' 1 0 1 L 0 0 Z'),
          fillcolor = 'red',
          line = list(color = 'red', width = 2),
          xref = 'paper', 
          yref = 'paper',
          x0 = 0.5, 
          y0 = 0.5, 
          x1 = 1.5, 
          y1 = 1.5
        )
      ),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      showlegend = FALSE
    )
  
  # Return the plot
  return(p)
}

# Example usage
gauge_plot <- create_half_gauge(value = 75, max_value = 100, title = "Half Gauge Example")

# Display the half gauge chart
gauge_plot
