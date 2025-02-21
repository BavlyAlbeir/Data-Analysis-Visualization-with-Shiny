library(shiny)
library(ggplot2)
library(dplyr)
library(arules)
library(arulesViz)

# start ui 
ui <- fluidPage(
  titlePanel("Data Cleaning, Visualization, K-means, and Association Rule Mining"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("clusters", "Number of Clusters (K-means)", choices = c(2, 3, 4), selected = 3),
      numericInput("supp", "Minimum Support (Association Rule)", value = 0.005, min = 0.001, max = 1, step = 0.01),
      numericInput("conf", "Minimum Confidence (Association Rule)", value = 0.005, min = 0.001, max = 1, step = 0.01),
      actionButton("run", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Uploaded Data", tableOutput("fullData")),
        tabPanel("Visualizations", 
                 plotOutput("barPlotPayment"), 
                 plotOutput("pieChartPayment"),
                 plotOutput("barPlotAge"),
                 plotOutput("pieChartAge"),
                 plotOutput("barPlotCity"),
                 plotOutput("pieChartCity"),
                 plotOutput("distributionPlot")),
        tabPanel("K-means Clustering", 
                 tableOutput("clusterOutput"), 
                 plotOutput("kmeansPlot")),
        tabPanel("Association Rules", 
                 tableOutput("rulesTable"), 
                 plotOutput("itemFrequencyPlot"),
                 plotOutput("rulesPlot"))
      )
    )
  )
)

# start server
server <- function(input, output, session) {
  # Read and clean data from user
  cleaned_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    # start of Cleaning data
    # Remove duplicates
    df <- unique(df)  
    # Remove missing values
    df <- na.omit(df) 
    df$total <- as.numeric(df$total)
    df$age <- as.integer(df$age)
    df$count <- as.integer(df$count)
    # Remove outliers
    outlier_count <- boxplot.stats(df$count)$out
    df <- df[!df$count %in% outlier_count, ]
    df$paymentType <- tolower(df$paymentType)
    df$city <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", df$city, perl = TRUE)
    # Add row numbers to the left
    df <- cbind(row_number = seq_len(nrow(df)), df) 
    return(df)
  })
  
  # check the min of supp and conf
  observe({
    if (input$supp < 0.001) {
      updateNumericInput(session, "supp", value = 0.001)
    }
    if (input$conf < 0.001) {
      updateNumericInput(session, "conf", value = 0.001)
    }
  })
  
  # print data
  output$fullData <- renderTable({
    req(cleaned_data())
    cleaned_data()
  })
  
  # Bar plot for cash and credit totals
  output$barPlotPayment <- renderPlot({ 
    data <- cleaned_data()
    payment_summary <- aggregate(total ~ paymentType, data = data, sum)
    bar_positions <- barplot(
      height = payment_summary$total,
      names.arg = payment_summary$paymentType,
      col = c("#FF5733", "#3498DB"),
      main = "Total Spending by Payment Type",
      xlab = "Payment Type",
      ylab = "Total Spending",
      ylim = c(0, max(payment_summary$total) * 1.1),
      las = 1
    )
    text(
      x = bar_positions,
      y = payment_summary$total,
      labels = format(payment_summary$total, big.mark = ",", scientific = FALSE),
      pos = 3, cex = 0.8, col = "black"
    )
  })
  # Pie chart for cash and credit totals
  output$pieChartPayment <- renderPlot({
    data <- cleaned_data()
    payment_summary <- aggregate(total ~ paymentType, data = data, sum)
    piepercent <- paste0(round(100 * payment_summary$total / sum(payment_summary$total), 1), "%")
    pie(
      x = payment_summary$total,
      labels = piepercent,
      main = "Total Spending by Payment Type",
      col = c("#FF5733", "#3498DB")
    )
    legend("topright", legend = payment_summary$paymentType, fill = c("#FF5733", "#3498DB"))
  })
  # Bar plot for every age and total spending
  output$barPlotAge <- renderPlot({
    data <- cleaned_data()
    age_summary <- aggregate(total ~ age, data = data, sum)
    bar_positions <- barplot(
      height = age_summary$total,
      names.arg = age_summary$age,
      col = rainbow(length(age_summary$age)),
      main = "Total Spending by Age",
      xlab = "Age",
      ylab = "Total Spending",
      ylim = c(0, max(age_summary$total) * 1.1),
      las = 1
    )
    text(
      x = bar_positions,
      y = age_summary$total,
      labels = format(age_summary$total, big.mark = ",", scientific = FALSE),
      pos = 3, cex = 0.8, col = "black"
    )
  })
  # Pie chart for every age and total spending
  output$pieChartAge <- renderPlot({
    data <- cleaned_data()
    age_summary <- aggregate(total ~ age, data = data, sum)
    piepercent <- paste0(round(100 * age_summary$total / sum(age_summary$total), 1), "%")
    pie(
      x = age_summary$total,
      labels = piepercent,
      main = "Total Spending by Age",
      col = rainbow(length(age_summary$age))
    )
    legend("topright", legend = age_summary$age, fill = rainbow(length(age_summary$age)))
  })
  # Bar plot for every city and total spending
  output$barPlotCity <- renderPlot({
    data <- cleaned_data()
    city_summary <- aggregate(total ~ city, data = data, sum)
    city_summary <- city_summary[order(city_summary$total, decreasing = TRUE), ]
    bar_positions <- barplot(
      height = city_summary$total,
      names.arg = city_summary$city,
      col = rainbow(length(city_summary$city)),
      main = "Total Spending by City",
      xlab = "City",
      ylab = "Total Spending",
      las = 2,
      ylim = c(0, max(city_summary$total) * 1.1)
    )
    text(
      x = bar_positions,
      y = city_summary$total,
      labels = format(city_summary$total, big.mark = ",", scientific = FALSE),
      pos = 3, cex = 0.8, col = "black"
    )
  })
  # Pie chart for every city and total spending
  output$pieChartCity <- renderPlot({
    data <- cleaned_data()
    city_summary <- aggregate(total ~ city, data = data, sum)
    city_summary <- city_summary[order(city_summary$total, decreasing = TRUE), ]
    piepercent <- paste0(round(100 * city_summary$total / sum(city_summary$total), 1), "%")
    pie(
      x = city_summary$total,
      labels = piepercent,
      main = "Total Spending by City",
      col = rainbow(length(city_summary$city))
    )
    legend("topright", legend = city_summary$city, fill = rainbow(length(city_summary$city)))
  })
  # Distribution of total spending
  output$distributionPlot <- renderPlot({
    data <- cleaned_data()
    boxplot(data$total, main = "Distribution of Total Spending", ylab = "Total Spending")
  })
  #k-mean
  output$clusterOutput <- renderTable({
    req(input$run)
    data <- cleaned_data() %>%
      group_by(customer, age) %>%
      summarise(totalSpending = sum(total))
    kmeans_result <- kmeans(data[, -1], centers = input$clusters)
    data$Cluster <- kmeans_result$cluster
    return(data)
  })
  # Association Rule table
  output$rulesTable <- renderTable({
    req(input$run)
    data <- cleaned_data()
    transactions <- as(split(data$items, seq(nrow(data))), "transactions")
    rules <- apriori(transactions, parameter = list(supp = input$supp, conf = input$conf))
    as(head(sort(rules, by = "confidence"), n = 10), "data.frame")
  })
  #Association Rule visualization
  output$itemFrequencyPlot <- renderPlot({
    req(input$run)
    data <- cleaned_data()
    transactions <- as(split(data$items, seq(nrow(data))), "transactions")
    itemFrequencyPlot(transactions, topN = 10, type = "absolute")
  })
  
}

shinyApp(ui = ui, server = server)