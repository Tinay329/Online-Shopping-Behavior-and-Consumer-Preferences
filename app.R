
# ==========================================
# ONLINE SHOPPING BEHAVIOR AND
# CONSUMER PREFERENCES
# INTERACTIVE SHINY DASHBOARD
# ==========================================

# Install required packages if missing
packages <- c("shiny", "shinydashboard",
              "ggplot2", "dplyr", "DT")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Load dataset
shopping_data <- read.csv(
  "online_shopping_data.csv",
  stringsAsFactors = FALSE
)

# Convert month into calendar order
shopping_data$PurchaseMonth <- factor(
  shopping_data$PurchaseMonth,
  levels = month.name
)

# ==========================================
# USER INTERFACE
# ==========================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Online Shopping Analytics"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")),
      
      menuItem("Sales Analysis",
               tabName = "sales",
               icon = icon("chart-bar")),
      
      menuItem("Customer Analysis",
               tabName = "customers",
               icon = icon("users")),
      
      menuItem("Shopping Preferences",
               tabName = "preferences",
               icon = icon("shopping-cart")),
      
      menuItem("Customer Records",
               tabName = "records",
               icon = icon("table"))
    ),
    
    hr(),
    
    selectInput(
      "category",
      "Select Product Category:",
      choices = c("All",
                  unique(shopping_data$ProductCategory))
    ),
    
    selectInput(
      "platform",
      "Select Shopping Platform:",
      choices = c("All",
                  unique(shopping_data$ShoppingPlatform))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f2f6fc;
        }

        .main-header .logo {
          background-color: #123052 !important;
          color: white !important;
          font-weight: bold;
        }

        .main-header .navbar {
          background-color: #1f68aa !important;
        }

        .main-sidebar {
          background-color: #123052;
        }

        .box {
          border-radius: 10px;
        }

        .small-box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      
      # ==================================
      # DASHBOARD
      # ==================================
      
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("customers"),
          valueBoxOutput("sales"),
          valueBoxOutput("average"),
          valueBoxOutput("rating")
        ),
        
        fluidRow(
          box(
            title = "Sales by Product Category",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("categoryChart")
          ),
          
          box(
            title = "Monthly Shopping Trends",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("monthlyChart")
          )
        ),
        
        fluidRow(
          box(
            title = "Payment Method Preferences",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("paymentChart")
          ),
          
          box(
            title = "Shopping Platform Usage",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("platformChart")
          )
        )
      ),
      
      # ==================================
      # SALES ANALYSIS
      # ==================================
      
      tabItem(
        tabName = "sales",
        
        fluidRow(
          box(
            title = "Product Category Sales",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("salesChart", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Monthly Sales Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("monthlyTable")
          )
        )
      ),
      
      # ==================================
      # CUSTOMER ANALYSIS
      # ==================================
      
      tabItem(
        tabName = "customers",
        
        fluidRow(
          box(
            title = "Customer Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("ageChart")
          ),
          
          box(
            title = "Customer Satisfaction Ratings",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("ratingChart")
          )
        )
      ),
      
      # ==================================
      # SHOPPING PREFERENCES
      # ==================================
      
      tabItem(
        tabName = "preferences",
        
        fluidRow(
          box(
            title = "Preferred Payment Methods",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("paymentPreference")
          ),
          
          box(
            title = "Product Category Preferences",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("categoryPreference")
          )
        )
      ),
      
      # ==================================
      # CUSTOMER RECORDS
      # ==================================
      
      tabItem(
        tabName = "records",
        
        fluidRow(
          box(
            title = "Online Shopping Customer Records",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            downloadButton(
              "downloadData",
              "Download CSV"
            ),
            
            br(), br(),
            
            DTOutput("customerTable")
          )
        )
      )
    )
  )
)

# ==========================================
# SERVER
# ==========================================

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- shopping_data
    
    if (input$category != "All") {
      data <- data %>%
        filter(ProductCategory == input$category)
    }
    
    if (input$platform != "All") {
      data <- data %>%
        filter(ShoppingPlatform == input$platform)
    }
    
    data
  })
  
  # KPI CARDS
  
  output$customers <- renderValueBox({
    valueBox(
      n_distinct(filtered_data()$CustomerID),
      "Total Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$sales <- renderValueBox({
    valueBox(
      paste0("₱", format(
        round(sum(filtered_data()$PurchaseAmount), 2),
        big.mark = ","
      )),
      "Total Sales",
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$average <- renderValueBox({
    valueBox(
      paste0("₱", round(
        mean(filtered_data()$PurchaseAmount), 2
      )),
      "Average Purchase",
      icon = icon("shopping-cart"),
      color = "aqua"
    )
  })
  
  output$rating <- renderValueBox({
    valueBox(
      if(nrow(filtered_data()) == 0) {
        "N/A"
      } else {
        paste0(round(
          mean(filtered_data()$CustomerRating), 2
        ), "/5")
      },
      "Average Rating",
      icon = icon("star"),
      color = "purple"
    )
  })
  
  # CATEGORY CHART
  
  output$categoryChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(ProductCategory) %>%
      summarise(TotalSales = sum(PurchaseAmount))
    
    ggplot(data, aes(
      x = reorder(ProductCategory, TotalSales),
      y = TotalSales
    )) +
      geom_col(fill = "#1f68aa") +
      coord_flip() +
      labs(x = "Product Category",
           y = "Total Sales") +
      theme_minimal()
  })
  
  # MONTHLY CHART
  
  output$monthlyChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(PurchaseMonth) %>%
      summarise(Transactions = n())
    
    ggplot(data, aes(
      x = PurchaseMonth,
      y = Transactions,
      group = 1
    )) +
      geom_line(color = "#159d99", linewidth = 1.2) +
      geom_point(color = "#1f68aa", size = 3) +
      labs(x = "Month",
           y = "Transactions") +
      theme_minimal() +
      theme(axis.text.x = element_text(
        angle = 45, hjust = 1
      ))
  })
  
  # PAYMENT CHART
  
  output$paymentChart <- renderPlot({
    
    data <- filtered_data() %>%
      count(PaymentMethod)
    
    ggplot(data, aes(
      x = "",
      y = n,
      fill = PaymentMethod
    )) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void()
  })
  
  # PLATFORM CHART
  
  output$platformChart <- renderPlot({
    
    data <- filtered_data() %>%
      count(ShoppingPlatform)
    
    ggplot(data, aes(
      x = ShoppingPlatform,
      y = n,
      fill = ShoppingPlatform
    )) +
      geom_col() +
      labs(x = "Platform",
           y = "Transactions") +
      theme_minimal()
  })
  
  # SALES ANALYSIS
  
  output$salesChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(ProductCategory) %>%
      summarise(TotalSales = sum(PurchaseAmount))
    
    ggplot(data, aes(
      x = reorder(ProductCategory, TotalSales),
      y = TotalSales,
      fill = ProductCategory
    )) +
      geom_col() +
      coord_flip() +
      labs(x = "Category",
           y = "Sales Amount") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$monthlyTable <- renderDT({
    
    data <- filtered_data() %>%
      group_by(PurchaseMonth) %>%
      summarise(
        Transactions = n(),
        TotalSales = round(sum(PurchaseAmount), 2)
      )
    
    datatable(data, options = list(pageLength = 12))
  })
  
  # CUSTOMER AGE CHART
  
  output$ageChart <- renderPlot({
    
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(
        binwidth = 5,
        fill = "#1f68aa",
        color = "white"
      ) +
      labs(x = "Customer Age",
           y = "Number of Customers") +
      theme_minimal()
  })
  
  # SATISFACTION CHART
  
  output$ratingChart <- renderPlot({
    
    data <- filtered_data() %>%
      count(CustomerRating)
    
    ggplot(data, aes(
      x = factor(CustomerRating),
      y = n
    )) +
      geom_col(fill = "#159d99") +
      labs(x = "Rating",
           y = "Number of Customers") +
      theme_minimal()
  })
  
  # PAYMENT PREFERENCE
  
  output$paymentPreference <- renderPlot({
    
    data <- filtered_data() %>%
      count(PaymentMethod)
    
    ggplot(data, aes(
      x = reorder(PaymentMethod, n),
      y = n
    )) +
      geom_col(fill = "#159d99") +
      coord_flip() +
      labs(x = "Payment Method",
           y = "Transactions") +
      theme_minimal()
  })
  
  # CATEGORY PREFERENCE
  
  output$categoryPreference <- renderPlot({
    
    data <- filtered_data() %>%
      count(ProductCategory)
    
    ggplot(data, aes(
      x = reorder(ProductCategory, n),
      y = n
    )) +
      geom_col(fill = "#1f68aa") +
      coord_flip() +
      labs(x = "Category",
           y = "Transactions") +
      theme_minimal()
  })
  
  # CUSTOMER TABLE
  
  output$customerTable <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
  })
  
  # DOWNLOAD CSV
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      "online_shopping_records.csv"
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# ==========================================
# RUN APPLICATION
# ==========================================

shinyApp(ui = ui, server = server)
