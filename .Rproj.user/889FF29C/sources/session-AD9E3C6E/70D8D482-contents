#Cài đặt thư viện
#install.packages("shinydashboard")
#install.packages("shiny")
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)


#Load data

data <- read.csv('clean.csv', encoding = "UTF-8")
model <-
  read.csv('model.csv',
           encoding = "UTF-8",
           stringsAsFactors = F)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #00A65A;
}"'

#Functions
format.money  <- function(x, c = 1, ...) {
  if (c == 1) {
    paste0("$",
           formatC(
             as.numeric(x),
             format = "f",
             digits = 2,
             big.mark = ","
           ))
  }
  else{
    formatC(
      as.numeric(x),
      format = "f",
      digits = 0,
      big.mark = ","
    )
  }
}


#UI
ui <-
  dashboardPage(
    skin = "green",
    dashboardHeader(title = "Google Store"),
    dashboardSidebar(sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard", lib = "glyphicon")
      ),
      menuItem(
        "Thống kê",
        tabName = "stats",
        icon = icon("stats", lib = "glyphicon"),
        menuSubItem("Tổng quan", tabName = "subitem0"),
        menuSubItem("Biểu đồ hộp", tabName = "subitem1"),
        menuSubItem("Biểu đồ histogram", tabName = "subitem2")
      ),
      menuItem(
        "Mô hình",
        tabName = "model",
        icon = icon("blackboard", lib = "glyphicon")
      )
      ,
      menuItem(
        "Chi tiết",
        tabName = "info",
        icon = icon("star", lib = "glyphicon")
      )
    )),
    dashboardBody(
      tags$style(js),
      tags$script(HTML("$('body').addClass('fixed');")),
      tags$head(tags$style(
        HTML(
          '
      .main-header .logo {
        font-family: Arial, sans-serif;
        font-weight: bold;
        font-size: 24px;
      }
    '
        )
      )),
      tabItems(
        # First tab content
        tabItem(
          tabName = "dashboard",
          height = "500px" ,
          fluidRow(
            infoBox(
              "TỔNG ỨNG DỤNG",
              format.money(nrow(data), c = 0),
              width = 3,
              icon = icon("android"),
              color = "green"
            ),
            
            infoBox(
              "LƯỢT ĐÁNH GIÁ",
              format.money(sum(data$Reviews), c = 0),
              width = 3,
              icon = icon("star"),
              color = "yellow"
            ),
            
            infoBox(
              "LƯỢT TẢI VỀ",
              format.money(sum(data$Installs), c = 0),
              width = 3,
              icon = icon("download")
            ),
            
            infoBox(
              "TỔNG DOANH THU",
              format.money(sum(
                round(data$Price * data$Installs, digits = 2)
              )),
              width = 3,
              icon = icon("hand-holding-dollar"),
              color = "red"
            )
          ),
          fluidRow(
            box(
              img(src = 'exs.png' , width = "100%" ),
              width = 12
            )
          )
          
        ),
        tabItem(
          tabName = "subitem0",
          h3("Phân tích thị trường Android"),
          fluidRow(box(
            img(src = 's1.png' , width = "100%"),
            h4("Danh mục nào có tỷ lệ ứng dụng cao nhất trên thị trường?"),
            p("FAMILY, GAME và TOOLS có mức độ phổ biến cao nhất trên thị trường"),
            width = 6
          ),
          box(
            img(src = 's2.png' , width = "100%"),
            h4("Các ứng dụng hoạt động tốt hay tệ?"),
            p("Trung bình điểm đánh giá: ", round(mean(data$Rating), digits = 2)),
            width = 6
          )),
          h3("Danh mục tốt nhất"),
          fluidRow(box(img(
            src = 's3.png' , width = "100%"
          ),
          width = 12)),
          h3("Tiêu chuẩn về kích thước và định giá"),
          fluidRow(
            box(
              img(src = 's4.png' , width = "100%"),
              h4("Kích thước ứng dụng ảnh hưởng như thế nào đến đánh giá?"),
              p(
                "Hầu hết các ứng dụng được đánh giá hàng đầu đều có kích thước trong khoảng từ 0MB đến 25MB"
              ),
              br(),
              br(),
              width = 6
            ),
            box(
              img(src = 's5.png' , width = "100%"),
              h4("Phí ứng dụng ảnh hưởng đến đánh giá ứng dụng như thế nào?"),
              p("Hầu hết các ứng dụng được đánh giá hàng đầu đều có phí từ ~1$ đến ~20$"),
              p(" Chỉ có rất ít ứng dụng có phí trên 20$."),
              width = 6
            ),
          )
          ,
          h3("Khác biệt giữa có phí và trả phí"),
          fluidRow(box(img(
            src = 's6.png' , width = "100%"
          ),
          width = 6),
          box(
            h4("Ứng dụng trả phí có được tải xuống nhiều như ứng dụng miễn phí không?"),
            p(
              "Ứng dụng trả phí có số lượt tải xuống tương đối thấp hơn ứng dụng miễn phí"
            ),
            width = 6
          ),)
          ,
        ),
        tabItem(
          tabName = "subitem1",
          h2("Biểu đồ hộp"),
          selectInput(
            inputId = "VarBox",
            label = "Chọn mục:",
            choices = list("Rating", "Size",
                           "Price")
          ),
          plotOutput("plot1")
        ),
        tabItem(
          tabName = "subitem2",
          h2("Biểu đồ histogram"),
          selectInput(
            inputId = "VarBox2",
            label = "Chọn mục:",
            choices = list("Rating","Size")
          ),
          plotOutput("plot2")
        ),
        # Third tab content
        tabItem(
          tabName = "model",
          
          fluidRow(
            box(
              tags$style(
                HTML(
                  ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}"
                )
              ),
              sliderInput(
                "Slider1",
                label = h3("Train/Test Split %"),
                min = 0,
                max = nrow(model),
                value = 200,
                
              )
            ),
            box(
              selectInput(
                "SelectX",
                label = "Select variables:",
                choices = names(model),
                multiple = TRUE,
                selected = names(model)
              ),
              solidHeader = TRUE,
              width = "3",
              status = "success",
              title = "X variable"
            ),
            box(
              selectInput("SelectY", label = "Select variable to predict:", choices = names(model)),
              solidHeader = TRUE,
              width = "3",
              status = "success",
              title = "Y variable",
            )
          ),
          fluidRow(
            tabBox(
              id = "tabset1",
              height = "600px",
              width = 12,
              
              tabPanel("Data",
                       box(withSpinner(DTOutput(
                         "Data"
                       )), width = 12)),
              tabPanel(
                "Data Summary",
                box(withSpinner(verbatimTextOutput("Summ")), width = 6),
                box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
              ),
              tabPanel("Plots",
                       box(withSpinner(plotOutput(
                         "Corr"
                       )), width = 12)),
              
              
              tabPanel(
                "Model",
                box(
                  withSpinner(verbatimTextOutput("Model")),
                  width = 6,
                  title = "Model Summary"
                ),
                box(
                  withSpinner(verbatimTextOutput("ImpVar")),
                  width = 5,
                  title = "Variable Importance"
                )
              ),
              
              tabPanel(
                "Prediction",
                box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
                box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
              )
              
            )
          )
        ),
        tabItem(
          tabName = 'info',
          fluidRow(
            box(withSpinner(DTOutput(
              "DataFull"
            )), width = 12)
          )
          
        )
      ),
    )
  )

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    boxplot(
      model[, input$VarBox],
      main = paste("Biểu đồ hộp của", input$VarBox),
      col = "green",
      horizontal = T
    )
  })
  
  output$plot2 <- renderPlot({
    hist(
      model[, input$VarBox2],
      main = paste("Biểu đồ histogram của", input$VarBox2),
      col = "green",
      xlab = input$VarBox2
    )
  })
  
  
  InputDataset <- reactive({
    model
  })
  
  InputFullDataset <- reactive({
    data
  })
  
  output$Data <- renderDT(InputDataset())
  
  output$DataFull <- renderDT(InputFullDataset())
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      model
    }
    else{
      model[, c(input$SelectX)]
    }
    
  })
  
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / nrow(model)
  })
  
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  
  set.seed(nrow(model))  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(),]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(), ]
  })
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T), ]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  
  Fit <-
    reactive({
      plot(
        actuals_preds()$actuals,
        actuals_preds()$predicted,
        pch = 16,
        cex = 1.3,
        col = "blue",
        main = "Best Fit Line",
        xlab = "Actual",
        ylab = "Predicted",
        
      )
    })
  
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model())
    par(mfrow = c(1, 1))
    
  })
  
}

shinyApp(ui, server)