library(shiny)
library(shinydashboard)
#Packaages to be installed
install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
install.packages("ggplot2", dependencies = TRUE)
install.packages("Packages/janitor_1.2.0.zip",
                 repos = NULL,
                 type = "win.binary")

#libraries to be included
library(tidyr)
library(dplyr)
library(tidyverse)
# for using clean name function
library(janitor)
library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)

############################
# Import data into R---------
# the dataset is in the csv format which is being imported and stored in the variable House_Data
House_Data <-
  read.csv("/Users/likhita/Downloads/House_Rent_Dataset.csv", stringsAsFactors = TRUE)
dim(House_Data)
#Print the dataset
str(House_Data)
head(House_Data)
summary(House_Data)
#dimensions of data
dim(House_Data)
#############################


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      #menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Linear Regression", tabName = "subitem1"),
               menuSubItem("K-Nearest Neighbors", tabName = "subitem2"),
               menuSubItem("Random Forest", tabName = "subitem3")
      )
    ),
    textOutput("res")
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",box(
        title = "Check the Missing Data "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("missingdata", height = "300px")
      )
      ,box(
        title = "Check the Missing Data"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("missingdata1", height = "300px")
      ),box(
        title = "Density Plot "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyPrd", height = "300px")
      )
      ,box(
        title = "House as per area type (and point of contact)"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("gp1", height = "300px")
      ) 
      
      
      ,box(
        title = "Correlation Plot "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Corplot", height = "300px")
      )
      ,box(
        title = "Scatter Plot "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("ScatterPlot", height = "300px")
      )
      ,box(
        title = "Q-Q Plot"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("qqnorm", height = "300px")
      )
      ,box(
        title = "Pie Chart"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("pie", height = "300px")
      ) 
      ,box(
        title = "Histogram"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("house_histogram", height = "300px")
      )
      ,box(
        title = "Furnishing Status with Bathroom"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Furnish", height = "300px")
      ) 
      ,box(
        title = "Houses For Rent in cities"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("barplotcity", height = "300px")
      )
      ,box(
        title = "Houses For Rent depending on  size"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("barplotSize", height = "300px")
      )
      ,box(
        title = "City"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("City", height = "300px")
      )
      ,box(
        title = "Tenant Preferred"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Tenant_Preferred", height = "300px")
      )
      ,box(
        title = "Area Type"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Area_type", height = "300px")
      )
      ,box(
        title = "Area Locality"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Area_Locality", height = "300px")
      )
      ,box(
        title = "Correlation"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("BHK", height = "300px")
      )
      ,box(
        title = "GGPlot"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Bath", height = "300px")
      )
      ,box(
        title = "GGPlot"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("HeatMap", height = "300px")
      )
      
      ,box(
        title = "boxplot"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Boxplot", height = "300px")
      )
      ,box(
        title = "HeatMap1"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("HeatMap1", height = "300px")
      )
      
      
      ),
     # tabItem("widgets", "Widgets tab content"),
      tabItem("subitem1", box(
        title = "Linear Regression "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("Linear", height = "300px")
      )),
      tabItem("subitem2", box(
        title = "K-Nearest Neighbors "
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("knn", height = "300px")
      )),
     tabItem("subitem3", box(
       title = "Random Forest"
       ,status = "primary"
       ,solidHeader = TRUE 
       ,collapsible = TRUE 
       ,plotOutput("random", height = "300px")
     ))
    )
  )
)

server <- function(input, output, session) {
  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
  
  
  #1 
  output$missingdata <- renderPlot({
    missmap(
      House_Data,
      col = c('yellow', 'black'),
      y.at = 1,
      y.labels = '',
      legend = TRUE
    )
  }) 
  
  #2
  output$missingdata1 <- renderPlot({
    library(naniar)
    vis_miss(House_Data)
    gg_miss_var(House_Data) + theme_dark() 
  }) 
  
  #3
  output$revenuebyPrd <- renderPlot({
    ggplot(House_Data,                              
           aes(x = Rent,
               fill = City)) +
      geom_density(alpha = 0.5) +
      theme_dark()
  }) 
  #4 
  output$gp1 <- renderPlot({
    #House as per area type (and point of contact)
    gp1<- subset(House_Data, Area.Type=="Super Area", select = c(Rent,City,Point.of.Contact))
    gp1<-top_n(gp1,20)
    ggplot(gp1, aes(x=gp1$Rent, y=gp1$City, fill=gp1$Point.of.Contact))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="House as per area type",subtitle="(point of contact)",x="Rent",y="City",fill="Pont.of.Contact")
  })
  
  #5
  output$Corplot <- renderPlot({
    cor.test(House_Data$Rent,House_Data$BHK, method = "spearman",exact=FALSE)
    cor.test(House_Data$BHK,House_Data$Size, method = "spearman",exact=FALSE)
    cor.test(House_Data$Bathroom,House_Data$Rent, method = "spearman",exact=FALSE)
    cor.test(House_Data$Rent,House_Data$Size, method = "spearman",exact=FALSE)
    
    
    cor4 <- cor(House_Data[, c(2,3,4,11)], method = "spearman")
    corrplot(cor4, type = "upper", tl.col = "black", tl.srt = 45)
    
  })
  
  #6
  output$ScatterPlot <- renderPlot({
    head(House_Data, 6)
    str(House_Data)
    library("ggpubr")
    g <- ggscatter(
      House_Data,
      x = "City",
      y = "Rent",
      add = "reg.line",
      conf.int = TRUE,
      cor.coef = TRUE,
      cor.method = "pearson",
      xlab = "City",
      ylab = "Rent"
    )
    require(scales)
    g + scale_y_continuous(
      labels = function(x)
        format(x, scientific = FALSE)
    )
  })
  
  #7
  output$qqnorm <- renderPlot({
    qqnorm(House_Data$Rent, pch = 1, frame = FALSE, main="Rent")
    qqline(House_Data$Rent, col = "steelblue", lwd = 2)
  })
  
  #8
  output$pie <- renderPlot({
    library(plotrix)
    slices <- c(2,3,4,5,10,6,7,8,9)
    lbls <- c('Number_of_Bedroom','Rent','Size_in_Sqft','Floor',' Tenant_Preferred','Area','Area_Locality','City','Furnishing_Status')
    #pie3D.labels(radialpos=0.1,radius=1,height=0.1,theta=pi/9,
    #lbls,labelcol=par("fg"),labelcex=1.5,labelrad=1.25,minsep=0.3)
    pct <- round(slices/sum(slices)*100)
    #lp<-pie3D(slices,radius=0.9,labels=lbls,explode=0.1,main="3D PIE OPINIONS")
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep=" ") # ad % to labels
    pie3D(slices,labels = lbls,
          main="Pie Chart of House Rent")
    #pie(slices,labels = rep("",5), col=rainbow(length(lbls)), radius=.8,lty=4)
    #text(0.9,0.6,"UK")
    #lines(c(0.6,0.85),c(0.45,0.55)})
  })
  
  #9
  output$house_histogram <- renderPlot({
    house_histogram <- c()
    dim(House_Data)
    summary(House_Data)
    for (b in 1:2000) {
      samp_b <- sample.int(nrow(House_Data), replace = TRUE)
      muhat_b <- mean(House_Data$Size[samp_b])
      house_histogram <- c(house_histogram, muhat_b)
    }
    
    hist(house_histogram,
         main = "",
         xlab = "Size",
         ylab = "Rent")
    str(House_Data$Size)
  })
  
  #10
  output$Furnish <- renderPlot({
    #furnishing status with bathroom
    m_plot <- ggplot(House_Data, aes(Furnishing.Status))
    m_plot + geom_bar(aes(fill=Bathroom), width = 0.5) +
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      labs(title="Furnishing Status with bathroom") + scale_fill_manual(values=c('plum2','navy'))
  })
  
  #11
  output$barplotcity <- renderPlot({
    #Bar Plot for Number of House in Each City which is Available for Rent
    barplot(height = House_Data$Rent, names = House_Data$City)
    library(ggplot2)
    ggplot(House_Data, aes(x = factor(Rent))) +
      geom_bar(fill = "coral") +
      theme_classic()
  })
  
  #12
  output$barplotSize <- renderPlot({
    #Bar Plot for Number of House dpending on size which is Available for Rent
    barplot(height = House_Data$Rent, names = House_Data$Size)
    library(ggplot2)
    ggplot(House_Data, aes(x = factor(Rent))) +
      geom_bar(fill = "coral") +
      theme_classic()
  })
  
  #13
  output$City <- renderPlot({
    # Bar Plot for Number of House in Each City which is Available for Rent ----
    ggplot(House_Data) +
      geom_bar(aes(x = City, color = "black"))
  })
  
  #14
  output$Tenant_Preferred <- renderPlot({
    #Bar Plot on Different Types of Tenant Preferred ----
    ggplot(House_Data) +
      geom_bar(aes(x = Tenant.Preferred, color = "orange"))
  })
  
  #15
  output$Area_type <- renderPlot({
    #Bar Plot on Different Types of Tenant Preferred ----
    ggplot(House_Data) +
      geom_bar(aes(x = Area.Type, color = "orange"))
  }) 
  
  #16
  output$Area_Locality <- renderPlot({
    #Bar Plot on Different Types of Tenant Preferred ----
    ggplot(House_Data) +
      geom_bar(aes(x = Area.Locality, color = "orange"))
  })
  
  #17
  output$BHK <- renderPlot({
    #Distribution of different number of BHK available in the Dataset ----
    correlation_df<-cor(House_Data[,2:4,12],
                        House_Data[,2:4,12], method="kendall", use="pairwise.complete.obs")
    
    
    # graph correlation specific columns
    
    corrplot(correlation_df,
             method="color", addCoef.col = "black")
  }) 
  
  #18
  output$Bath <- renderPlot({
    #Distribution of different number of Bathrooms available in the Dataset ----
    ggplot(House_Data) +
      geom_bar(aes(x =Bathroom, color = "orange"))
  }) 
  
  #19
  output$HeatMap <- renderPlot({
    #Heatmap on Heatmap on BHK vs Area Type ----
    ggplot(House_Data, aes(BHK,Area.Type)) +
      geom_raster(aes(fill = Area.Type))
    ############################
  }) 
  
  #20
  output$Boxplot <- renderPlot({
    #boxplot
    boxplot(House_Data$Furnishing.Status, main = "Effect of Furnishing Status on Rent",
            xlab = "Rent", ylab = "Furnishing.Status",
            col = "orange", border = "brown",
            horizontal = TRUE, notch = TRUE)
  })
  output$HeatMap1 <- renderPlot({
    plot(House_Data$Area.Locality , House_Data$Rent,
         main ="Scatterplot ",
         xlab ="Area locality",
         ylab =" Rent ", pch = 19)
    
  }) 
  
  output$Linear <- renderPlot({
    split <- sample.split(House_Data, SplitRatio = 0.80)
    train <- subset(House_Data, split == TRUE)
    dim(train)
    test <- subset(House_Data, split == FALSE)
    dim(test)
    #Training to the model
    model <-
      lm(
        Rent ~ BHK + Size  + City + Furnishing.Status + Tenant.Preferred + Bathroom + Point.of.Contact,
        data = train
      )
    summary(model)
    res <- residuals(model)
    res <- as.data.frame(res)
    head(res)
    # Histogram plot for the model -----
    ggplot(res, aes(res)) +  geom_histogram(fill = 'red', alpha = 0.6)
    plot(model)
    test$predicted_rent <- predict(model, test)
    head(test)
    plot1 <- test %>%
      ggplot(aes(Rent, predicted_rent)) +
      geom_point(alpha = 0.5) +
      stat_smooth(aes(colour = 'black')) +
      xlab('Actual value of Rent ') +
      ylab('Predicted value of Rent') +
      theme_bw()
    ggplotly(plot1)
    #Assesing the model -------
    regressionMetrics <- function(real, predicted) {
      real = test$Rent
      predicted = test$predicted_rent
      # Mean Square Error
      MSE <- mean((real - predicted) ^ 2)
      # Root Mean Square Error
      RMSE <- sqrt(MSE)
      # Mean Absolute Error
      MAE <- mean(abs(real - predicted))
      # Median Absolute Error
      MedAE <- median(abs(real - predicted))
      # Mean Logarithmic Absolute Error
      MSLE <- mean((log(1 + real) - log(1 + predicted)) ^ 2)
      # Total Sum of Squares
      TSS <- sum((real - mean(real)) ^ 2)
      # Explained Sum of Squares
      RSS <- sum((predicted - real) ^ 2)
      # R2
      R2 <- 1 - RSS / TSS
      result <- data.frame(MSE, RMSE, MAE, MedAE, MSLE, R2)
      return(result)
    }
    print(regressionMetrics())
    
  }) 
  
  
  
  
  output$knn <- renderPlot({
    
    train.control <- trainControl(method = "cv", number = 5)
    different_k <- data.frame(k = seq(1, 99, 4))
    set.seed(987654321)
    knnmodellog <-
      train(
        log(Rent + 1) ~ .,
        data = House_Data %>%
          dplyr::select(-BHK,-Size,-Bathroom),
        method = "knn",
        trControl = train.control,
        tuneGrid = different_k
      )
    knnmodellog
    plot(knnmodellog)
    
  }) 
  
  
  output$random <- renderPlot({
    
    
    #Random Forest Model
    train.control <- trainControl(method = "cv", number = 5)
    set.seed(987654321)
    RFmodel <- train(
      log(Rent + 1) ~ .,
      # tuneLength = tuneGrid,
      data = House_Data %>%
        dplyr::select(-BHK,-Size,-Bathroom),
      method = "ranger",
      trControl = train.control
    )
    RFmodel
    plot(RFmodel)
    
  }) 

  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)
