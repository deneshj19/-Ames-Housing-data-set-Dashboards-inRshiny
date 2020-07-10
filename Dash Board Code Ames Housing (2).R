install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
library(shiny)
library("ggplot2")
library("dplyr")



Ames_house_prices <- read.csv(file.choose(),header = T)
head(Ames_house_prices)
str(Ames_house_prices)
max(table(Ames_house_prices$SalePrice))

x <- unique(Ames_house_prices$Neighborhood,comparables=FALSE)
print(x)

ui <- fluidPage(
  headerPanel("Ames House prices"),
  sidebarLayout(
    sidebarPanel(
     selectInput("Neighborhood",label="Neighborhood",choices = c("CollgCr", "Veenker", "Crawfor", "NoRidge", "Mitchel", "Somerst", "NWAmes"  ,"OldTown" ,"BrkSide","Sawyer",  "NridgHt", "NAmes","SawyerW", "IDOTRR", 
                              "MeadowV", "Edwards", "Timber",  "Gilbert", "StoneBr", "ClearCr", "NPkVill", "Blmngtn", "BrDale",  "SWISU"   ,"Blueste")
      )
     ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("sales price distribution",plotOutput("Price")),
        tabPanel("Sales price vs Age",plotOutput("age")),
        tabPanel("Sales price in neighbourhood",plotOutput("cool")),
        tabPanel("Sales price vs Type of Road( Normal /Artery)",plotOutput("box"))
          
          )
      
         )
  )
  )
server <- function(input, output) {
  # Histogram for sales distribution
  output$Price <- renderPlot({
    ggplot(Ames_house_prices,aes(x=SalePrice))+geom_histogram(breaks=seq(0,1000000,by=50000),color="black",fill="lightblue",binwidth = 10000)+
    labs(title = "Sales price distribution in Ames ",x="Sales price in $",y="Count")
  })
   # Regression between Sales price and Age of house
  output$age <- renderPlot({
    b <- Ames_house_prices$YearBuilt
    r <- Ames_house_prices$YearRemodAdd
    s <- Ames_house_prices$YrSold
    age <- ifelse(r>b,(s-r),(s-b))
    age
    ggplot(Ames_house_prices,aes(x=age,y=SalePrice))+geom_point()+geom_smooth(method = lm)
    
    
  })
  # Sales price distribution in each neighborhood
  output$cool <- renderPlot({
    filtered <- Ames_house_prices %>%
      filter(Neighborhood==input$Neighborhood)
    ggplot(filtered,aes(x=SalePrice))+geom_histogram(breaks=seq(0,1000000,by=25000),color="black",fill="deeppink3",binwidth = 10000)+
      labs(title = "Sales price distribution in Neighborhood ",x="Sales price in $",y="Count")
    
    })
  # Box plot for sales price for normal and artery road 
  output$box <- renderPlot({
     artery <- subset(Ames_house_prices,Ames_house_prices$Condition1== "Artery")
     normal <- subset(Ames_house_prices,Ames_house_prices$Condition1=="Norm")  
     artery$SalePrice  
    normal$SalePrice
    boxplot(artery$SalePrice,normal$SalePrice,main = "Sales prices of houses having arterial road vs normal road",names = c("Artery","Normal"),col = c("pink","blue"),notch = TRUE)
      
   })
  
  }
shinyApp(ui = ui, server = server)
