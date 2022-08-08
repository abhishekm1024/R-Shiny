
library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggplot2)

dataset = read.csv("data_1.csv")
names(dataset)[names(dataset) == "ï..Year"] = "Year"

g1_var = c("Industry..Exiobase.", "Country")
g21_var = c("Total.Environmental.Cost", "Fish.Production.Capacity", "Working.Capacity", "Water.Production.Capacity",
            "Meat.Production.Capacity", "Crop.Production.Capacity", "Biodiversity", "Wood.Production.Capacity")
g22_var = c("Industry..Exiobase.", "Country", "Year")

ui = dashboardPage(skin = "purple",
                   dashboardHeader(title="VISU Sem Project Comp2"),
                   dashboardSidebar(
                       sidebarMenu(
                           menuItem("Uni-variate Graphs",tabName = "tab_uni"),
                           menuItem("Bi-variate Graphs",tabName = "tab_bi")
                       )
                   ), 
                   dashboardBody(
                       tabItems(
                           tabItem(tabName = "tab_uni",
                                   h2("Coorporate Environmental Impact"),
                                   h5("Uni-Variable Graphs"),
                                   h5("Select the variable to see a graphical representation"),
                                   selectInput("g1_variable", 
                                               "Choose the Variable", 
                                               g1_var),
                                   actionButton("simulate1", "Show Graph"),
                                   plotOutput("graph1")
                           ),
                           tabItem(tabName = "tab_bi",
                                   h2("Coorporate Environmental Impact"),
                                   h5("Bi-Variable Graphs"),
                                   h5("Select the variable to see a graphical representation"),
                                   selectInput("g21_variable", 
                                               "Choose the Variable", 
                                               g21_var),
                                   selectInput("g22_variable", 
                                               "Choose the Variable", 
                                               g22_var),
                                   actionButton("simulate2", "Show Graph"),
                                   plotOutput("graph2"))
                       )))

server = function(input, output) {
    
    graph1_data = eventReactive(input$simulate1, {
        if( input$g1_variable == "Industry..Exiobase."){
            tabl = table(dataset$Industry..Exiobase.)
            x = as.data.frame(tabl)
            colnames(x) = c("Variable_Values", "Freq")
            x[2] = round(x[2],2)
            x
        }  }
    )
    
    output$graph1 = renderPlot({
        if(input$g1_variable == "Industry..Exiobase."){
            p1 = ggplot(data=graph1_data(), aes(x="",y=Freq,fill=Variable_Values)) +
                geom_col() + coord_polar(theta = "y") +
                geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
                labs(title="Piechart of the Selected Categorical Variable", x="", y="")
            p1
        } 
        else if(input$g1_variable == "Country" ){
            p2 = ggplot(dataset, aes(x=Country)) +
                geom_bar() + coord_flip()
            p2
        }
    })
    
    
    output$graph2 = renderPlot({
        if(input$g22_variable == "Country"){
            if(input$g21_variable == "Working.Capacity"){
                d_2 = ggplot(dataset, aes(x = Working.Capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Total.Environmental.Cost"){
                d_2 = ggplot(dataset, aes(x = Total.Environmental.Cost, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Fish.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Fish.Production.Capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Water.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Water.production.capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Meat.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Meat.Production.Capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Crop.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Crop.Production.Capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Biodiversity"){
                d_2 = ggplot(dataset, aes(x = Biodiversity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Wood.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Wood.Production.Capacity, y = Country)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
        }
        
        else if(input$g22_variable == "Year"){
            if(input$g21_variable == "Working.Capacity"){
                d_1 = ggplot(dataset, aes(x = Year, y = Working.Capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_1}
            else if(input$g21_variable == "Total.Environmental.Cost"){
                d_2 = ggplot(dataset, aes(x = Year, y = Total.Environmental.Cost)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Fish.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Fish.Production.Capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Water.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Water.production.capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Meat.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Meat.Production.Capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Crop.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Crop.Production.Capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Biodiversity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Biodiversity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
            else if(input$g21_variable == "Wood.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Year, y = Wood.Production.Capacity)) +
                    geom_bar(stat = 'identity') + coord_flip()
                d_2}
        }
        
        else if(input$g22_variable == "Industry..Exiobase."){
            if(input$g21_variable == "Working.Capacity"){
                d_2 = ggplot(dataset, aes(x = Working.Capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Total.Environmental.Cost"){
                d_2 = ggplot(dataset, aes(x = Total.Environmental.Cost, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Fish.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Fish.Production.Capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Water.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Water.production.capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Meat.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Meat.Production.Capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Crop.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Crop.Production.Capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Biodiversity"){
                d_2 = ggplot(dataset, aes(x = Biodiversity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
            else if(input$g21_variable == "Wood.Production.Capacity"){
                d_2 = ggplot(dataset, aes(x = Wood.Production.Capacity, y = Industry..Exiobase.)) + 
                    geom_point(stat = "identity") + geom_smooth(method = "lm")
                d_2}
        
    }
    })
    
    
}


shinyApp(ui = ui, server = server)

