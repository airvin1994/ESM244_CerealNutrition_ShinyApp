library(shiny)
library(tidyverse)
library(shinythemes)
library(ggbiplot)
library(ggrepel)

cereals <- read_csv("cereal_dataset.csv") %>% 
  filter(potass >= 0 & carbo >= 0 & sugars >= 0) #remove any negative values
cereals[(4:12)] <- cereals[(4:12)] / cereals[["cups"]] # Normalize all variables to serving size of 1 cup
cereals[] <- lapply(cereals, function(x) if (is.numeric(x)) round(x, 2) else x)
cereals$mfr <- replace(cereals$mfr, cereals$mfr %in% c("A","G","K","N","P","Q","R"), c("American Home Food Products","General Mills", "Kelloggs","Nabisco","Post","Quaker Oats","Ralston Purina"))
colnames(cereals) <- c("Name","mfr","Type","Calories","Protein","Fat","Sodium","Fiber","Carbohydrates","Sugars","Potassium","Vitamins","Shelf","Weight","Cups","Rating")

#Linear Model
cereals$mfr <- as.factor(cereals$mfr)
cereals$Shelf <- as.factor(cereals$Shelf)
cereal_lm <- lm(Rating ~ Calories + Protein + Fat + Sodium + Fiber + Carbohydrates + Sugars + Potassium + Vitamins + Shelf + mfr, data = cereals)
summary(cereal_lm)

finalized_statements <- c(
   "Cereal rating is significantly predicted by calories (p < 0.001), \n with each additional calorie reducing the overall rating of a cereal by -0.22, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05).",
   "Cereal rating is significantly predicted by protein (p < 0.001), \n with each additional gram of protein increasing the overall rating of a cereal by 3.6, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05).",
   "Fat (g) does not significantly predict cereal rating (p = 0.48, F(17,56) = 69.34, alpha = 0.05).",
   "Cereal rating is significantly predicted by sodium (p < 0.001), \n with each additional mg of sodium decreasing the overall rating of a cereal by -0.04, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05).",
   "Cereal rating is significantly predicted by fiber (p < 0.001), \n with each additional gram of fiber increasing the overall rating of a cereal by 1.7, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05).",
   "Cereal rating is significantly predicted by carbohydrates (p < 0.001), \n with each additional gram of carbohydrates increasing the overall rating of a cereal by 1.3, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05).",
   "Sugar (g) does not significantly predict cereal rating (p = 0.57, F(17,56) = 69.34, alpha = 0.05).",
   "Potassium (mg) does not significantly predict cereal rating  (p = 0.24, F(17,56) = 69.34, alpha = 0.05).",
   "Cereal rating is significantly predicted by vitamin percentage (p < 0.001), \n with each additional percentage point reducing the overall rating of a cereal by -0.08, \n assuming all else is equal between two cereals (F(17,56) = 69.34, alpha = 0.05)."
)

names(finalized_statements) <- c("Calories","Protein","Fat","Sodium","Fiber","Carbohydrates","Sugars","Potassium","Vitamins")

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("flatly"),
   
   # Application title
   titlePanel(strong("How Healthy is Your Cereal?")),
   
   navbarPage(em("Alex Irvin and Caitlin Martin"),
              
              tabPanel("Summary",
                       mainPanel(
                         img(src = "spoon.jpg", height = 350, width = 600, align = "center"),
                         
                       h2("What does the app do?"),
                       p("
                         This app visualizes nutritional content of different cereal brands and examines how different nutrients impact cereal nutrition rating. The app uses dietary characteristics and nutritional information associated with 74 different cereal products.", 
                         
                        "Using  principle components analysis and multiple linear regression, the app identifies correlation between nutritional variables and shows how specific variables influence nutrition ratings."),
                       h2("How do I use this app?"),
                       p("Begin by selecting one of the tabs at the top of the page. Each tab shows a different visualization of cereal nutritional variables. Under", span("Nutrition Breakdown", style = "color:darkblue") , "the user can see how the nutritional content of his or her favorite cereal brand compares to a baseline healthy cereal. 
                         
                        Under", span("Principal Components Analysis (PCA)", style = "color:darkblue"), "the user can select different cereal manufacturers and see how the nutritional variables of their cereal brands are related to one another. 

                        Under", span("Nutrients & Rating", style = "color:darkblue"), " , the user can select a nutrient to graph and see how that nutrient influences the overall cereal nutrition rating."),
                       h3("Data Source"),
                       p("The dataset was found on Kaggle, and was compiled by James R. Eagan, as Associate Professor at Telecom ParisTech. The dataset was gathered and cleaned by Petra Isenberg, Pierre Dragicevic, and Yvonne Jansen. There are 16 variables in the dataset and 74 observations for cereal products. All variables were normalized to the unit of 1 cup of cereal. 

                         Nutrition ratings were based off Consumer Reports. Larger values are associated with higher nutrient content.")
                       )      
              ),
              
              tabPanel("Nutrition Breakdown",
                       
                       # Sidebar with a slider input for number of bins
                       sidebarLayout(
                          sidebarPanel(
                             selectInput("cereal", 
                                         "Select a cereal to display:",
                                         choices = cereals$Name),
                             htmlOutput("baseline")
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                           htmlOutput("nutrition")
                          )
                       )),
              
              tabPanel("Nutrients & Rating",
                       sidebarLayout(
                         sidebarPanel(
                          selectInput("nutrient",
                          "Select a nutrient to graph",
                          choices = c("Calories","Protein","Fat","Sodium","Fiber","Carbohydrates","Sugars","Potassium","Vitamins")
                          
                          ),
                          
                         checkboxGroupInput("mfr2", 
                                      "Select a manufacturer:",
                                      choices = unique(cereals$mfr)
                         )
                         ),
                         # Show cereal nutrition information
                         mainPanel(
                           plotOutput("modelPlot")
                           )
                         )
                       ),
              
              
              tabPanel("Principle Components Analysis (PCA)",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           #check boxes to show manufacturer in PCA
                           radioButtons("mfr", 
                                        "Select a manufacturer:",
                                        choices = unique(cereals$mfr)) 
                         ),
                         
                         # Show cereal nutrition information
                         mainPanel(
                           plotOutput("pca", width = "100%")
                         )
                       ))
              )
   )



# Define server logic
server <- function(input, output) {
   
   #Vector with just the baseline cereal information
   baseline_cereal <- filter(cereals, Name == "All-Bran with Extra Fiber")
   
   #Output for baseline cereal nutrition info
   output$baseline <- renderText({
      paste("<font size = 4>",
            "<b>Baseline healthy cereal: All-Bran with Extra Fiber<b>",
            "<font size = 2>",
            "<br><br>",
            paste("Calories: ", baseline_cereal$Calories),
            "<br><br>",
            paste("Fat (g): ", baseline_cereal$Fat),
            "<br><br>",
            paste("Sugar (g): ", baseline_cereal$Sugars),
            "<br><br>",
            paste("Sodium (mg): ", baseline_cereal$Sodium),
            "<br><br>",
            paste("Fiber (g): ", baseline_cereal$Fiber),
            "<br><br>",
            paste("Vitamins and Minerals: ", baseline_cereal$Vitamins),
            "<br><br>",
            paste("Potassium (mg): ", baseline_cereal$Potassium),
            "<br><br>",
            paste("Protein (g): ", baseline_cereal$Protein),
            "<br><br>",
            paste("Carbohydrates (g): ", baseline_cereal$Carbohydrates),
            "<br><br>",
            paste("Nutrition Rating: ", baseline_cereal$Rating),
            "<br><br>",
            paste("Shelf Display Location: ", baseline_cereal$Shelf),
            "<br><br>",
            paste("Manufacturer: ", baseline_cereal$mfr)
            )})
   
   
   #Output for selected cereal nutrition info
   output$nutrition <- renderText({
      
      #Set color based on selection
      #Green = #008000	
      #Red = #FF0000
      #Black = #000000
      
      selected_cereal <- filter(cereals, Name == input$cereal)
      color_vector <- vector()
      bad_nutrients <- c("Calories","Fat","Sodium","Carbohydrates","Sugars")
      
      for (i in 4:length(selected_cereal))
      {
         if (names(baseline_cereal)[i] %in% bad_nutrients ) {
            color_vector <- c(color_vector, ifelse(as.numeric(selected_cereal[1,i]) > as.numeric(baseline_cereal[1,i]),"FF0000\\","008000\\"))
         }
         else if (names(baseline_cereal)[i] == "Shelf") {
            color_vector <- c(color_vector, ifelse(as.numeric(selected_cereal[1,i]) == 2 | as.numeric(selected_cereal[1,i]) == as.numeric(baseline_cereal[1,i]),"008000\\","FF0000\\"))
         }
         else {
            color_vector <- c(color_vector, ifelse(as.numeric(selected_cereal[1,i]) >= as.numeric(baseline_cereal[1,i]),"008000\\","FF0000\\"))
         }
         
      }
      #Print out nutrition info
      paste( "<font size = 5>", #change font size
            "<font color=\"#000000\"><b><br>",                     #change color to black
            "Calories (per 1 cup serving)",                        #title of section
            paste0("<font color=\\",color_vector[1],"><b><br>"),  #change color to red
            filter(cereals, Name == input$cereal)$Calories,        #filter specific cell
            "<br>",                                                #line break
                                         
            "<font color=\"#000000\"><b><br>", 
            "Fat (g)", 
            paste0("<font color=\\",color_vector[3],"><b><br>"),
            filter(cereals, Name == input$cereal)$Fat,
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Sugar (g)", 
            paste0("<font color=\\",color_vector[7],"><b><br>"),
            filter(cereals, Name == input$cereal)$Sugars,
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Sodium (mg)", 
            paste0("<font color=\\",color_vector[4],"><b><br>"),
            filter(cereals, Name == input$cereal)$Sodium, 
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Fiber (g)", 
            paste0("<font color=\\",color_vector[5],"><b><br>"),
            filter(cereals, Name == input$cereal)$Fiber, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Vitamins & Minerals (typical percentage of FDA recommended consumption)", 
            paste0("<font color=\\",color_vector[9],"><b><br>"),
            filter(cereals, Name == input$cereal)$Vitamins, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Potassium (mg)", 
            paste0("<font color=\\",color_vector[8],"><b><br>"),
            filter(cereals, Name == input$cereal)$Potassium, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Protein (g)", 
            paste0("<font color=\\",color_vector[2],"><b><br>"),
            filter(cereals, Name == input$cereal)$Protein, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Complex Carbohydrates (g)", 
            paste0("<font color=\\",color_vector[6],"><b><br>"),
            filter(cereals, Name == input$cereal)$Carbohydrates, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Nutrition Rating", 
            paste0("<font color=\\",color_vector[13],"><b><br>"),
            filter(cereals, Name == input$cereal)$Rating, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Shelf Display Location (1, 2, or 3, counting from the floor)", 
            paste0("<font color=\\",color_vector[10],"><b><br>"),
            filter(cereals, Name == input$cereal)$Shelf, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Manufacturer", 
            "<font color=\"#008000\"><b><br>",
            filter(cereals, Name == input$cereal)$mfr, 
            "<br>"
            )})
   
   
   output$pca <- renderPlot({
      
      #PCA model and graph
      cereal_gm <- cereals %>%
         subset(mfr %in% input$mfr) #filter by manufacturer
      cereal_remove <- cereal_gm %>% 
         select(-c(Shelf, Weight, Cups, mfr, Type)) %>% #removed columns for PCA analysis b/c not numeric values
         column_to_rownames('Name') #change the name to the row name
      cereal_pca <- prcomp(cereal_remove, scale = TRUE)
   
     
      #Graph PCA
      ggbiplot(cereal_pca, labels = cereal_gm$Name, labels.size = 4, varname.size = 5) + 
         theme_classic() + 
         xlim(-2,2) + 
         ylim(-2,2) + 
         ggtitle(paste("PCA of ", input$mfr, "Cereals")) +
         theme(axis.text = element_text(size = 16), 
               axis.title = element_text(size = 16),
               title = element_text(size = 16))
      
   }, width = 800, height = 800)
   
   
   #Graph Tab 4
   output$modelPlot <- renderPlot({
      
     cereals_by_mfr <- cereals %>% 
       subset(mfr %in% input$mfr2)
     
     units <- c("","(g)","(g)","(mg)","(g)","(g)","(g)","(mg)","(%)")
     names(units) <- c("Calories","Protein","Fat","Sodium","Fiber","Carbohydrates","Sugars","Potassium","Vitamins")
      
     ggplot(cereals_by_mfr) +
       geom_point(aes_string(x = input$nutrient, y = "Rating", color = "mfr"), size = 3) +
       scale_color_brewer( palette = "Dark2") +
       theme_classic() +
       geom_text_repel(aes_string(label = "Name", x = input$nutrient, y = "Rating"), hjust = 0.2, vjust = 1) +
       #geom_text(aes(label = name, x = calories, y =rating), hjust = 0.2, vjust = 1) +
       theme(panel.grid.major=element_blank()) +
       theme(plot.caption = element_text(hjust = 0.5, size = 17, face = "bold"), 
             axis.text = element_text(size = 16), 
             axis.title = element_text(size = 16),
             legend.text = element_text(size = 16), 
             legend.title = element_text(size = 16),
             title = element_text(size = 16)) +
       xlab (paste("\n",input$nutrient, units[input$nutrient])) +
       ylab ('Cereal Nutrition Rating\n') +
       labs(color='Manufacturer') +
       theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
       labs(caption = paste("\n",finalized_statements[input$nutrient])) +
       ggtitle(paste("Rating vs.", input$nutrient)) 
       #scale_x_continuous(expand = c(0, 0)) +
     # scale_y_continuous(expand = c(0, 0)) 
     
   }, width = 900, height = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)

