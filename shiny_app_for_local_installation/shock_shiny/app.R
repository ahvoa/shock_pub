#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
for (package in c("shiny", "ggplot2", "viridis", "plotly", "shinyWidgets",
                  "tmap", "tmaptools", "terra", "sf", "leaflet", "raster",
                  "scico", "dplyr", "tidyr", "scales", "wesanderson", "mapview",
                  "fullPage", "gridExtra")) {
  if (!require(package, character.only=TRUE, quietly=TRUE)) {
    install.packages(package)
  }
}
library(shiny)
library(ggplot2)
library(viridis)
library(plotly)
library(shinyWidgets)
library(tmap)
library(tmaptools)
library(terra)
library(sf)
library(leaflet)
library(raster)
library(scico)
library(dplyr)
library(tidyr)
library(scales)
library(wesanderson)
library(mapview)
library(fullPage)
library(gridExtra)

##### Helper variables and functions #####

crop_list <- c("barley", "cassava", "groundnut", "maize", "millet", "potato", "rice", "sorghum", "soybean", "sugarbeet", "sugarcane", "wheat")
scenario_labs <- c("25 %", "50 %", "75 %")
data("World")
rob_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
#change projection on raster and World vector
data("World")
World_geo <- World$geometry
tmap_options(max.raster = c(plot = 1e9, view = 1e9))
ctrl_list <- list(attributionControl = FALSE,
                  zoomDelta = 0.5)
mapviewOptions(fgb = TRUE)
names(scenario_labs) <- c(25, 50, 75)
pal_bivariate <- c("#d3d3d3", "#b6cdcd", "#97c5c5", "#75bebe", "#52b6b6",
                   "#d6c1a6", "#b8bba1", "#99b49c", "#77ae96", "#53a690",
                   "#d9ad77", "#bba874", "#9ca26f", "#799c6b", "#549567",
                   "#dd9843", "#bf9341", "#9e8e3f", "#7b893d", "#56833a",
                   "#e17e06", "#c27b06", "#a17606", "#7d7206", "#576d05")

scenario_pal <- viridis(n = 5, option = "magma", direction = -1)
pal_map <- colorNumeric("magma", domain = c(-100, 0), reverse = FALSE, na.color = NA)
input_color_list <- list("nitrogen" = "#326286",
                         "phosphorus" =  "#CF5422",
                         "potassium" =  "#FD6467",
                         "machinery" =  "#5B1A18",
                         "pesticides" = "#4E2E74",
                         "crop yield" =  "#157567")

input_unit_list <- list("nitrogen" = "kg/ha",
                        "phosphorus" =  "kg/ha",
                        "potassium" =  "kg/ha",
                        "machinery" =  "tractors/km2",
                        "pesticides" =  "rescaled sum",
                        "crop yield" =  "t/ha")


get(load("data/prod_change_bardata.RData"))
get(load("data/prod_change_countries.RData"))
get(load("data/NSE_data.RData"))

prod_change_raster <- raster("data/prod_change_raster.tif")
earthstat_file <- "data/barley_YieldPerHectare.tif"


make_shock_raster_shiny <- function(crop_scenarios, shock_column, scenario_number) {
  
  #select data for raster, make columns of scenario percents
  raster_data <- crop_scenarios %>%
    dplyr::select(cell, shock = shock_column, observed_normal_yield, scenario_percent) %>%
    dplyr::filter(scenario_percent == scenario_number)%>%
    mutate(shock_value = 100 * ((shock - observed_normal_yield)/ observed_normal_yield))%>%
    dplyr::select(cell, shock_value)
  
  #turn all positive values to zero
  raster_data$shock_value[raster_data$shock_value > 0] <- 0
  raster_data$shock_value <- round(raster_data$shock_value)
  
  #get a raster file to input shock values to
  
  yield_raster <- rast(earthstat_file)
  
  #substitute all values of the raster
  yield_raster[] <- 9999
  
  #create a dataframe of the raster with cell numbers and coordinates
  yield_df <- terra::as.data.frame(yield_raster, xy = TRUE, cells = TRUE)
  
  #join shock data to empty raster by cell number
  raster_data_with_all_cells <- left_join(yield_df, raster_data, by = "cell")
  #delete cell number column and yield column
  raster_data_with_all_cells <- raster_data_with_all_cells %>%
    dplyr::select(-cell, -barley_YieldPerHectare)
  
  #create raster
  shock_raster <- rasterFromXYZ(raster_data_with_all_cells, res = c(0.08333333, 0.08333333), crs = "+proj=longlat +datum=WGS84", digits = 6)

  # #create a vector of shock values
  # shock_vec <- na.omit(c(as.matrix(shock_raster)))
  # 
  # #substitute NA-values of shock raster
  # shock_raster[is.na(shock_raster)] <- 9999
  # 
  # #select all result values for mask
  # shock_mask <- shock_raster <= 0
  # 
  # #create a raster base for the final
  # final_raster <- rast(shock_mask)
  # 
  # #substitute base raster shock mask area with result values
  # final_raster[shock_mask] <- shock_vec
  
  return(shock_raster)
  
}

make_input_raster_shiny <- function(crop_scenarios, shock_column, scenario_number) {
  
  #select data for raster, make columns of scenario percents
  raster_data <- crop_scenarios %>%
    dplyr::select(cell, shock = shock_column, scenario_percent) %>%
    dplyr::filter(scenario_percent == scenario_number)%>%
    dplyr::select(cell, shock)
  
  #turn all positive values to zero
  #raster_data$shock_value[raster_data$shock_value > 0] <- 0
  #raster_data$shock_value <- round(raster_data$shock)
  
  #get a raster file to input shock values to
  # earthstat_file <- list.files(path = "data/",
  #                              recursive = TRUE, pattern = paste0(crop_list[1], "_YieldPerHectare.tif"), full.names = TRUE)
  yield_raster <- rast(earthstat_file)
  
  #substitute all values of the raster
  yield_raster[] <- 9999
  
  #create a dataframe of the raster with cell numbers and coordinates
  yield_df <- terra::as.data.frame(yield_raster, xy = TRUE, cells = TRUE)
  
  #join shock data to empty raster by cell number
  raster_data_with_all_cells <- left_join(yield_df, raster_data, by = "cell")
  #delete cell number column and yield column
  raster_data_with_all_cells <- raster_data_with_all_cells %>%
    dplyr::select(-cell, -barley_YieldPerHectare)
  
  #create raster
  shock_raster <- rasterFromXYZ(raster_data_with_all_cells, res = c(0.08333333, 0.08333333), crs = "+proj=longlat +datum=WGS84", digits = 6)

  return(shock_raster)
  
}

plot_one_loess_ale <- function(ale_dataframe, min, max, input_name_from_list, unit_from_list) {
  
  ale_plot <- ale_dataframe %>%
    dplyr::filter(input_name == input_name_from_list)%>%
    ggplot(aes(x = input_rate, y = ale_value, color = as.factor(bin)))+
    geom_smooth(aes(group = as.factor(bin)), method="loess", size = 0.75, formula=y~x, span = 0.6, se = FALSE) +
    scale_color_manual(name = "bin", values = pal_bivariate)+
    scale_y_continuous(name = "ALE", limits = c(min, max))+
    ggtitle(input_name_from_list)+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_blank())
  
  ale_plot <- grid.arrange(ale_plot, bottom = grid::textGrob(unit_from_list))
  
  return(ale_plot)
  
}


plot_all_loess_ales <- function(ale_dataframe, crop) {
  y_max <- max(ale_dataframe$ale_value)
  y_min <- min(ale_dataframe$ale_value)
  
  plot_list <- list()
  
  input_list <- c("N-rate", "P-rate", "K-rate", "machinery",
                  "pesticides", "irrigation")
  
  unit_list <- c("(kg/ha)", "(kg/ha)", "(kg/ha)", "(tractors/km2)", "(rescaled sum)", "(%)")
  
  
  for (x in 1:length(input_list)) {
    
    plot_list[[x]] <- plot_one_loess_ale(ale_dataframe, y_min, y_max, input_list[x], unit_list[x])
  }
  
  grid.arrange(grobs = plot_list, nrow = 2, bottom=grid::textGrob("input"), left=grid::textGrob("ALE-score", rot = 90), top=grid::textGrob(paste0(crop, " ALEs for all bins")))
  
}

plot_one_ale <- function(ale_dataframe, min, max, input_name_from_list, unit_from_list) {
  
  ale_plot <- ale_dataframe %>%
    dplyr::filter(input_name == input_name_from_list)%>%
    ggplot()+
    geom_line(aes(x = input_rate, y = ale_value, group = as.factor(iteration), color = "grey"))+
    scale_y_continuous(name = "ALE", limits = c(min, max))+
    scale_color_manual(name = "iteration", values = "gray")+
    ggtitle(input_name_from_list)+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_blank(),)
  
  ale_plot <- grid.arrange(ale_plot, bottom = grid::textGrob(unit_from_list))
  
  return(ale_plot)
  
}


plot_all_ales <- function(ale_dataframe, crop, bin) {
  y_max <- max(ale_dataframe$ale_value)
  y_min <- min(ale_dataframe$ale_value)
  
  plot_list <- list()
  
  input_list <- c("N-rate", "P-rate", "K-rate", "machinery",
                  "pesticides", "irrigation")
  
  unit_list <- c("(kg/ha)", "(kg/ha)", "(kg/ha)", "(tractors/km2)", "(rescaled sum)", "(%)")
  
  for (x in 1:length(input_list)) {
    
    plot_list[[x]] <- plot_one_ale(ale_dataframe, y_min, y_max, input_list[x], unit_list[x])
  }
  
  grid.arrange(grobs = plot_list, nrow = 2, bottom=grid::textGrob("input"), left=grid::textGrob("ALE-score", rot = 90), top=grid::textGrob(paste0(crop, " climatebin ", bin)))
  
}


##### UI #####
# Define UI for application that draws 
ui <- pagePiling(center = FALSE,
                 sections.color = c("#F1BB7B",
                                    "white",
                                    "white",
                                    "white",
                                    "white",
                                    "white",
                                    "white",
                                    "white",
                                    "white"),
                
                 menu = c(
                   "Start" = "intro",
                   "Climate bins" = "climate",
                   "Agricultural inputs" = "inputs",
                   "Model" = "model",
                   "Scenario maps" = "maps",
                   "Tileplots" = "tiles",
                   "Scatterplots" = "scatter",
                   "Production" = "production",
                   "Model performance" = "performance"),
                 
                 
        
        ###### Start tab ######         
        pageSection(menu = "intro", 
                 h1(style = "text-align: center;", "Agricultural input shocks"),
                 h3(style = "text-align: center;", "What we depend on to grow food and how"),
                 br(),
                 br(),
                 br(),
            fluidRow(
              column(6,
                column(2),
                column(9,
                       h4("Food production today is more global than ever. 
                          Food trade ensures adequate and diverse food even in 
                          areas with low self-sufficiency. Foodstuffs are being 
                          traded across the world, but so are agricultural inputs
                          such as fertilizers, machinery, and pesticides. Shocks 
                          and disturbances in the trade flows of agricultural inputs,
                          caused by e.g. conflict, may be devastating to the food 
                          production and yields of otherwise self-sufficient countries.
                          This aspect of food security and resilience requires more attention.
                          In this study, we modelled the effects of agricultural input 
                          shocks using global spatial data on crop yields, fertilizers,
                          machinery and pesticides with random forest, a machine 
                          learning algorithm.")),
                column(1)
                
                  ),
              column(6,
                  column(11,
                         br(),
                         br(),
                         img(src = "inputs.png", width = "100%")),
                  column(1))
              
            ),
        

                 ),

        ###### Climate tab ######
        pageSection(menu = "climate",
               fluidRow(
                 
                 column(4,
                        column(1),
                        column(11, 
                               br(),
                               br(),
                               tags$p("Climate affects how crops grow. To eliminate that effect and only study agricultural inputs,
                               we divided each crop into climate zones or bins according to temperature and precipitation. 
                               We then analyzed and modelled each bin individually."),
                               tags$p("Check out the climate bins of all crops below."),
                              selectInput("bincrop", label = "Select crop",
                                          choices = crop_list,
                                          selected = "barley"),
                              
                              img(src = "climate_png.png", width = "100%"))),
                 
                  column(8,
                         br(),
                         br(),
                         br(),
                         h3(style = "text-align: center;", "Climate bins"),
                         br(),
                         column(11, tmapOutput("climatebinmap")),
                         column(1))
                       
                        #tabpanel ends
                
              ) #tabset ends
              
            ), #mainpanel ends
            
        ###### Inputs tab ######
        pageSection(menu = "inputs",
              fluidRow(
                br(),
                column(6,
                  column(2),
                  column(10,
                         h4("Visualize crop yields and agricultural inputs in maps 
                                and as averages in each climate bin."))),
                column(2, offset = 1,
                       
                       column(12,
                       selectInput("inputcrop", "Select crop",
                                   choices = crop_list,
                                   selected = "barley"))),
                column(2,
                    column(12,
                       selectInput("agriinput", "Select data to display",
                                   choices = c("crop yield", 
                                               "nitrogen", 
                                               "phosphorus",
                                               "potassium", 
                                               "machinery",
                                               "pesticides"),
                                   selected = "crop yield")))
            
                ),
              fluidRow(
                column(6, offset = 1,
                       leafletOutput("inputmap")
                       ),
                column(5,
                  column(11,
                       plotlyOutput("inputtile")),
                  column(1))
                
                
              )),
                    

        ###### Model tab ######
        pageSection(menu = "model",
                h2(style = "text-align: center;","How did we model the effects
                   of agricultural input shocks on crop yields?"),
                br(),
                
              fluidRow(
                    column(8,
                      column(1),
                      column(11,
                        h4(" Random forest is a machine learning method. 
                           It builds hundreds of decision
                           trees or regression trees and then the model
                           output is an average of all the trees.
                           "),
                        h4("We built a model with the random forest algoritm and global data of
                               crop yields, nitrogen fertilizer, phosphorus fertilizer, 
                               potassium fertilizer, agricultural machinery, and pesticides. 
                               Then we used that model to predict how decreases or shocks in different 
                               agricultural inputs would affect yields."))),
                    column(3, img(src = "make_model.png", width = "100%"))),
         hr(),
         br(),

              fluidRow(
                column(1),
                column(5,
                       img(src = "use_model.png", width = "100%")),
                column(4, offset = 1,
                       h4("To model agricultural input shock scenarios, new data 
                       was created with a decrease of", strong("25%, 50% or 75%"), "in either:",
                      strong("nitrogen, "), strong("phosphorus, "), 
                      strong("potassium, "), strong("machinery,", "or",),
                      strong("pesticides,"), "or in ", strong("all fertilizers"), "or in ",
                      strong("all inputs"), "togehter.",
                      "See results in the following pages!"),
                      
                    ))),
                    
                    
        ###### Maps ######
        
        pageSection(menu = "maps",
                 fluidRow(
                   
                   column(3, align = "center",
                        column(1),
                        column(11,
                          h4("Draw maps from different angricultural input shock scenarios"),
                          br(),
                          selectInput("mapcrop", "Select crop:",
                                      choices = crop_list,
                                      selected = "barley"),
                          
                          selectInput("mapshock", "Select shock:",
                                      choices = c("nitrogen shock", "phosphorus shock",
                                                  "potassium shock",
                                                  "machinery shock",
                                                  "pesticide shock",
                                                  "fertilizer shock", "shock in all inputs"), 
                                      selected = "nitrogen shock"),
                          
                          prettyRadioButtons("mapscenario", "Select shock severity:",
                                       choiceNames = c("25%", "50%", "75%"),
                                       choiceValues = c(25, 50, 75),
                                       selected = 50,
                                       status = "warning",
                                       outline = FALSE,
                                       animation = "smooth")
                          )),
                   
                   column(9,
                        column(10,
                          h4(textOutput("maptitle")),
                          leafletOutput("scenariomap"),
                          helpText("Yield decrease in % from the original yield. 
                                   The map only shows yield decreases and omits increases.")),
                        column(2))
                 )),
        
        ###### Tiles ###### 
        pageSection(menu = "tiles",
                    fluidRow(
                      
                      column(3, align = "center",
                             br(),
                          column(1),
                          column(11,
                             h4("Examine scenario shocks from tileplots"),
                             br(),
                             selectInput("tilecrop", "Select crop",
                                         choices = crop_list,
                                         selected = "barley"),
                             
                             prettyRadioButtons("tilescenario", "Select shock severity",
                                          choices = list("25%" = 25, "50%" = 50, "75%" = 75),
                                          #choiceValues = c(25, 50, 75),
                                          selected = 50,
                                          status = "warning",
                                          outline = FALSE,
                                          animation = "smooth"),
                             
                             prettyRadioButtons("tilecount", "Select to see either share of cells with yield decline (share),
                            or the mean decline in them (mean)",
                                          choices = c("share", "mean"),
                                          selected = "share",
                                          status = "warning",
                                          outline = FALSE,
                                          animation = "smooth"),
                             
                             "Select if you would like to see tileplots grouped in climate bins",
                             prettyCheckbox("tileclimate", " ",
                                           value = FALSE,
                                           status = "warning",
                                           fill = TRUE,
                                           animation = "smooth")
                             
                      )),
                      
                      column(9,
                             column(11, 
                                    br(),
                                    plotlyOutput("tileplot"),
                                    helpText(strong("share:"),"each tile represents
                                           the share of the respective climate bin’s
                                             cells where yield decline after the shock
                                             was greater than 10% from the original yield.",
                                    (strong("mean:")), "the colour of the tile
                                             reflects the average yield decline of all 
                                             the cells where decline was greater than 10% of 
                                             the original yield (note the different colour scale).")),
                             column(1))
                    )
                    
        ),
        
        
        ###### Scatter tab ######
        pageSection(menu = "scatter",
                    
                fluidRow(chooseSliderSkin("Flat", color = "#5B1A18"),
                      
                      column(3,
                        column(1),
                        column(11,
                             h4("Examine shock scatterplots with different agricultural inputs."),
                             
                             selectInput("crop", label = "Choose crop:",
                                         choices = c("barley", "cassava", "groundnut", "maize",
                                                     "millet", "potato", "rice", "sorghum",
                                                     "soybean", "sugarbeet", "sugarcane", "wheat"),
                                         selected = "barley"),
                             
                             selectInput("shock", label = "Choose shock type:",
                                         choices = c("nitrogen shock", "phosphorus shock",
                                                     "potassium shock",
                                                     "machinery shock",
                                                     "pesticide shock",
                                                     "fertilizer shock", "shock in all inputs"), 
                                         selected = "nitrogen shock"),
                             
                             selectInput("agri", 
                                         label = "Choose agricultural input rate (colour):",
                                         choices = c("nitrogen", 
                                                     "phosphorus",
                                                     "potassium", 
                                                     "machinery",
                                                     "pesticides"),
                                         selected = "nitrogen"),
                             
                             sliderInput("bin", 
                                         label = "Choose climate bin:",
                                         min = 1, max = 25, value = 10),
                             
                             prettyCheckbox("abline",
                                           label = "Show 1:1 line",
                                           value = FALSE,
                                           status = "warning",
                                           fill = TRUE,
                                           animation = "smooth")
                             
                      )), #column ends
                      
                      
                      column(9,
                          column(11, 
                                 br(),
                                 br(),
                                 plotOutput("agriplot")),
                          column(1)) #buffer
                    ) #column ends
        ), #page ends
        
        
        
        ###### Production ######
        pageSection(menu = "production",
                 fluidRow(
                   
                   column(12,
                    column(1),
                    column(10,
                          
                      tabsetPanel(
                        
                        tabPanel("Different crops",
                            column(3,
                                   br(),
                                   br(),
                                   tags$p("The effects of agricultural input shocks 
                                          can also be examined through production, or total volume of cultivated crop."),
                                   
                                   tags$p("Production is", strong("yield * harvested area."), " In this barplot you can examine
                                          shock effects on the global production of different crops individually."),
                                   tags$p("In the tabs ", tags$i("All crops"),
                                          "you can see the effects on all 12 crops summed together.
                                          In the tab", tags$i("All countries"), "production decreases are summed for whole countries.")),
                            
                            column(9,
                                  plotlyOutput("productionbars"))),    
                      
                        tabPanel("All crops",
                                 h5(style = "text-align: center;", "Change in production for all 12 crops after 50% shock in all inputs"),
                                  tmapOutput("productionraster")),
                      
                        tabPanel("All countries",
                                 h5(style = "text-align: center;", "Countrywise change in the total production of all 12 crops after 50% shock in all inputs"),
                                  tmapOutput("productioncountries"))
                      
                   )),
                   column(1)
                 
                 ))),
        
        ###### Performance ######
        pageSection(menu = "performance",
            column(1),
            column(11,
                    
              tabsetPanel(
                tabPanel("NSE-scores",
                     fluidRow(
                       column(3,
                          column(1),
                          column(11,
                                 br(),
                                 br(),
                              tags$p("Model performance can be measured by comparing results generated by the model to 
                                     results we know previously. In this case, random forest modelled yields that we compared to
                                      known yields. NSE or ", tags$i("Nash-Sutcliffe Efficiency"), " is one measure of 
                                      the models predicting power. A model with NSE score 1 is a perfect model, and models between 0-1 are
                                      acceptable. Models with NSE > 0.65 are good and NSE > 0.75 are very good."))),
                       column(9,
                          column(11,
                                 br(),
                            plotOutput("NSE")),
                          column(1)))),
                
                tabPanel("RMSE-scores",
                    fluidRow(
                      column(3,
                        column(1),
                        column(11,
                               br(),
                               selectInput("RMSE", label = "Select crop",
                                           choices = crop_list,
                                           selected = "barley"),
                               br(),
                               tags$p("RMSE or ", tags$i("Root Mean Square Error"), " is another measure of model performance.
                                      Here we see RMSE-values calculated from when training the model ", tags$strong("(train)"), "and then again when testing
                                       with external data ", tags$strong("(test)."), " If the RMSE-scores are not significantly 
                                      different between train and test, the model does not overfit."),
                              
                         )),
                      column(9,
                          column(11,
                             plotOutput("RMSE")),
                          column(1))
                         )),
                tabPanel("ALE-plots",
                    fluidRow(
                      column(4,
                        column(12,
                               br(),
                               br(),
                               tags$p("Model behaviour can be measured with ALE or ",
                                      tags$i("Accumulated Local Effects."), 
                                             "The ALE score is the parameter effect in relation to the average of the model."),
                              
                         selectInput("ALE", label = "Select crop",
                                     choices = crop_list,
                                     selected = "barley"),
                         sliderInput("ALEbin", label = "Select climate bin",
                                     min = 1, max = 25, value = 10),
                         prettyCheckbox("wholecropALE", label = "See ALE-scores for the whole crop",
                                       value = FALSE,
                                       status = "warning",
                                       fill = TRUE,
                                       animation = "smooth")
                         #img(src = "climate_png.png", width = "100%")
                         
                         )),
                      
                      column(8,
                        column(11,
                               br(),
                               br(),
                             plotOutput("aleplot")),
                        column(1))))
              )
                 )))



##### SERVER #####
# Define server logic required to draw 
server <- function(input, output) {
  
  ###### Climate bin plots ######
  
  bin_data <- reactive({
    raster_name <- paste0('data/', input$bincrop, "_binmatrix.tif")
    climate_bins <- rast(raster_name)
  })
  
  output$climatebinmap <- renderTmap({

    tm_shape(bin_data()) +
        tm_raster(style = "cont" , # draw gradient instead of classified
                  palette = pal_bivariate,
                  colorNA = "white",
                  title = input$bincrop,
                  legend.show = FALSE) +
        tm_shape(World) +
        tm_borders(col = "grey") +
        tm_view(alpha = 1, set.view = c(30,50,2),
                leaflet.options = ctrl_list,
                view.legend.position = NA)

  })

  ###### Input plots ######
  
  #load data
  input_map_data <- reactive({
    file_name <- paste0("data/", input$inputcrop, "_scenario_summary.RData")
    get(load(file_name))
    colnames(crop_scenarios) <- c("cell", "scenario_percent", "x", "y", "crop yield",
                                  "nitrogen shock", "phosphorus shock",
                                  "potassium shock", "machinery shock",
                                  "pesticide shock", "fertilizer shock", 
                                  "shock in all inputs", "bin", "nitrogen", 
                                  "phosphorus","potassium", "machinery",
                                  "pesticides", "irrigation")
    as.data.frame(crop_scenarios)
  })
  
  #make raster
  input_raster <- reactive({
    
    input_raster <- make_input_raster_shiny(input_map_data(), input$agriinput, 50)
    names(input_raster) <- paste(input$agriinput, unlist(input_unit_list[input$agriinput]))
    input_raster
    
  })
  
  
  #render output

  output$inputmap <- renderLeaflet({
    
    map <- mapview(input_raster(),
                   use.layer.names = TRUE,
                   col.regions = c("#F9E1C3", unlist(input_color_list[input$agriinput])),
                   na.color = NA,
                   alpha.regions = 1,
                   maxpixels =  9331200,
                   map.types = "Esri.WorldGrayCanvas",
                   query.digits = 1,
                   query.prefix = "") 
    
    map@map %>%
      setView(lng=30, lat=50, zoom =2)
    
  })
  
  output$inputtile <- renderPlotly({
    
    input_means <- input_map_data() %>%
      dplyr::select(bin, agri = input$agriinput) %>%
      group_by(bin) %>%
      summarise(across(agri, mean))
    
    input_means$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
    input_means$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
    
    input_tile <- input_means %>%
      ggplot(aes(ones, tens, text = paste0("bin ", bin))) +
      geom_tile(aes(fill = agri)) + 
      geom_text(aes(label = round(agri, 1)))+
      xlab(paste("precipitation \u2192"))+
      ylab(paste("temperature \u2192")) +
      scale_fill_gradient(low = "#F9E1C3", high = unlist(input_color_list[input$agriinput]),
                          name = paste0(input$agriinput, "\n", unlist(input_unit_list[input$agriinput])))+
      theme(axis.text.x = element_text(angle=90))+
      theme_classic()+
      ggtitle(paste0("Average ", input$agriinput, " in each climate bin"))+
      theme(axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            strip.background = element_blank())
    
    ggplotly(input_tile, tooltip = "text")
    
  })

  
  ###### load crop scenarios for scatterplot ######
  plot_data <- reactive({
    file_name <- paste0( "data/", input$crop, "_scenario_summary.RData")
    get(load(file_name))
    colnames(crop_scenarios) <- c("cell", "scenario_percent", "x", "y", "observed_normal_yield",
                               "nitrogen shock", "phosphorus shock",
                               "potassium shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", 
                               "shock in all inputs", "bin", "nitrogen", 
                               "phosphorus",
                               "potassium", 
                               "machinery", "pesticides", "irrigation")
    as.data.frame(crop_scenarios)
  })
  
  ###### scatterplot ######
  output$agriplot <- renderPlot({
    
    scatter_plot <- plot_data() %>%
      dplyr::filter(bin == input$bin) %>%
      dplyr::select(shock = input$shock, scenario_percent, input_rate = input$agri, observed_normal_yield, bin) %>%
      ggplot(aes(x = observed_normal_yield, y = shock))+
      geom_jitter(aes(color = input_rate), alpha = 0.5)+
      scale_color_gradient(name = paste0(input$agri, "\nkg/ha"), low = "#F9E1C3", high = unlist(input_color_list[input$agri]))+
      #scale_color_viridis(name = paste0(input$agri, "\nkg/ha"), option = "magma", direction = -1)+
      theme_classic()+
      labs(x = "original yield t/ha", y = "yield after shock t/ha", title = paste0(input$shock, ", ", input$crop, " climate bin ", input$bin))+
      theme(strip.background = element_blank())+
      facet_wrap(~scenario_percent, labeller = labeller(scenario_percent = scenario_labs))
    
    if (input$abline) {
      scatter_plot <- scatter_plot + 
        geom_abline(slope = 1, intercept = 0, color = "red")

    }
    
    plot(scatter_plot)
    
  })
  
  ###### Tile plots ######
  
  tileplot_data <- reactive({
    file_name <- paste0("data/", input$crop, "_scenario_summary.RData")
    get(load(file_name))
    
  })
  
  output$tileplot <- renderPlotly({
    
    if (input$tilecount == "share") {
    
      count_test <- tileplot_data() %>%
        dplyr::group_by(scenario_percent, bin) %>%
        dplyr::summarise(across(contains("shock"), ~ sum( .x < 0.9 * observed_normal_yield)))
      
      count_test <- tileplot_data() %>%
        dplyr::filter(scenario_percent == 25) %>%
        dplyr::count(bin) %>%
        right_join(count_test, by = "bin")
      
      count_test <- count_test %>%
        dplyr::mutate(across(contains("shock"), ~ (.x/n)*100))
      
      if (input$tileclimate) {
      
        ### make climate tile plots for count
        
        climate_count <- count_test %>%
          dplyr::filter(scenario_percent == input$tilescenario)
        
        climate_count$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
        climate_count$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
        
        climate_pivot_count <- climate_count %>%
          dplyr::select(-scenario_percent, -n)%>%
          pivot_longer(c(2:8), names_to = "shock", values_to = "count")
        
        tile_plot <- climate_pivot_count%>%
          mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                "machinery_shock", "rescaled_pesticide_sum_shock",
                                                "shock_fert_only", "shock_all"))) %>%
          mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                         "machinery_shock", "rescaled_pesticide_sum_shock",
                                                         "shock_fert_only", "shock_all"),
                        to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
          ggplot(aes(ones, tens, text = paste0("bin: ", bin, "\n% of bin area: ",
                                               round(count, 2)))) +
          geom_text(aes(label = bin), size = 2)+
          geom_tile(aes(fill = count)) + 
          xlab(paste("precipitation \u2192"))+
          ylab(paste("temperature \u2192")) +
          #scale_fill_viridis(direction = 1, name = "% area of bin")+
          scale_fill_scico(name = "% area of bin", palette = "batlow", direction = 1, limits = c(0, 100))+
          ggtitle(paste0(input$tilecrop, ", cells where yield decline was more than 10%")) +
          theme(axis.text.x = element_text(angle=90))+
          theme_classic()+
          theme(axis.ticks = element_blank(),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.text = element_blank(),
                strip.background = element_blank()) +
          facet_wrap(~shock, nrow = 2)
        
        ggplotly(tile_plot, tooltip = "text")
      
      } else {
  
      ### make normal tile plot count
      
        count_pivot <- count_test %>%
          pivot_longer(cols = c(4:10), names_to = "shock" , values_to = "percent")
        
        tile_plot <- count_pivot %>%
          dplyr::filter(scenario_percent == input$tilescenario)%>%
          ggplot(aes(as.numeric(bin), shock, text = paste0("bin: ", bin, "\n% of bin area: ",
                                                           round(percent, 2))))+
          geom_tile(aes(fill = percent))+
          scale_fill_scico(name = "% of bin area", palette = "batlow", direction = 1, limits = c(0, 100))+
          scale_y_discrete("",limits = unique(count_pivot$shock), 
                           labels = c("N shock", "P shock", "K shock", "machinery shock",
                                      "pesticide shock", "fertilizer shock", "shock all"))+
          scale_x_continuous("bin", breaks = seq(5,25,5))+
          ggtitle(paste0(input$tilecrop, ", cells where yield decline was more than 10%"))+
          theme(#axis.text.x = element_text(size = 7, angle = 0),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank())#+
          #facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
      
          ggplotly(tile_plot, tooltip = "text")
        }  
    } else {
    
    ### make plot from yield decrease depth
    
      depth_test <- tileplot_data() %>%
        dplyr::group_by(scenario_percent, bin) %>%
        dplyr::mutate(across(contains("shock"), ~ 100*( .x - observed_normal_yield)/observed_normal_yield)) %>%
        dplyr::select(-x, -y, -observed_normal_yield, -N_rate, -P_rate, -K_rate, -machinery, -rescaled_pesticide_sum, -irrigation_share)
      
      depth_test_neg <- depth_test %>%
        dplyr::mutate(across(contains("shock"), ~ replace(.x, .x > -10, NA)))
      
      next_test <- depth_test_neg %>%
        dplyr::select(-cell)%>%
        dplyr::group_by(bin, scenario_percent) %>%
        dplyr::summarise(across(contains("shock"), ~mean(.x, na.rm = TRUE)))
      
      ### make graph with climate tiles ###
      
      if (input$tileclimate) {
      
        climate_depth <- next_test %>%
          dplyr::filter(scenario_percent == input$tilescenario)
        
        climate_depth$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
        climate_depth$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)
        
        climate_pivot <- climate_depth %>%
          dplyr::select(-scenario_percent)%>%
          pivot_longer(c(2:8), names_to = "shock", values_to = "decline")
        
        tile_plot <- climate_pivot %>%
          mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                "machinery_shock", "rescaled_pesticide_sum_shock",
                                                "shock_fert_only", "shock_all"))) %>%
          mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                         "machinery_shock", "rescaled_pesticide_sum_shock",
                                                         "shock_fert_only", "shock_all"),
                        to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                               "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
          ggplot(aes(ones, tens, text = paste0("bin: ", bin, "\naverage decline: ",
                                               round(decline, 2)))) +
          geom_text(aes(label = bin), size = 2)+
          geom_tile(aes(fill = decline)) + 
          xlab(paste("precipitation \u2192"))+
          ylab(paste("temperature \u2192")) +
          scale_fill_scico(name = "average yield\ndecline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
          ggtitle(paste0(input$tilecrop, ", mean yield decline of bin cells where yield decline was more than 10%")) +
          theme(axis.text.x = element_text(angle=90))+
          theme_classic()+
          theme(axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                strip.background = element_blank()) +
          facet_wrap(~shock, nrow = 2)
        
        ggplotly(tile_plot, tooltip = "text")
        
        
      } else {
      ### make normal depth tile plot
      
        depth_pivot <- next_test %>%
          dplyr::filter(scenario_percent == input$tilescenario) %>%
          pivot_longer(c(3:9), names_to = "shock", values_to = "decline")
        
        tile_plot <- depth_pivot %>%
          ggplot(aes(as.numeric(bin), shock, text = paste0("bin: ", bin, "\naverage decline: ",
                                                           round(decline, 2)))) +
          geom_tile(aes(fill = decline))+
          scale_fill_scico(name = "average yield\ndecline < -10%", palette = "batlow", direction = -1, limits = c(-50, -10), oob = squish)+
          scale_y_discrete("",limits = unique(depth_pivot$shock), 
                           labels = c("N shock", "P shock", "K shock", "machinery shock",
                                      "pesticide shock", "fertilizer shock", "shock all"))+
          scale_x_continuous("bin", breaks = seq(5,25,5))+
          ggtitle(paste0(input$tilecrop, ", Mean yield decline of bin cells where yield decline was more than 10%"))+
          theme(#axis.text.x = element_text(size = 7, angle = 0),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank())#+
          #facet_wrap(~scenario_percent, nrow = 1, labeller = labeller(scenario_percent = scenario_labs))
      
        ggplotly(tile_plot, tooltip = "text")
        
        }
    }
    
  })
  
  ###### Maps ######
  

  #load data
  scenario_map_data <- reactive({
    file_name <- paste0("data/", input$mapcrop, "_scenario_summary.RData")
    get(load(file_name))
    colnames(crop_scenarios) <- c("cell", "scenario_percent", "x", "y", "observed_normal_yield",
                                  "nitrogen shock", "phosphorus shock",
                                  "potassium shock", "machinery shock",
                                  "pesticide shock", "fertilizer shock", 
                                  "shock in all inputs", "bin", "nitrogen fertilizer", 
                                  "phosphorus fertilizer",
                                  "potassium fertilizer", 
                                  "machinery", "pesticides", "irrigation")
    as.data.frame(crop_scenarios)
  })

  #make raster
  map_raster <- reactive({
    
    map_raster <- make_shock_raster_shiny(scenario_map_data(), input$mapshock, input$mapscenario)
    names(map_raster) <- "yield decrease"
    map_raster

  })
  
  output$maptitle <- renderText({
    title <- paste0(input$mapcrop, ", a ", input$mapscenario, "% ", input$mapshock)
    
  })
  
  #render output
   output$scenariomap <- renderLeaflet({
     
     map <- mapview(map_raster(),
             use.layer.names = TRUE,
             col.regions = viridis(n = 5, option = "magma", direction = 1),
             na.color = NA,
             alpha.regions = 1,
             maxpixels =  9331200,
             map.types = "Esri.WorldGrayCanvas",
             query.digits = 1,
             query.prefix = "%") 
     
     map@map %>%
       setView(lng=30, lat=50, zoom =2)
 
    })
  
  
  ###### Production plots ######
  
  output$productionbars <- renderPlotly({
    
    production_plot <- prod_df %>%
      mutate(across(shock, factor, levels=c("N_rate_shock","P_rate_shock","K_rate_shock",
                                            "machinery_shock", "rescaled_pesticide_sum_shock",
                                            "shock_fert_only", "shock_all"))) %>%
      mutate(across(shock, plyr::mapvalues, from = c("N_rate_shock","P_rate_shock","K_rate_shock",
                                                     "machinery_shock", "rescaled_pesticide_sum_shock",
                                                     "shock_fert_only", "shock_all"),
                    to = c("N-rate shock", "P-rate shock", "K-rate shock", "machinery shock",
                           "pesticide shock", "fertilizer shock", "all inputs shock")))%>%
      dplyr::mutate(production_decrease = ifelse(production_decrease > 0, NA, production_decrease))%>%
      ggplot(aes(x = shock, y = production_decrease, alpha = scenario_percent, fill = shock,
                 text = paste0("shock: ", shock, "\nproduction decrease: ", round(production_decrease,2),
                               "%\nshock severity: ", scenario_percent, "%")))+ #
      geom_bar(position= position_stack(reverse = T), stat = "identity")+
      #coord_cartesian(ylim= c(-0.5, 35))+
      scale_y_continuous(name = "production decrease after shock %")+
      ggtitle("Scenario shock effects on global production")+
      scale_alpha_discrete("scenario percent", range = c(1.0, 0.4))+
      scale_fill_manual(values = wes_palette("GrandBudapest1", n = 7, type = "continuous"))+
      theme_bw()+
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x = element_text(angle = 90),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none"
      )+
      facet_wrap(~crop, nrow = 3)+
      guides(alpha = guide_legend(order = 2), fill = guide_legend(order = 1))
    
    ggplotly(production_plot, tooltip = c("text"))
    
  })
  
  
   output$productionraster <- renderLeaflet({
     
     names(prod_change_raster) <- "total.yield.decrease"
     prod_change_raster[prod_change_raster > 0] <- 0    
  
     map <- mapview(prod_change_raster,
                    use.layer.names = TRUE,
                    col.regions = viridis(n = 5, option = "magma", direction = 1),
                    na.color = NA,
                    alpha.regions = 1,
                    maxpixels =  9331200,
                    map.types = "Esri.WorldGrayCanvas",
                    query.digits = 1,
                    query.prefix = "%") 
     
     map@map %>%
       setView(lng=30, lat=50, zoom =2)
  
   })
  
  output$productioncountries <- renderTmap({

    tm_shape(World_sp)+
      tm_polygons("prod_change",
                  style="cont",
                  palette = viridis(n = 5, option = "magma", direction = -1),
                  breaks = seq(-75, 0, 25),
                  legend.reverse = FALSE,
                  title = "decrease %")+
      tm_view(set.view = c(30,50,2))
      # tm_layout(main.title = "Change in production in all 12 crops after 50% shock in all inputs",
      #           main.title.size = 1,
      #           bg.color = "white",
      #           legend.show = TRUE,
      #           legend.outside = TRUE,
      #           #legend.outside.position = c("left", "bottom"),
      #           earth.boundary = c(-180, 180, -90, 90),
      #           earth.boundary.color = "gray",
      #           frame = FALSE)
  })
  

  ###### Performance plots ######
  
  # NSE plot, data loaded already
  
  output$NSE <- renderPlot({

    NSE_total_df$ones <- rep((rep(seq(1, 5, by= 1), 5)), 1)
    NSE_total_df$tens <- rep((rep(seq(00, 20, by = 5), each= 5)), 1)

    NSE_pivot <- NSE_total_df %>%
      dplyr::select(-bin)%>%
      pivot_longer(c(1:12), names_to = "crop", values_to = "NSE")

    NSE_plot <- NSE_pivot %>%
      ggplot(aes(ones, tens)) +
      geom_tile(aes(fill = NSE)) +
      geom_text(aes(label = round(NSE, 2)), size = 2)+
      xlab(paste("precipitation \u2192"))+
      ylab(paste("temperature \u2192")) +
      scale_fill_viridis(option = "cividis", direction = 1, name = "NSE", limits = c(0,1))+
      ggtitle(paste0("NSE scores")) +
      theme(axis.text.x = element_text(angle=90))+
      theme_classic()+
      theme(axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            strip.background = element_blank()) +
      facet_wrap(~crop, nrow = 3)
    
    NSE_plot

    #ggplotly(NSE_plot)

  })
  
  
  RMSE_data <- reactive({
    file_name <- paste0("data/RMSE/", input$RMSE, "_RMSE.RData")
    get(load(file_name))
    
  })
  
  
  output$RMSE <- renderPlot({
    
    
    RMSE_data() %>%
      ggplot(aes(ones, tens)) +
        geom_tile(aes(fill = RMSE)) + 
        geom_text(aes(label = paste0(round(RMSE, 2), "±", round(sd_value, 3)))) +
        scale_fill_viridis(option = "cividis", direction = -1) +
        xlab(paste("precipitation \u2192"))+
        ylab(paste("temperature \u2192")) +
        theme_classic()+
        theme(axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            strip.background = element_blank()) +
        facet_wrap(~traintest, nrow = 1)
    
    #ggplotly(RMSE_plot)
    
    
  })
  
  ###### ALE-plots ######
  
  ALE_data <- reactive({
    file_name <- paste0("data/ALEs/", input$ALE, "_ALEs.RData")
    get(load(file_name))
    
  })
  
  
  bin_ALE_data <- reactive({
    
    ALE_data() %>%
      dplyr::filter(bin == input$ALEbin)
    
  })
  
  output$aleplot <- renderPlot({
    
    
    if (input$wholecropALE) {
    
      plot_all_loess_ales(ALE_data(), input$ALE)
      
    } else {
      
      plot_all_ales(bin_ALE_data(), input$ALE, input$ALEbin)
      
    }
    
  })
  
  
}

##### Run the application #####
shinyApp(ui = ui, server = server)
