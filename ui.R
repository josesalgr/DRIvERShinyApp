library(leaflet)
library(shinyjs)
#library(highcharter)
library(htmlwidgets)

#navbarPage(title=div(img(src="https://prioriactions.github.io/prioriactions/reference/figures/logo.png", width = '30px'),"prioriactions"), id="nav",
#navbarPage(title="prioriactions", id="nav",
fluidPage(           
    div(class="outer",

      tags$head(
        tags$style(HTML("
          .leaflet-left .leaflet-control{
          visibility: hidden;
          }
      ")),
        
        tags$script(HTML(
          '
          window.LeafletWidget.methods.setStyle = function(category, layerId, style){
            var map = this;
            if (!layerId){
              return;
            } 
            else if (!(typeof(layerId) === "object" && layerId.length)){ 
              // in case a single layerid is given
              layerId = [layerId];
            }
          
            //convert columnstore to row store
            style = HTMLWidgets.dataframeToD3(style);
            
            //console.log(style);
            layerId.forEach(function(d,i){
            var layer = map.layerManager.getLayer(category, d);
            if (layer){ // or should this raise an error?
              layer.setStyle(style[i]);
            }
          });
        };
        ')),
        # Include our custom CSS
        includeCSS("styles.css"),
        #includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      ##########################################################################
      #transparency bar
      ##########################################################################
      absolutePanel(bottom = "0%", left = "50%",
                    useShinyjs(), 
                    shinyjs::disabled(
                      sliderInput("range", NULL, min = 0, max = 1,
                                  value = 1, step = 0.1
                      )
                    ),
      ),

      ##########################################################################
      # Left bar
      ##########################################################################
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 20, left = 0, right = "auto", bottom = "auto",
        width = 450, height = "auto",

        column(12, tabsetPanel(id="plot_tabs", 
          # First panel
          tabPanel("Data explorer",
                 fluidRow(
                   useShinyjs(), 
                   div(img(src= myImgResources[1], width = '60px'),
                       img(src= myImgResources[2], width = '250px')),
                   
                   #h3("Conservation Management Visualizer"),
                   #h6("v0.1"),
                   HTML("<span style='font-size: 23px;'>Conservation Planning Visualizer</span>v0.1</span>"),
                   
                   h3(""),
  
                   selectInput("filemap", "Choose study:", width = 400, 
                               choices = list(`Shapes` = list("",
                                                              "Albarine", 
                                                              "Bukkosdi",
                                                              "Butiznica",
                                                              "Genal",
                                                              "Lepsamaanjoki",
                                                              "Velicka")),
                               selected = ""
                   ),
                   
                   shinyjs::disabled(
                     selectInput("specific_plot", "Filtered map:", width = 400,
                                 choices = list(`Costs` = list(""),
                                                `Features distribution` = list(""))
                     )
                   ),
                   
                 ),
          ),
          # Second panel
          tabPanel("Running", 
                   fluidRow(
                     useShinyjs(), 
                     
                     shinyjs::disabled(
                       h3("Step 1: Validate inputs"),
                       
                       actionButton("validate", "Validate"),
                       verbatimTextOutput("step1"),
                       
                       h3("Step 2: Create model"),
                       sliderInput("target", NULL, min = 0, max = 1,
                                   value = 1, step = 0.1
                       ),
                       actionButton("create", "Create"),
                       
                       verbatimTextOutput("step2"),
                       
                       h3("Step 3: Run the model"),
                       
                       actionButton("run", "Run!"),
                       verbatimTextOutput("step3"),
                       tags$head(tags$style("#step3{overflow-y:scroll; max-height: 500px; background: ghostwhite;}"))
                     )
                   )
          ),
          # Thirth panel
          tabPanel("Explore solutions",
                   fluidRow(
                     
                     useShinyjs(), 
                    
                       h3("Explore solutions"),
                       shinyjs::disabled(
                       fileInput(inputId = "solutionsImport",
                                 label="Import solution data",
                                 buttonLabel=list(icon("folder"),""),
                                 multiple = TRUE,
                                 accept =  c(".txt", ".dat", ".csv"),
                                 width = 600),
                        
                       selectInput("solution_name", "Solution name:", width = 600,
                                     choices = list("")),
                                     
                       selectInput("solution_plot", "Field:", width = 600,
                                     choices = list(`Costs` = list(""),
                                                    `Features distribution` = list(""),
                                                    `Threats distribution` = list(""))),
                       
                       downloadButton("downloadData", "Download")
                       #shinyjs::hidden(
                        #div(highchartOutput("pie_features_sol", width = 400, height = 320), align = "center"),
                        #div(highchartOutput("pie_threats_sol", width = 400, height = 320), align = "center")
                       #)
                      )
                   )
          ),
        ))
      ),
    ),
  conditionalPanel("false")
)