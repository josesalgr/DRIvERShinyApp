
# Define UI ----
header <- dashboardHeader(title = img(src = "DRYvER-Logo-1.png", height = 45, align = "left"))

sidebar <- dashboardSidebar(
  width = 290, collapsed = FALSE, 
  sidebarMenu(id = "tabs",
    menuItem("General", tabName = "tab_main", icon = icon("home", lib = "glyphicon"),
             menuSubItem("About the DRYvER project","tab_dryver"),
             menuSubItem("DRYvER-OptimApp","tab_dryver-OptimApp")
    ),
    menuItem("DRYvER case studies", tabName = "tab_drns", icon = icon("map-marker", lib = "glyphicon")),
    menuItem("Data exploration", tabName = "tab_map", icon = icon("picture", lib = "glyphicon")),
    menuItem("Optimization", tabName = "tab_opt", icon = icon("cog", lib = "glyphicon")),
    
    hr(),
    
    conditionalPanel(
      condition = "input.tabs == 'tab_map'",
      
      tags$p(tags$strong("Please select entries to visualize data"), style = "color: white; margin-left: 16px;"),
      
      selectInput("drn_map", label = tags$span(
        "River network", 
        tags$i(
          class = "glyphicon glyphicon-info-sign", 
          style = "color:#FFBF00;",
          title = "Select the river network for analysis in the chosen region"
        )),
        choices = drns_long,
        selected = " "),
      
      pickerInput(
        inputId = "Variable",
        label = tags$span(
                    "Indicator",
                    tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#FFBF00;",
                    title = "Choose the indicator to display on the map"
                    )),
        choices = list('Aquatic Macroinvertebrates biodiversity' = variables_long[2:5],
                       'Ecological Functions' = variables_long[6:7],
                       'Ecosystem Services' = variables_long[8:13]),
        selected = " "
      ),
      
      shinyjs::disabled(
        selectInput("Scale", 
                    label = tags$span(
                      "Timescale", 
                      tags$i(
                        class = "glyphicon glyphicon-info-sign", 
                        style = "color:#FFBF00;",
                        title = "Select between current data (2021) or future projections"
                      )),
                    choices = scale_list)),

      absolutePanel(bottom = "2%", left = "40%", width = "100%", 
                    downloadButton("downloadData", "", style = "color: black;"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'tab_opt'",
      
      tags$p(tags$strong(HTML("Please select data and preferences <br/> to optimize")), style = "color: white; margin-left: 16px;"),

      selectInput("drn_opt", label = tags$span(
        "River network", 
        tags$i(
          class = "glyphicon glyphicon-info-sign", 
          style = "color:#FFBF00;",
          title = "Select the river network for analysis in the chosen region"
        )),
        choices = drns_long,
        selected = " "),
      
      theme = bslib::bs_theme(5),
      # make sure that js and css are set and added
      tags$head(tags$style(css)),
      tags$head(tags$script(src = "js/index.js")),
      
      weightedPickerInput(
        id = "features",
        label = tags$span(
          "Features", 
          tags$i(
            class = "glyphicon glyphicon-info-sign", 
            style = "color:#FFBF00;",
            title = "Choose the variables to incorpore in the mathamtical optimization"
          )),
        choices = list('Aquatic Macroinvertebrates biodiversity' = variables_long[2:5],
                       'Ecological Functions' = variables_long[6:7],
                       'Ecosystem Services' = variables_long[8:13]),
        selected = " "
      ),

      noUiSliderInput(
        inputId = "blm", 
        label = tags$span(
          "Aggregation", 
          tags$i(
            class = "glyphicon glyphicon-info-sign", 
            style = "color:#FFBF00;",
            title = "Choose the agreggation level of the solution"
        )),
        min = 0, max = 10,
        value = 0, tooltips = TRUE,
        step = 0.01, orientation = "horizontal",
        color = "forestgreen", inline = FALSE,
        format = wNumbFormat(decimals = 2),
        height = "15px", width = "250px"
      ),
      
      # Warning box at the end
      tags$div(
        class = "alert alert-warning",
        style = "margin-top: 20px;",
        tags$strong("Warning: "),
        HTML("Setting a high aggregation value <br/> may significantly increase computation <br/> times.")
      ),
      
      absolutePanel(bottom = "5%", left = "15%",

      shinyjs::disabled(
        loadingButton(
          "optimize", 
          label = "Optimize",
          style = "background-color: forestgreen; border-color: forestgreen; font-size: 16px;",
          loadingLabel = "Optimizing...",
        )
      ),
      shinyjs::disabled(
        downloadButton("downloadPDF", "PDF")
      )
      ))
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS("body > div > header > nav > a {visibility: hidden}"),
  tabItems(
    tabItem(tabName = "tab_dryver",
            h1("About the DRYvER project: Drying rivers and climate change"),
            HTML('River networks are among Earth’s most threatened hot-spots of biodiversity and are essential for human well-being. However, climate change and increased human water use are causing more rivers and streams to dry, but these drying river networks (DRNs) have received little attention. DRYvER is a Horizon 2020 project, which aims to collect, analyse and model data from nine DRNs in Europe and South America to create a novel global meta-system approach that incorporates hydrology, socio-economics, ecology and biogeochemistry in order to craft strategies, tools, guidelines, and recommendations for adaptive management of river networks in the EU and worldwide. More information is available on the <a href="https://www.dryver.eu/" target="_blank">DRYvER web-page</a>.'),
            div(style = "margin-top: 20px;"),
            HTML('<center><img src="albarine-river-intro.png" width="60%"></center>'),
            div(
              class="footer",
              includeHTML("footer.html"),
              br(),
              HTML(paste0('<div style="bottom:0;font-size:12px;position: absolute;right: 0;"> Version ',{Sys.getenv("APP_VERSION")},' </div>'))
            ),
            div(style = "margin-top: 20px;"),
            HTML("Reference:<br />"),
            HTML('Datry T, Allen D, Argelich R, Barquin J, Bonada N, Boulton A, Branger F, Cai Y, Cañedo-Argüelles M,
Cid N, Csabai Z, Dallimer M, de Araújo JC, Declerck S, Dekker T, Döll P, Encalada A, Forcellini M, Foulquier A,
Heino J, Jabot F, Keszler P, Kopperoinen L, Kralisch S, Künne A, Lamouroux N, Lauvernet C, Lehtoranta V,
Loskotová B, Marcé R, Martin Ortega J, Matauschek C, Miliša M, Mogyorósi S, Moya N, Müller Schmied H,
Munné A, Munoz F, Mykrä H, Pal I, Paloniemi R, Pařil P, Pengal P, Pernecker B, Polášek M, Rezende C,
Sabater S, Sarremejane R, Schmidt G, Senerpont Domis L, Singer G, Suárez E, Talluto M, Teurlincx S,
Trautmann T, Truchy A, Tyllianakis E, Väisänen S, Varumo L, Vidal J-P, Vilmi A, Vinyoles D (2021) <b>Securing
Biodiversity, Functional Integrity, and Ecosystem Services in Drying River Networks (DRYvER)</b>. Research Ideas
and Outcomes 7: e77750. <a href="https://doi.org/10.3897/rio.7.e77750" target="_blank">https://doi.org/10.3897/rio.7.e77750</a>')
    ),
    
    tabItem(tabName = "tab_dryver-OptimApp",
            h1("DRYvER-OptimApp application"),
            HTML("As part of the DRYvER project, a spatial hydrological model for simulating the flow intermittence in river networks was developed and implemented on <b>6 European river networks</b> (see the Modelling method tab)."),
            HTML("Additionally, the <b>DRYvER-OptimApp</b> is an application designed for the optimization of conservation and restoration of biodiversity in these river networks. It leverages the results of flow intermittence modelling to identify key areas and strategies for maintaining and improving biodiversity."),
            div(style = "margin-top: 10px;"),
            HTML("This application shows the results of <b>flow intermittence modelling</b> in the 6 studied river networks: Albarine (France), Bükkösdi (Hungary), Butižnica (Croatia), Genal (Spain), Lepsämänjoki (Finland), and Velička (Czech Republic). DRYvER-Hydro allows to explore the evolution of the spatio-temporal patterns of flow intermittence in the river networks under the <b>past-present climate</b> (1960-2021) and under <b>climate change projections</b> until 2100."),
            div(style = "margin-top: 10px;"),
            HTML("3 types of <b>indicators</b> can be displayed."),
            div(style = "margin-top: 1px;"),
            HTML('<b>Spatial flow intermittence indicators</b> giving flow condition statistics for each of the river network:'),
            div(style = "margin-top: 1px;"),
            HTML("<ul><li>conD: Number of days with dry conditions</li>
                 <li>conF: Number of days with flowing conditions</li>
                 <li>durD: Maximum number of consecutive days with dry conditions</li>
                 <li>durF: Maximum number of consecutive days with flowing conditions</li>
                 <li>numFreDr: Absolute number of drying events</li>
                 <li>numFreRW: Absolute number of rewetting events</li>
                 <li>FstDrE: Julian day of first drying event per year [1-366]</li></ul>"),
            HTML('<b>Aggregated flow intermittence indicators</b> giving general flow condition statistics for the entire river network:'),
            div(style = "margin-top: 1px;"),
            HTML("<ul><li>RelInt: Proportion of model derived river length with intermittent conditions [%]</li>
                 <li>RelFlow: Proportion of model derived river length with flowing conditions [%]</li>
                 <li>PatchC: Proportion of model-derived reach length with changing flowing and intermittent conditions compared to adjacent downstream reaches [%]</li></ul>"),
            HTML('<b>Aggregated climate indicators</b> showing the climatic characteristics of the catchment area:'),
            div(style = "margin-top: 1px;"),
            HTML("<ul><li>Temp: Air temperature [°C]</li>
                 <li>Precip: Precipitation [mm]</li>
                 <li>ET: Evapotranspiration [mm]</li></ul>"),
            div(style = "margin-top: 10px;"),
            HTML("For future projections, simulations were carried out using climate projections from <b>5 Global Climate Models (GCMs)</b> from the "),
            HTML('<a href="https://pcmdi.llnl.gov/CMIP6/" target="_blank">CMIP6 project</a>'),
            HTML(" and <b>3 Shared Socio-economic Pathways (SSPs)</b>:"),
            HTML("<ul><li>SSP1-2.6 Sustainability</li>
                 <li>SSP3-7.0 Regional rivalry</li>
                 <li>SSP5-8.5 Fossil-fuelled development</li></ul>"),
            div(style = "margin-top: 10px;"),
            HTML('<figure>
                 <left><img src="ssps_intro.png" width="40%"></left>
                 <figcaption>Global warming trajectories according to the five SSPx-y scenarios used in the <a href="https://www.ipcc.ch/report/sixth-assessment-report-working-group-i/" target="_blank">IPCC summary for decision-makers</a></figcaption>
                 </figure>'),
            div(style = "margin-top: 30px;"),
            HTML("<b>Contributors to DRYvER-OptimApp:</b><br />"),
            HTML("<b>Application developper</b> | José Salgado-Rojas (jose.salgroj@gmail.com)<br />"),
            HTML("<b>Past/present climate modelling</b> | Flora Branger, Annika Künne, Sven Kralisch, Louise Mimeau<br />"),
            HTML("<b>Future climate modelling</b> | Alexandre Devers, Claire Lauvernet, Jean-Philippe Vidal<br />"),
            HTML("<b>Indicators analyses</b> | Annika Künne, Sven Kralisch, Louise Mimeau<br />"),
            HTML("<b>Observed flow intermittence data collection</b> | Thibault Datry, Bertrand Launay, Amélie Truchy (Albarine, France), Zoltán Csabai, Bálint Pernecker (Bükkösdi, Hungary),
                 Marko Miliša, Luka Polovic (Butižnica, Croatia), Amaia Angulo Rodeles, Nuria Bonada, Nuria Cid, Maria Soria (Genal, Spain), 
                 Heikki Mykrä, Henna Snåre (Lepsämänjoki, Finland), Petr Pařil (Velička, Czech Republic)."),
    ),
    
    tabItem(tabName = "tab_drns",     
            h1("DRYvER case studies"),
            box(title = "River networks location", width=5,status = "primary", solidHeader = TRUE,
                HTML('<left><img src="europe-drns-no-names.png" width="400px" id="map"></left>'),
                div(style = "margin-top: 1px;"),
                "Click on a river network to see more information.",
                div(style = "position:absolute;left:130px;top:245px",
                    actionButton("drn_info_Albarine", "Albarine",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                div(style = "position:absolute;left:280px;top:255px",
                    actionButton("drn_info_Bukkosdi", "Bükkösdi",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                div(style = "position:absolute;left:60px;top:330px",
                    actionButton("drn_info_Genal", "Genal",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                div(style = "position:absolute;left:180px;top:320px",
                    actionButton("drn_info_Butiznica", "Butižnica",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                div(style = "position:absolute;left:250px;top:215px",
                    actionButton("drn_info_Velicka", "Velička",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                div(style = "position:absolute;left:265px;top:90px",
                    actionButton("drn_info_Lepsamanjoki", "Lepsämänjoki",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
            ),
            uiOutput("presentation_drn")
    ),
    
    tabItem(tabName = "tab_results",
            fluidRow(
              autoWaiter(html=spin_pulsar(), color = transparent(0.5)),
              uiOutput("box1"),
              uiOutput("box2"),
              uiOutput("box3"),
              uiOutput("box4")
            )
    ),
    tabItem(tabName = "tab_map",

            fluidPage(
              div(class="outer",
              tags$head(

              # Include our custom CSS
              includeCSS("styles.css"),
            ),

            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("map_data", width="100%", height="100%"),
            )),

            
            
            ##########################################################################
            #transparency bar
            ##########################################################################
            chooseSliderSkin("Flat", color = "green"),
              
            absolutePanel(bottom = "0%", right = "40%",
                          shinyjs::disabled(
                            sliderTextInput("range", label = "",
                                            choices = campaign_list,
                                            selected = campaign_list[1],
                                            grid = TRUE

                            )),
            ),
            shinyjs::disabled(
            absolutePanel(bottom = "1.5%", right = "20%",
                          shinyjs::disabled(
                          awesomeCheckbox(
                            inputId = "Compare",
                            label = "Compare",
                            value = FALSE,
                            status = "success"
                          )),
            )),
            
  
            ##########################################################################
            # Left bar
            ##########################################################################
            absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                          draggable = FALSE, top = 60, left = 290, right = "auto", bottom = 0,
                          width = 500, height = 310, 
                          
                          column(12, tabsetPanel(id="plot_tabs"),
                                 #shinyjs::hidden(
                                 #girafeOutput(outputId = "plotly1"),
                                 #),
                                 div(style = "margin-top: 0px;", plotlyOutput(outputId = "plotly2", inline = TRUE))),
                                 #plotlyOutput(outputId = "plotly2", inline = TRUE)),
            ),
    ),  
    tabItem(tabName = "tab_opt",

            fluidPage(           
              div(class="outer",
              tags$head(
                
              # Include our custom CSS
              includeCSS("styles.css"),
            ),
            
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("map_opt", width="100%", height="100%"),
            )),  
            
            ##########################################################################
            #transparency bar
            ##########################################################################
            chooseSliderSkin("Flat", color = "green"),
            
            ##########################################################################
            # Left bar
            ##########################################################################

            absolutePanel(id = "controls_opt", class = "panel panel-default", fixed = FALSE,
                          draggable = FALSE, top = 60, left = 290, right = "auto", bottom = 0,
                          width = 600, height = 310, #310
                          
                          column(12, tabsetPanel(id="plot_tabs"),
                                 div(style = "margin-top: 0px;",
                                     plotlyOutput(outputId = "plotly3", inline = FALSE))),
            ),

            absolutePanel(id = "controls_opt", class = "panel panel-default", fixed = FALSE,
                          draggable = FALSE, top = 60, left = 895, right = "auto", bottom = 0,
                          width = 360, height = 310, #310
                          
                          div(class = "center-content",
                            plotlyOutput(outputId = "plotly4", inline = FALSE)
                          )
            ),
            
            
    )
  ),
)



ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body, 
                    skin = "black")

