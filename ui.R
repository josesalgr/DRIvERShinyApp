
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
          "Indicators", 
          tags$i(
            class = "glyphicon glyphicon-info-sign", 
            style = "color:#FFBF00;",
            title = "Select the indicator and specify the percentage of the total available amount (target)  to include in the final solution"
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
            HTML('<p style="text-align: justify;"> River networks are among Earth’s most threatened hot-spots of biodiversity and are essential for human well-being. However, climate change and increased human water use are causing more rivers and streams to dry, but these drying river networks (DRNs) have received little attention. DRYvER is a Horizon 2020 project, which aims to collect, analyse and model data from nine DRNs in Europe and South America to create a novel global meta-system approach that incorporates hydrology, socio-economics, ecology and biogeochemistry in order to craft strategies, tools, guidelines, and recommendations for adaptive management of river networks in the EU and worldwide. More information is available on the <a href="https://www.dryver.eu/" target="_blank">DRYvER web-page</a>.'),
            div(style = "margin-top: 20px;"),
            HTML('<center><img src="albarine-river-intro.png" width="60%"></center>'),
            div(
              class="footer",
              includeHTML("footer.html"),
              br(),
              HTML(paste0('<div style="bottom:0;font-size:12px;position: absolute;right: 0;"> Version ',{Sys.getenv("APP_VERSION")},' </div>'))
            ),
            div(style = "margin-top: 20px;"),
            HTML('<b>Links of interest: </b> <br />'),
            HTML('<a href="https://dryver-hydro.sk8.inrae.fr/" target="_blank">DRYvER-Hydro application</a> <br />'),
            HTML('<br /> <br />'),
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
            h1("DRYvER-OptimApp Application"),
            HTML('<p style="text-align: justify;"> This application is a collaborative effort aimed at identifying and prioritizing critical areas for restoration and conservation of Drying River Networks (DRNs). It is part of the DRYvER project, funded by the European Union’s Horizon 2020 research and innovation programme under grant agreement No 869226. It involves multiple esteemed institutions including FEHM-Lab at the University of Barcelona, IRBio, EBD-CSIC, IRTA Marine and Continental Waters Programme, and INRAE UR RiverLy.'),
            div(style = "margin-top: 10px;"),
            HTML('<p style="text-align: justify;"> The main objective of the DRYvER-OptimApp is to provide a tool to locate and prioritize critical areas for maintaining significant values including biodiversity values, ecological functions and ecosystem services in the six European DRN that are case studies of DRYvER project: Albarine (France), Bükkösdi-viz (Hungary), Butižnica (Croatia), Genal (Spain), Lepsämänjoki (Finland), and Velička (Czech Republic). It also enables classification of identified priority areas based on their suitability for conservation or restoration purposes. DRYvER-OptimApp allows users to visualize all inputs, aggregate it for the prioritization process, and determine the amount. All river networks are divided into river sections (reaches).'),
            div(style = "margin-top: 10px;"),
            HTML('<p style="text-align: justify;"> There are two projections available for the six European river networks: one based on the year of sampling (2021) and another under climate change and shared socioeconomic pathways scenarios (2040-2070). Each projection was separated into six time periods, coinciding with the time between the sampling campaigns developed in 2021. All information was translated to the river sections (reaches).'),
            div(style = "margin-top: 10px;"),
            HTML("Three types of <b>inputs</b> (called indicators) can be visualized in the <i>Data exploration </i> section (Figure 1) and selected for prioritization in the <i>Optimization section </i> (Figure 2). The indicators available are divided into biodiversity indicators, ecological functions, and ecosystem services. Here is a short description of each of them:"),
            div(style = "margin-top: 10px;"),
            HTML("<b>1) Biodiversity metrics:</b> analyzed for the macroinvertebrate communities :"),
            HTML("<ul><li><b>Functional Diversity</b>: Indicating the diversity of functional traits.</li>
      <li><b>Functional Richness</b>: Indicating the variety of functional traits within macroinvertebrate communities.</li>
      <li><b>Local Contribution to Beta Diversity</b>: Indicating the variation in species composition between different reaches within the same DRN.</li>
      <li><b>Temporal Beta Diversity</b>: Indicating changes in species composition over time, analyzed through variation partitioning results of separate sampling campaigns.</li></ul>"),
            div(style = "margin-top: 10px;"),
            HTML("<b>2) Ecosystem Functions:</b> Daily calculations that were average across the six campaigns for the two projections."),
            HTML("<ul><li><b>CO2 sequestration</b>: Daily total emissions of CO2 considering both emissions from flowing water and dry riverbed divided by reach area (wetted area + dry area): g C-CO2 m-2</li>
      <li><b>Leaf litter decomposition rate</b>: Average decomposition rate: degree-day-1</li></ul>"),
            div(style = "margin-top: 10px;"),
            HTML("<b>3) Ecosystem Services:</b>"),
            HTML("<ul><li><b>Drought Regulation</b>: Assessed using local recharge and surface storage indices.</li>
      <li><b>Erosion Regulation</b>: Assessed both on slopes and floodplains.</li>
      <li><b>Flood Regulation</b>: Assessed both on slopes and floodplains.</li></ul>"),
            div(style = "margin-top: 10px;"),
            HTML('<figure><left><img src="Figure1.png" width="80%"></left><figcaption>Figure 1. Example of the <i>Data exploration</i> tab, visualization of the Predicted functional richness in the Butižnica (Croatia) catchment in the current (2021) time period. To view the upper-left graph, click on a specific river reach.</figcaption></figure>'),
            div(style = "margin-top: 10px;"),
            h2("Prioritization"),
            HTML('<p style="text-align: justify;"> The objective of the planning exercise that can be run with DRYvER-OptimApp is to identify reaches that are important for both time periods that correctly represents the indicators selected in the amount selected. Then, based on the prioritization exercise, a conservation/restoration proposal is made.'),
            div(style = "margin-top: 10px;"),
            HTML('<p style="text-align: justify;"> The DRYvER-OptimApp internally uses the R package <a href="https://prioritizr.net/" target="_blank">prioritizr</a> to run the optimization analysis. Prioritizr utilizes mixed integer linear programming (MILP) to offer a versatile framework for formulating and solving conservation planning problems. Through Prioritizr, DRYvER-OptimApp allows users to create customized conservation strategies that are optimized based on various constraints and objectives. Constraints that can be applied are related to the aggregation of the selected reaches we want to achieve, and the amount of each feature we want to include in the final solution:'),
            HTML("<ul><li><b>Selected indicators</b>: in this tab, users can select which features (among ecosystem services, biodiversity, and ecological values) to include in the analysis, and in what amount.</li>
      <li><b>Aggregation</b>: This is a typical parameter used in Systematic Conservation Planning and it is used for weighting the degree of aggregation you want to achieve in your final solution.</li></ul>"),
            div(style = "margin-top: 10px;"),
            div(style = "margin-top: 10px;"),
            HTML('<figure><left><img src="Figure2.png" width="80%"></left><figcaption>Figure 2. Example of the <i>optimization</i> tab, , after clicking the bottom “Optimize” with the satellite background in the Butiznica (Croatia) catchment. Red arrows indicate the Optimization tab, the bar for adjusting aggregation and the Optimize bottom. The upper box shows three examples of how solutions change when using different aggregation values.</figcaption></figure>'),
            h3("Conservation/Restoration proposal"),
            HTML('<p style="text-align: justify;"> The <b>frequency of selection</b> for each river reach indicates the number of campaigns where the reach was chosen during the optimization process. This frequency can be visualized after running the prioritization analysis (i.e., pressing the bottom “Optimize”). The selection frequency is displayed on a scale from 1 to 12, corresponding to the six campaigns conducted during the two time periods. If the legend does not extend to the value of 12, it indicates that no reaches were selected in all campaigns across both time periods (Figure 3).  The DRYvER-OptimApp also shows <b>trends</b> in ecosystem services, biodiversity, and ecological values between 2021 and 2060, identifying reaches with positive or negative trends. Finally, it highlights reaches <b>value for conservation</b> (positive trend, and at least 1 frequency of selection) or <b>value for restoration</b> (negative trend and at least 1 frequency of selection). The <b>flow regime</b> of each reach can also be visualized: perennial, intermittent, or newly intermittent in the future.'),
            div(style = "margin-top: 10px;"),
            HTML("**All inputs used here were sampled and developed by DRYvER partners. More details on the indicators, the sampling protocol and model development, please visit: <a href='https://www.dryver.eu/' target='_blank'>DRYvER web-page</a>."),
            div(style = "margin-top: 10px;"),
            HTML("**For more details of the DRYvER-OptimApp development and usability are available in the Deliverable and <a href='https://www.dryver.eu/' target='_blank'>manual</a>."),
            div(style = "margin-top: 10px;"),
            HTML('<figure><left><img src="Figure3.png" width="80%"></left><figcaption>Figure 3. Overview of a solution after clicking the bottom Optimize for the Butiznica (Croatia) catchment. To view the upper-left graph, click on a specific river reach.</figcaption></figure>'),
            div(style = "margin-top: 30px;"),
            HTML("<b>Contributors to DRYvER-OptimApp:</b><br />"),
            HTML("<b>Application developpers</b> | José Salgado-Rojas (jose.salgroj@gmail.com), Mónica Lanzas<br />"),
            HTML("<b>Analysts</b> | Núria Bonada, Virgilio Hermoso, Núria Cid<br />"),
            # HTML("<b>Future climate modelling</b> | Alexandre Devers, Claire Lauvernet, Jean-Philippe Vidal<br />"),
            # HTML("<b>Indicators analyses</b> | Annika Künne, Sven Kralisch, Louise Mimeau<br />"),
            # HTML("<b>Observed flow intermittence data collection</b> | Thibault Datry, Bertrand Launay, Amélie Truchy (Albarine, France), Zoltán Csabai, Bálint Pernecker (Bükkösdi, Hungary),
            #      Marko Miliša, Luka Polovic (Butižnica, Croatia), Amaia Angulo Rodeles, Nuria Bonada, Nuria Cid, Maria Soria (Genal, Spain), 
            #      Heikki Mykrä, Henna Snåre (Lepsämänjoki, Finland), Petr Pařil (Velička, Czech Republic)."),
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

