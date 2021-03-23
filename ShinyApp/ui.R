#==============================================================================
# UI code (general)                                                        ----
#==============================================================================

shinyUI(navbarPage(
  
  # ------------------------
  # Navigation bar and logo
  # ------------------------
  
  title = tags$img(src = "logos/AZTI_logo_Trans.png", height = "38px"),
  id = "navBar",
  theme = shinytheme('lumen'), # aldatu leike
   # collapsible = TRUE,
  fluid = TRUE,
  inverse = TRUE,
  # windowTitle= tags$head(
  #   tags$link(rel = "icon", type = "image/png", href = "logos/ANICHO_minilogo.png"),
  #   tags$title("Adapting simple index-based catch rules for data-limited stocks to short-lived fish stocks’ characteristics")
  # ),
  windowTitle = "AZTI VAPEM",
  position = "fixed-top",
  #footer = includeHTML("./www/footer.html"), # twitter, disclaimer etc.
  header = tags$style(
             ".navbar-right { float: right !important;}",
             "body {padding-top: 75px;}"),

  
  # --------------
  # HOME Tab panel 
  # --------------
  
  tabPanel(title = introBox(icon("home")), 

    shinyjs::useShinyjs(),
    includeCSS("./www/style/style.css"), 
    # includeCSS("https://www.azti.es/wp-content/themes/azti/ui/dist/style/azti.bundle.css?ver=1.0.1"),
    includeHTML('./www/landingBanner.html'), # landing banner
    #Theme buttons
    fluidRow( 
      br(), br(),
      align = "center",
      p("Interactive version of the figures in the above publication.")
    ) # to customize as desire
  ), # close home
  
  # --------------
  # Following Tab panels 
  # --------------
  
  tabPanel(title = "Figure 2", 
           value = "fig02",
           fig02panel(),
  ), # close Figure 2
  
  tabPanel(title = "Figure 3", 
           value = "fig03",
           fig03panel(),
  ), # close Figure 3
  
  tabPanel(title = "Figure 4", 
           value = "fig04",
           fig04panel(),
  ), # close Figure 4
  
  tabPanel(title = "Figure 5", 
           value = "fig05",
           fig05panel(),
  ), # close Figure 5
  
  tabPanel(title = "Figure 6", 
           value = "fig06",
           fig06panel(),
  ), # close Figure 6
  
  tabPanel(title = "Figure 7", 
           value = "fig07",
           fig07panel(),
  ), # close Figure 7
  
  tabPanel(title = "Figure 8", 
           value = "fig08",
           fig08panel(),
  ), # close Figure 8
  
  tabPanel(title = "Figure 9", 
           value = "fig09",
           fig09panel(),
  ), # close Figure 9
  
  tabPanel(title = "Figure 10", 
           value = "fig10",
           fig10panel(),
  ), # close Figure 10
  
  tabPanel(title = "Figure 11",
           value = "fig11",
           fig11panel(),
  ), # close Figure 11
  
  tabPanel(title = "Figure 12",
           value = "fig12",
           fig12panel(),
  ), # close Figure 12
  
  tabPanel(title = "Figure 13",
           value = "fig13",
           fig13panel(),
  ), # close Figure 13
  
  tabPanel(title = "ABOUT",
           value = "about"
  ) # close about
  
 ) # close navBar page
)  # close shinyUI
