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
  windowTitle= tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logos/ANICHO_minilogo.png"),
    tags$title("ANICHO")
  ),
  # windowTitle = "AZTI VAPEM",
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
      p("Lo que se quiera incluir en este layout.")
    ) # to customize as desire
  ), # close home
  
  # --------------
  # Following Tab panels 
  # --------------
  
  tabPanel(title = "Figure 3", 
           value = "fig03",
           fig03panel(),
  ), # close Figure 3
  
  tabPanel(title = "Figure 6", 
           value = "fig06",
           fig06panel(),
  ), # close Figure 6
  
  tabPanel(title = "Figure 8", 
           value = "fig08",
           fig08panel(),
  ), # close Figure 8
  
  tabPanel(title = "ABOUT", 
           value = "about"
  ) # close about
  
 ) # close navBar page
)  # close shinyUI
