#   ---------------------- #
#   Tab1
#   ---------------------- #

tab1panel <- function() {

  fluidPage(
    titlePanel("title panel"),
    
    sidebarLayout(
      sidebarPanel(
        "sidebar panel",
        varSelectInput(
          "X_Axis",
          label = "Select Variable 1",
          data = sc.dat,
          selected = "STKN" # selecting one
        ),
        varSelectInput(
          "Col",
          label = "Select Variable Colour",
          data = sc.dat,
          selected = "HCRT" # selecting one
        )
      ),
      mainPanel(
        "main panel",
        plotOutput("plot1")
      )
    ) # close sidebarLayout
  ) # close fluidpage      
}

#   ---------------------- #
#   Tab2  
#   ---------------------- #

tab2panel <- function() {
  tagList(
    div(
      h1("Tab2"),
      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed eiusmod tempor incidunt ut labore et dolore 
                magna aliqua."),
      hr(),
      br()
    )
  )
  
}
    

