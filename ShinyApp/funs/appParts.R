#==============================================================================
# Figure 3                                                                 ----
#==============================================================================

fig03panel <- function() {
  
  fluidPage(
    titlePanel("Calendar effect"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        # varSelectInput(
        #   "X_Axis",
        #   label = "Select Variable 1",
        #   data = df_bc,
        #   selected = "STKN" # selecting one
        # ),
        # varSelectInput(
        #   "Col",
        #   label = "Select Variable Colour",
        #   data = df_bc,
        #   selected = "HCRT" # selecting one
        # ),
        checkboxGroupInput("id03", "Indicator(s):",
                           choiceNames = perflabels, choiceValues = perfnms,
                           # choices = setNames(as.list(perfnms), perflabels),
                           selected = perfnms,
                           inline = TRUE
        ),
        checkboxGroupInput("stkn03", "Stock type:",
                           choiceNames = STKNnms, choiceValues = STKNnms,
                           selected = STKNnms,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist03", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms), FHISTnms),
                           selected = FHISTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("term03", "Term:",
                           choices = setNames(as.list(TERMnms), TERMnms),
                           selected = c("short","long"), #TERMnms 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt03", "Rule (n-over-m):",
                           choices = setNames(as.list(HCRTnms), HCRTnms),
                           selected = HCRTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("uc03", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms), UCnms),
                           selected = UCnms, 
                           inline = TRUE
        ),
        width = 2
      ),
      mainPanel(
        # "main panel",
        plotOutput("plot03", height = "600px"), #, width = "100%", height = "100%"
        # textOutput("text03"),textOutput("text03a"),textOutput("text03b"),
        # textOutput("text03c"),textOutput("text03d"),textOutput("text03e"),
        width = 10
        # tableOutput("table03")
      )
    ) # close sidebarLayout
  ) # close fluidpage      
}

#==============================================================================
# Figure 3b                                                                 ----
#==============================================================================

fig03bpanel <- function() {

  fluidPage(
    titlePanel("Calendar effect"),

    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        checkboxGroupInput("id03b", "Indicator(s):",
                           choiceNames = perflabels, choiceValues = perfnms,
                           selected = perfnms,
                           inline = TRUE
        ),
        checkboxGroupInput("stkn03b", "Stock type:",
                           choiceNames = STKNnms, choiceValues = STKNnms,
                           selected = STKNnms,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist03b", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms), FHISTnms),
                           selected = FHISTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("term03b", "Term:",
                           choices = setNames(as.list(TERMnms), TERMnms),
                           selected = c("short","long"), #TERMnms 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt03b", "Rule (n-over-m):",
                           choices = setNames(as.list(HCRTnms), HCRTnms),
                           selected = HCRTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("uc03b", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms), UCnms),
                           selected = UCnms, 
                           inline = TRUE
        ),
        h4("Figure settings"), 
        selectInput("idpos03b",
          label = "Select STKN and fhist positions:",
          choices = c("STKN_fhist in cols","STKN_fhist in rows",
                      "STKN in cols & fhist in rows", "STKN in rows & fhist in cols"),
          selected = "STKN_fhist in cols",
          multiple = FALSE
        ),
        width = 2
      ),
      mainPanel( # "main panel",
        plotOutput("plot03b", height = "600px"),
        textOutput("text03b"),
        width = 10
      )
    ) # close sidebarLayout
  ) # close fluidpage
}


    

