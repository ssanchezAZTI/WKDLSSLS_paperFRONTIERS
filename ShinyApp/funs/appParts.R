#==============================================================================
# Figure 3                                                                 ----
#==============================================================================

fig03panel <- function() {

  fluidPage(
    titlePanel("Calendar effect"),

    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        checkboxGroupInput("id03", "Indicator(s):",
                           choiceNames = perflabels, choiceValues = perfnms,
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
        h4("Figure settings"), 
        selectInput("idpos03",
          label = "Select STKN and fhist positions:",
          choices = c("STKN_FHIST in cols","STKN_FHIST in rows",
                      "STKN in cols & FHIST in rows", "STKN in rows & FHIST in cols"),
          selected = "STKN_FHIST in cols",
          multiple = FALSE
        ), 
        width = 3
      ),
      mainPanel( # "main panel",
        plotOutput("plot03", height = "575px"),
        uiOutput("text03"),
        # fluidRow( tags$b("Figure 3 (interactive version)."),
        #   textOutput("text03") ),
        width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}



#==============================================================================
# Figure 6                                                                 ----
#==============================================================================

fig06panel <- function() {
  
  fluidPage(
    titlePanel("Trajectories"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        selectInput("id06", "Indicator:",
                    choices = perfnms,
                    selected = "Risk3.Blim",
                    multiple = FALSE
        ),
        checkboxGroupInput("stkn06", "Stock type:",
                           choiceNames = STKNnms, choiceValues = STKNnms,
                           selected = STKNnms,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist06", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms), FHISTnms),
                           selected = FHISTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt06", "Rule (n-over-m):",
                           choices = setNames(as.list(HCRTnms), HCRTnms),
                           selected = HCRTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("uc06", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms), UCnms),
                           selected = UCnms, 
                           inline = TRUE
        ),
        h4("Figure settings"), 
        selectInput("idpos06",
                    label = "Select STKN and fhist positions:",
                    choices = c("STKN_FHIST in rows","STKN_FHIST in cols",
                                "STKN in cols & FHIST in rows", "STKN in rows & FHIST in cols"),
                    selected = "STKN_FHIST in rows",
                    multiple = FALSE
        ), 
        width = 3
      ),
      mainPanel( # "main panel",
        plotOutput("plot06", height = "575px"),
        uiOutput("text06"),width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}


 
#==============================================================================
# Figure 8                                                                 ----
#==============================================================================

fig08panel <- function() {
  
  fluidPage(
    titlePanel("Risks vs. relative yields"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        checkboxGroupInput("stkn08", "Stock type:",
                           choiceNames = STKNnms, choiceValues = STKNnms,
                           selected = STKNnms,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist08", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms), FHISTnms),
                           selected = FHISTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("term08", "Term:",
                           choices = setNames(as.list(TERMnms), TERMnms),
                           selected = c("short","long"), #TERMnms 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt08", "Rule (n-over-m):",
                           choices = setNames(as.list(HCRTnms), HCRTnms),
                           selected = HCRTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("uc08", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms), UCnms),
                           selected = UCnms[!UCnms%in% c("(0.2,0.2)", "(0.2,0.25)", "(0.5,1)", "(0.5,1.5)")], 
                           inline = TRUE
        ),
        h4("Figure settings"), 
        selectInput("idpos08",
                    label = "Select STKN and fhist positions:",
                    choices = c("STKN_FHIST in cols","STKN_FHIST in rows",
                                "STKN in cols & FHIST in rows", "STKN in rows & FHIST in cols"),
                    selected = "STKN_FHIST in cols",
                    multiple = FALSE
        ), 
        width = 3
      ),
      mainPanel(
        plotOutput("plot08", height = "575px"),
        uiOutput("text08"),
        width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}   

