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



#==============================================================================
# Figure 9                                                                 ----
#==============================================================================

fig09panel <- function() {
  
  fluidPage(
    titlePanel("Risks vs. relative yields"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        checkboxGroupInput("stkn09", "Stock type:",
                           choiceNames = STKNnms, choiceValues = STKNnms,
                           selected = STKNnms,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist09", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms), FHISTnms),
                           selected = FHISTnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("term09", "Term:",
                           choices = setNames(as.list(TERMnms), TERMnms),
                           selected = c("short","long"), #TERMnms 
                           inline = TRUE
        ),
        selectInput("hcrt09", "Rule (n-over-m):",
                    choices = setNames(as.list(HCRTnms_bsafe), HCRTnms_bsafe),
                    selected = "1o2",
                    multiple = FALSE
        ),
        checkboxGroupInput("uc09", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms_bsafe), UCnms_bsafe),
                           selected = UCnms_bsafe, 
                           inline = TRUE
        ),
        h4("Figure settings"), 
        selectInput("idpos09",
                    label = "Select STKN and fhist positions:",
                    choices = c("STKN_FHIST in cols","STKN_FHIST in rows",
                                "STKN in cols & FHIST in rows", "STKN in rows & FHIST in cols"),
                    selected = "STKN_FHIST in cols",
                    multiple = FALSE
        ), 
        width = 3
      ),
      mainPanel(
        plotOutput("plot09", height = "575px"),
        uiOutput("text09"),
        width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}



#==============================================================================
# Figure 10                                                                 ----
#==============================================================================

fig10panel <- function() {
  
  fluidPage(
    titlePanel("Sensitivity to index CV"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        checkboxGroupInput("id10", "Indicator:",
                           choiceNames = perflabels, choiceValues = perfnms, 
                           selected = perfnms, 
                           inline = TRUE
        ),
        checkboxGroupInput("stkn10", "Stock type:",
                           choiceNames = STKNnms_cvid, choiceValues = STKNnms_cvid,
                           selected = STKNnms_cvid,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist10", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms_cvid), FHISTnms_cvid),
                           selected = FHISTnms_cvid, 
                           inline = TRUE
        ),
        checkboxGroupInput("term10", "Term:",
                           choices = setNames(as.list(TERMnms_cvid), TERMnms_cvid),
                           selected = c("short","long"), #TERMnms_cvid 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt10", "Rule (n-over-m):",
                    choices = setNames(as.list(HCRTnms_cvid), HCRTnms_cvid),
                    selected = HCRTnms_cvid,
                    inline = TRUE
        ),
        checkboxGroupInput("uc10", "Uncertainty cap (low,up):",
                           choices = setNames(as.list(UCnms_cvid), UCnms_cvid),
                           selected = UCnms_cvid, 
                           inline = TRUE
        ),
        h4("Figure settings"), 
        selectInput("idpos10",
                    label = "Select STKN and fhist positions:",
                    choices = c("STKN_FHIST in cols","STKN_FHIST in rows",
                                "STKN in cols & FHIST in rows", "STKN in rows & FHIST in cols"),
                    selected = "STKN_FHIST in cols",
                    multiple = FALSE
        ), 
        width = 3
      ),
      mainPanel(
        plotOutput("plot10", height = "555px"),
        uiOutput("text10"),
        width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}



#==============================================================================
# Figure 11                                                                 ----
#==============================================================================

fig11panel <- function() {

  fluidPage(
    titlePanel("IAV in the projection period"),

    sidebarLayout(
      sidebarPanel(
        h4("Select variables to plot"),
        selectInput("id11", "Indicator:",
                    choices = perfnms,
                    selected = "Risk3.Blim",
                    multiple = FALSE
        ),
        checkboxGroupInput("stkn11", "Stock type:",
                           choices = setNames(as.list(STKNnms_cvid), STKNnms_cvid),
                           selected = STKNnms_cvid,
                           inline = TRUE
        ),
        checkboxGroupInput("fhist11", "Historical exploitation level:",
                           choices = setNames(as.list(FHISTnms_cvid), FHISTnms_cvid),
                           selected = FHISTnms_cvid,
                           inline = TRUE
        ),
        checkboxGroupInput("term11", "Term:",
                           choices = setNames(as.list(TERMnms), TERMnms),
                           selected = c("short","long"), #TERMnms 
                           inline = TRUE
        ),
        checkboxGroupInput("hcrt11", "Rule (n-over-m):",
                           choices = setNames(as.list(HCRTnms_cvid), HCRTnms_cvid),
                           selected = HCRTnms_cvid,
                           inline = TRUE
        ),
        checkboxGroupInput("cvid11", "Index CV:",
                           choices = setNames(as.list(CVIDnms_cvid), CVIDnms_cvid),
                           selected = CVIDnms_cvid,
                           inline = TRUE
        ),
        h4("Figure settings"),
        selectInput("idpos11",
                    label = "Select CVID position:",
                    choices = c("CVID in cols","CVID in rows"),
                    selected = "CVID in cols",
                    multiple = FALSE
        ),
        width = 3
      ),
      mainPanel( # "main panel",
        # tableOutput("table11"),
        # textOutput("txt11"),
        plotOutput("plot11", height = "575px"),
        uiOutput("text11"),width = 9
      )
    ) # close sidebarLayout
  ) # close fluidpage
}




