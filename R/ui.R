library(shiny)
library(networkD3)

shinyUI(fluidPage(
  
  titlePanel("Sankey Diagram Visualization for Severe Sepsis Patients Clinical Pathways"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("sinksRight", "Align to the left", value = TRUE),
      h3("Instructions for reading the Sankey diagram"),
      h4("Acronyms"),
      p("--SI: suspect infection onset"),
      p("--SIRS2: two SIRS onset"),
      p("--OD1: first site of organ dysfunctio onset"),
      p("--AC: abnormal coagulation"),
      p("--KF: kidney failure"),
      p("--IL: increased lactate"),
      p("--AMS: altered mental status"),
      p("--LF: liver failure"),
      p("--BLOOD_C: first blood cultuer order"),
      p("--ABX: first antibiotic administration"),
      br(),
      h4("Temporal indicator"),
      p("--@X: inidicates the relative temporal order of an event along an existing pathway"),
      br(),
      h4("Color Codes"),
      p("--pink: sepsis alerts"),
      p("--blue: treatment bundle components"),
      p("--green: a pathway with lower risk (empirical odd ration <= 1) to sepsis onset; the darker the green is the lower the risk is"),
      p("--red: a pathway with higher risk (empirical odd ratio > 1) to sepsis onset; the darker the green is the lower the risk is"),
      p("--grey: a pathway lead to no sepsis within 48 hours since triage"),
      br(),
      h4("Tooltip Messages"),
      p("--if you hover over each node, you can see exactly how many patient-encounters have reached that state"),
      p("--if you hover over each link, you can see exactly how many patient-encounters have experienced that transition"),
      p("--size of links and nodes is proportional to patient-encounter size")
    ),
    
    mainPanel(
      sankeyNetworkOutput("sankey",width="100%",height="1000px")
    )
  )
))