################################################################################
#
#
# This is a Shiny web application to support the implementation of health and
# nutrition coverage surveys in Liberia.
#
# This is the code for the user interface (UI) of the Shiny web application.
#
#
################################################################################


################################################################################
#
# UI for web application
#
################################################################################

## Define UI for application
ui <- dashboardPage(
  skin = "red",
  ## Header
  dashboardHeader(
    title = "Liberia Coverage Surveys",
    titleWidth = 300),
  ## Sidebar
  dashboardSidebar(
    width = 300,
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton"
    ),
    ## Sidebar menu
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Urban Montserrado", 
        tabName = "gm", 
        icon = icon(name = "pencil", 
                    lib = "font-awesome",
                    class = "fa-lg")),
      menuItem(text = "Grand Bassa", 
        tabName = "gb",
        icon = icon(name = "tablet",
                    lib = "font-awesome",
                    class = "fa-lg"))
    )
  ),
  ## Body
  dashboardBody(
    ## Specify a custom.css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## Body outputs for every menu item on sidebar
    tabItems(
      ## Body output when 'gm' menu is selected
      tabItem(tabName = "gm",
        fluidRow(
          box(title = "Options",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            radioButtons(inputId = "round",
              label = "Select round",
              choices = c("Round 1" = "r1",
                          "Round 2" = "r2"),
              selected = "r1"
            ),
            radioButtons(inputId = "indicators",
              label = "Select indicators",
              choices = c("Iron-folic acid coverage" = "ifa",
                          "IYCF counselling coverage" = "iycf",
                          "Micronutrient powder coverage" = "mnp",
                          "Vitamin A coverage" = "vit",
                          "Nutrition screening coverage" = "screen",
                          "Stunting prevalence" = "stunt",
                          "Underweight prevalence" = "underweight",
                          "Wasting (WHZ)" = "whz",
                          "Wasting (MUAC)" = "muac"),
              selected = "ifa"
            )
          ),
          box(title = "Map",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            leafletOutput(outputId = "mapGM")
          ),
          tabBox(id = "gm",
            selected = "ifaGM",
            title = "",
            width = 12,
            tabPanel(title = "IFA",
              value = "ifaGM",
              fluidRow(
                box(title = "Overall estimates",
                  status = "danger",
                  solidHeader = TRUE
                ),
                box(title = "Reasons for non-coverage",
                  status = "danger",
                  solidHeader = TRUE
                )
              )
            ),
            tabPanel(title = "IYCF",
              value = "iycfGM"
            ),
            tabPanel(title = "MNP",
              value = "mnpGM"
            ),
            tabPanel(title = "Vitamin A",
              value = "vitGM"
            ),
            tabPanel(title = "Nutrition Screening",
              value = "screenGM"
            ),
            tabPanel(title = "IMAM",
              value = "imamGM"
            ),
            tabPanel(title = "Stunting",
              value = "stuntGM"
            ),
            tabPanel(title = "Underweight",
              value = "underweightGM"
            ),
            tabPanel(title = "Wasting (WHZ)",
              value = "whzGM"
            ),
            tabPanel(title = "Wasting (MUAC)",
              value = "muacGM"
            ),
            tabPanel(title = "Oedema",
              value = "oedemaGM"
            )
          )
        )
      ),
      ## Body output when 'gb' menu is selected
      tabItem(tabName = "gb",
        fluidRow(
        )
      )
    )
  )
)
