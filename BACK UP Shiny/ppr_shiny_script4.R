library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(openxlsx)

# Maximale Upload-Größe erhöhen
options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB

# UI ####
ui <- fluidPage(
  
  titlePanel("PPR2.0"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("show_instructions", "Anleitung anzeigen"), # Anleitung-Schaltfläche
      br(), br(), # Fügt zwei Zeilenumbrüche ein
      fileInput("TNL", "Aufenthalte hochladen:", accept = c(".csv")),
      fileInput("L3", "PPR-Kategorien hochladen", accept = c(".csv")),
      fileInput("Steuerung", "Steuerungsdatei hochladen:", accept = c(".csv")),
      fileInput("Nachtdienst", "Nachtdienststeuerung hochladen", accept = c(".csv")),
      
      downloadButton("downloadData", "Ergebnisse herunterladen")
    ),
    
    mainPanel(
      h4("Status der hochgeladenen Dateien:"),
      verbatimTextOutput("fileStatus")
    )
  )
)

# Server ####
server <- function(input, output, session) {
  
  # Anleitung anzeigen
  observeEvent(input$show_instructions, {
    showModal(
      modalDialog(
        title = "Dateiformat-Anleitung",
        HTML("
          <ul>
            <li><strong>Aufenthalte hochladen:</strong> Eine CSV-Datei mit den Spalten Fallnummer, PID, Name, Kürzel, Datum.</li>
            <li><strong>PPR-Kategorien hochladen:</strong> Eine CSV-Datei mit den Spalten Aufenthaltsbeginn, Aufenthaltsende, Station.</li>
            <li><strong>Steuerungsdatei hochladen:</strong> Eine CSV-Datei mit Kategorien und Werten.</li>
            <li><strong>Nachtdienststeuerung hochladen:</strong> Eine CSV-Datei mit Patientenanzahl und Minuten.</li>
          </ul>
        "),
        easyClose = TRUE,
        footer = modalButton("Schließen")
      )
    )
  })
  
  # Hochgeladenen Datei-Status anzeigen
  output$fileStatus <- renderPrint({
    list(
      TNL = if (!is.null(input$TNL)) "Aufenthalte geladen" else "Aufenthalte fehlen",
      L3 = if (!is.null(input$L3)) "PPR-Kategorien geladen" else "PPR-Kategorien fehlen",
      Steuerung = if (!is.null(input$Steuerung)) "Steuerungsdatei geladen" else "Steuerungsdatei fehlt",
      Nachtdienst = if (!is.null(input$Nachtdienst)) "Nachtdienststeuerung geladen" else "Nachtdienststeuerung fehlt"
    )
  })
  
  # Datenverarbeitung
  processed_data <- reactive({
    req(input$TNL, input$L3, input$Steuerung, input$Nachtdienst)
    
    # Dateien einlesen
    TNL <- read_delim(input$TNL$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "latin1"))
    L3 <- read_delim(input$L3$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    Steuerung <- read_delim(input$Steuerung$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    Nachtdienst <- read_delim(input$Nachtdienst$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    
    ### Datenverarbeitungsschritte ###
    Steuerung$Wert <- as.numeric(Steuerung$Wert)
    Nachtdienst <- data.frame(lapply(Nachtdienst, as.numeric))
    
    # Beispiel für Rückgabe
    list(
      Steuerung = Steuerung,
      Nachtdienst = Nachtdienst
    )
  })
  
  # Export der Ergebnisse
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PPR_Ergebnisse_", Sys.time(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- processed_data()
      wb <- createWorkbook()
      
      addWorksheet(wb, "Steuerung")
      writeData(wb, "Steuerung", data$Steuerung)
      
      addWorksheet(wb, "Nachtdienst")
      writeData(wb, "Nachtdienst", data$Nachtdienst)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Shiny-App starten
shinyApp(ui, server)
