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
      br(), br(),
      fileInput("TNL", "Aufenthalte hochladen:", accept = c(".csv")),
      fileInput("L3", "PPR-Kategorien hochladen", accept = c(".csv")),
      fileInput("Steuerung", "Steuerungsdatei hochladen:", accept = c(".csv")),
      fileInput("Nachtdienst", "Nachtdienststeuerung hochladen", accept = c(".csv"))
    ),
    
    mainPanel(
      h4("Status der hochgeladenen Dateien:"),
      uiOutput("fileStatus"), # Benutzerfreundliche Statusanzeige
      br(),
      downloadButton("downloadData", "Ergebnisse herunterladen"),
      br(),
      h4("Berechnungsfortschritt:"),
      textOutput("progressStatus") # Fortschrittsanzeige
    )
  )
)

# Server ####
server <- function(input, output, session) {
  
  # Reaktiver Fortschrittsstatus
  progress <- reactiveVal("Warten auf Start...")
  
  # Fortschrittsanzeige
  output$progressStatus <- renderText({
    progress()
  })
  
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
  
  # Benutzerfreundliche Statusanzeige
  output$fileStatus <- renderUI({
    status <- list(
      "Aufenthalte" = if (!is.null(input$TNL)) "geladen" else "fehlt",
      "PPR-Kategorien" = if (!is.null(input$L3)) "geladen" else "fehlt",
      "Steuerungsdatei" = if (!is.null(input$Steuerung)) "geladen" else "fehlt",
      "Nachtdienststeuerung" = if (!is.null(input$Nachtdienst)) "geladen" else "fehlt"
    )
    
    tagList(
      HTML("<ul>"),
      lapply(names(status), function(name) {
        status_color <- if (status[[name]] == "geladen") "green" else "red"
        HTML(paste0("<li><strong>", name, ":</strong> ",
                    "<span style='color:", status_color, "'>", status[[name]], "</span></li>"))
      }),
      HTML("</ul>")
    )
  })
  
  # Datenverarbeitung
  processed_data <- eventReactive(input$downloadData, {
    req(input$TNL, input$L3, input$Steuerung, input$Nachtdienst)
    
    # Fortschritt auf "Berechnung läuft..." setzen
    progress("Berechnung läuft...")
    
    # Dateien einlesen
    Sys.sleep(1)
    TNL <- read_delim(input$TNL$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "latin1"))
    progress("Datei 'Aufenthalte' verarbeitet.")
    
    Sys.sleep(1)
    L3 <- read_delim(input$L3$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    progress("Datei 'PPR-Kategorien' verarbeitet.")
    
    Sys.sleep(1)
    Steuerung <- read_delim(input$Steuerung$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    progress("Datei 'Steuerungsdatei' verarbeitet.")
    
    Sys.sleep(1)
    Nachtdienst <- read_delim(input$Nachtdienst$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"))
    progress("Datei 'Nachtdienststeuerung' verarbeitet.")
    
    # Datenverarbeitungsschritte
    Steuerung$Wert <- as.numeric(Steuerung$Wert)
    Nachtdienst <- data.frame(lapply(Nachtdienst, as.numeric))
    
    # PPR-Daten
    ppr_daten <- TNL %>%
      filter(grepl("^A[0-9]", Kürzel)) %>%
      select(Fallnummer, PID, Name, Kürzel, Datum = `Datum (Tag)`) %>%
      mutate(Tag = as_date(parse_date_time(Datum, orders = c("%d.%m.%y", "%d.%m.%Y"))))
    
    iso_daten <- ppr_daten %>%
      filter(Kürzel == "A5_ISO")
    
    ppr_daten <- ppr_daten %>%
      filter(Kürzel != "A5_ISO") 
    
    ppr_daten <- ppr_daten[!duplicated(ppr_daten[, c("PID", "Datum")], fromLast = TRUE), ]
    iso_daten <- iso_daten[!duplicated(iso_daten[, c("PID", "Datum")], fromLast = TRUE), ]
    
    # L3-Daten
    L3 <- L3 %>%
      mutate(
        auftag = as_date(parse_date_time(Aufenthaltsbeginn, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M"))),
        enttag = as_date(parse_date_time(Aufenthaltsende, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M")))
      ) %>%
      select(PID, Fallnummer, Station = Station...9, auftag, enttag)
    
    # Masterliste
    tagesliste <- L3 %>%
      filter(!is.na(enttag)) %>%
      rowwise() %>%
      mutate(Tag = list(seq(auftag, enttag, by = "day"))) %>%
      unnest(Tag)
    
    tagesliste <- tagesliste[!duplicated(tagesliste[, c("PID", "Tag")], fromLast = TRUE), ]
    
    tagesliste2 <- tagesliste %>%
      left_join(ppr_daten %>% select(PID, Kürzel, Tag), by = c("PID", "Tag")) %>%
      left_join(iso_daten %>% select(PID, Tag) %>% mutate(iso = 1), by = c("PID", "Tag")) %>%
      replace_na(list(iso = 0))
    
    tagesliste3 <- tagesliste2 %>%
      group_by(Fallnummer) %>%
      arrange(Tag, .by_group = TRUE) %>%
      mutate(Kürzel2 = Kürzel) %>%
      fill(Kürzel, .direction = "up") %>%
      fill(Kürzel, .direction = "down") %>%
      left_join(Steuerung, by = c("Kürzel" = "Kategorie")) %>%
      mutate(Wert = as.numeric(Wert)) %>%
      mutate(Pflegegrundwert = ifelse(iso == 1, 
                                      as.numeric(Steuerung$Wert[Steuerung$Kategorie == "erhöhter Pflegegrundwert"]), 
                                      as.numeric(Steuerung$Wert[Steuerung$Kategorie == "Pflegegrundwert"]))) %>%
      mutate(imputed = ifelse(is.na(Wert), 1, 0)) %>%
      group_by(Station) %>%
      mutate(Wert = ifelse(is.na(Wert), mean(Wert, na.rm = TRUE), Wert))
    
    progress("Berechnung abgeschlossen.")
    
    list(
      Steuerung = Steuerung,
      Nachtdienst = Nachtdienst,
      ppr_ergebnis_minuten = tagesliste3,
      fehlende_daten = fehlende_daten
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
      
      addWorksheet(wb, "fehlende_daten")
      writeData(wb, "fehlende_daten", data$fehlende_daten)
      
      
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
