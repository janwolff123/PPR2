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
      fileInput("L3", "Aufenthalte hochladen:", accept = c(".csv")),
      fileInput("TNL", "PPR-Kategorien hochladen", accept = c(".csv")),
      fileInput("Steuerung", "Steuerungsdatei hochladen:", accept = c(".csv")),
      fileInput("Nachtdienst", "Nachtdienststeuerung hochladen:", accept = c(".csv")),
      fileInput("Station", "Stationen hochladen:", accept = c(".csv")),
      fileInput("Neugeborene", "Neugeborende hochladen:", accept = c(".csv"))
    ),
    
    mainPanel(
      h4("Status der hochgeladenen Dateien:"),
      uiOutput("fileStatus"), # Benutzerfreundliche Statusanzeige
      br(),
      downloadButton("downloadData", "Ergebnisse herunterladen"),
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Anleitung anzeigen
  observeEvent(input$show_instructions, {
    showModal(
      modalDialog(
        title = "Dateiformat-Anleitung",
        HTML("
          <ul>
            <li><strong>PPR-Kategorien hochladen:</strong> Eine CSV-Datei mit den Spalten Fallnummer, PID, Name, Kürzel, Datum (Tag).</li>
            <li><strong>Aufenthalte hochladen:</strong> Eine CSV-Datei mit den Spalten Aufenthaltsbeginn, Aufenthaltsende, Station.</li>
            <li><strong>Steuerungsdatei hochladen:</strong> Eine CSV-Datei mit Kategorien und Werten.</li>
            <li><strong>Nachtdienststeuerung hochladen:</strong> Eine CSV-Datei mit Patientenanzahl und Minuten.</li>
            <li><strong>Station hochladen:</strong> Eine CSV-Datei mit Stationsbezeichnung, Anzahl der Betten und Ausfallzeiten.</li>
            <li><strong>Neugeborene hochladen:</strong> Eine CSV-Datei der Fallnummer der Neugeborenen.</li>
          </ul>
        "),
        easyClose = TRUE,
        footer = modalButton("Schließen")
      )
    )
  })
  
  # Benutzerfreundliche Statusanzeige mit Farben
  output$fileStatus <- renderUI({
    status <- list(
      "Aufenthalte" = if (!is.null(input$L3)) "geladen" else "fehlt",
      "PPR-Kategorien" = if (!is.null(input$TNL)) "geladen" else "fehlt",
      "Steuerungsdatei" = if (!is.null(input$Steuerung)) "geladen" else "fehlt",
      "Nachtdienststeuerung" = if (!is.null(input$Nachtdienst)) "geladen" else "fehlt",
      "Station" = if (!is.null(input$Station)) "geladen" else "fehlt",
      "Neugeborene" = if (!is.null(input$Neugeborene)) "geladen" else "fehlt"
    )
    
    # HTML-Ausgabe mit farblicher Darstellung
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
  processed_data <- reactive({
    req(input$TNL, input$L3, input$Steuerung, input$Nachtdienst, input$Station, input$Neugeborene)
    
    # Dateien einlesen
    TNL <- read_delim(input$TNL$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    L3 <- read_delim(input$L3$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Steuerung <- read_delim(input$Steuerung$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Nachtdienst <- read_delim(input$Nachtdienst$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Station <- read_delim(input$Station$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Neugeborene <- read_delim(input$Neugeborene$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))

    ### Datenverarbeitungsschritte
    Steuerung$Wert<-as.numeric(Steuerung$Wert)
    Nachtdienst <- data.frame(lapply(Nachtdienst, as.numeric))
    
    # PPR-Daten
    ppr_daten <- TNL %>%
      filter(grepl("^A[0-9]", .[[1]])) %>%
      select(Fallnummer, 
             PID, 
             Name, 
             Kürzel, 
             Datum=`Datum (Tag)`) %>%
      mutate(
        Tag = as_date(parse_date_time(Datum, orders = c("%d.%m.%y", "%d.%m.%Y")))
      )
    
    
    iso_daten <- ppr_daten %>%
      filter(Kürzel == "A5_ISO")
    
    ppr_daten <- ppr_daten %>%
      filter(Kürzel != "A5_ISO") 
    
    ppr_daten<-
      ppr_daten[!duplicated(ppr_daten[, c("PID", "Datum")], fromLast = TRUE), ]
    
    iso_daten<-
      iso_daten[!duplicated(iso_daten[, c("PID", "Datum")], fromLast = TRUE), ]
    
    # L3-Daten
    L3 <- L3 %>%
      mutate(
        auftag = as_date(parse_date_time(Aufenthaltsbeginn, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M"))),
        enttag = as_date(parse_date_time(Aufenthaltsende, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M")))
      ) %>%
      select(PID, Fallnummer, Station = Station...9, auftag, enttag)%>%
      filter(!is.na(Station))
    
    L3<-
      L3%>%
      filter(year(auftag)>2023)
    
    
    # Masterliste
    tagesliste <- L3 %>%
      filter(!is.na(enttag))%>%
      rowwise() %>%
      mutate(Tag = list(seq(auftag, enttag, by = "day"))) %>%
      unnest(Tag)
    
    tagesliste<-
      tagesliste[!duplicated(tagesliste[, c("PID", "Tag")], fromLast = TRUE), ]
    
    
    tagesliste2 <- tagesliste %>%
      left_join(ppr_daten %>% select(PID, Kürzel, Tag), by = c("PID", "Tag")) %>%
      left_join(iso_daten %>% select(PID, Tag) %>% mutate(iso = 1), by = c("PID", "Tag")) %>%
      replace_na(list(iso = 0))
    
    
    tagesliste3 <- tagesliste2 %>%
      group_by(Fallnummer) %>%
      arrange(Tag, .by_group = TRUE)%>%
      mutate(Kürzel2=Kürzel)%>%
      fill(Kürzel, .direction = "up") %>% # Werte aus der nächsten oder vorherigen Zeile verwenden
      fill(Kürzel, .direction = "down") %>% # Werte aus der nächsten oder vorherigen Zeile verwenden
      left_join(Steuerung, by = c("Kürzel" = "Kategorie")) %>%
      mutate(Wert=as.numeric(Wert))%>%
      mutate(Pflegegrundwert = ifelse(iso == 1, 
                                      as.numeric(Steuerung$Wert[Steuerung$Kategorie == "erhöhter Pflegegrundwert"]), 
                                      as.numeric(Steuerung$Wert[Steuerung$Kategorie == "Pflegegrundwert"])))%>%
      mutate(imputed=ifelse(is.na(Wert),
                            1,
                            0))%>%
      group_by(Station)%>%
      mutate(Wert=ifelse(is.na(Wert),
                         mean(Wert,na.rm=T),
                         Wert))
    
    # Anpassung der Neugeborenen
    Neugeborene$Fallnummer<-as.character(Neugeborene$`Fallnummer  Säugling`)
    
    
    Neugeborene_2<-
      Neugeborene%>%
      select(Fallnummer, Entbindung, Entlassung)%>%
      mutate(auftag = as_date(parse_date_time(Entbindung, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M"))),
             enttag = as_date(parse_date_time(Entlassung, orders = c("%d.%m.%y %H:%M", "%d.%m.%Y %H:%M"))))%>%
      filter(!is.na(enttag))%>%
      rowwise() %>%
      mutate(Tag = list(seq(auftag, enttag, by = "day"))) %>%
      unnest(Tag)%>%
      select(Fallnummer,auftag, enttag, Tag)%>%
      distinct()%>%
      mutate(Station="GB2",
             Gesamtwert=110)%>%
      group_by(Fallnummer)%>%
      mutate(nachts_da=ifelse(Tag==max(Tag),0,1),
             Kürzel="Ges_Neugeb")
    
    tagesliste4<-
      tagesliste3%>%
      anti_join(Neugeborene_2,
                by="Fallnummer")
    
    ### PPR Ergebnis
    
    ppr_ergebnis_pre <- tagesliste4 %>%
      group_by(Fallnummer) %>%
      mutate(
        Aufnahmewert = ifelse(Tag == min(Tag), as.numeric(Steuerung[Steuerung$Kategorie == "Aufnahme",]$Wert), 0),
        Gesamtwert= Wert + Pflegegrundwert + Aufnahmewert,
        Entlasswert = ifelse(Tag == max(Tag), lag(Gesamtwert, 1) * as.numeric(Steuerung[Steuerung$Kategorie == "Entlassung",]$Wert), 0),
        Gesamtwert=ifelse(Entlasswert > 0, Entlasswert, Gesamtwert),
        nachts_da=ifelse(Tag==max(Tag),0,1)
      )
    
    ppr_ergebnis<-
      ppr_ergebnis_pre%>%
      rbind(Neugeborene_2)
    
    #Ausfallzeiten
    
    Station<-
      Station%>%
      ungroup()%>%
      mutate(Station2=ifelse(Station %in% c("CH1", "CH3"),"CH13",Station))%>%
      mutate(Station2=ifelse(Station %in% c("IM1", "IM2"),"IM12",Station2))%>%
      group_by(Station2, Kategorie, Ausfall1, Ausfall2, Ausfall3)%>%
      summarise(Betten=sum(Betten))%>%
      distinct()%>%
      ungroup()
    
    Station$Ausfallzeit<-
      Station$Ausfall1+Station$Ausfall2+Station$Ausfall3
    
    
    # Personalbedarf berechnen
    
    Station$arbeitszeit_monat <- 38.5 * (365 / 12 / 7) * 60 
    
    Station$arbeitszeit_tag_vk <- (38.5/7)*60 
    
    
    ppr_ergebnis_minuten<-
      ppr_ergebnis%>%
      mutate(Station2=ifelse(Station %in% c("CH1", "CH3"),"CH13",Station))%>%
      mutate(Station2=ifelse(Station %in% c("IM1", "IM2"),"IM12",Station2))%>%
      group_by(Station2, Tag)%>%
      summarise(ppr_tag=sum(Gesamtwert, na.rm=T),
                Patienten_mitternacht=sum(nachts_da, na.rm=T),
                Fehlende_Tageswerte=sum(imputed),
                Patiententage=n())%>%
      left_join(Nachtdienst%>%
                  select(Patienten,
                         Minuten_ex_nacht_net,
                         Minuten_ex_nacht_geb_net),
                by=c("Patienten_mitternacht"="Patienten"))%>%
      left_join(Station%>%
                  select(Station2, arbeitszeit_tag_vk),
                by="Station2")%>%
      mutate(Minuten_nacht=ifelse(Station2!="GB2",
                                  Minuten_ex_nacht_geb_net,
                                  Minuten_ex_nacht_net),
             VK_tag=ppr_tag/arbeitszeit_tag_vk,
             VK_nacht=Minuten_nacht/arbeitszeit_tag_vk)%>%
      select(-c("Minuten_ex_nacht_net","Minuten_ex_nacht_geb_net"))
    
    
    
    ppr_bericht_pre1<-
      ppr_ergebnis_minuten%>%
      group_by(Station2,Monat=month(Tag))%>%
      summarise(Minuten=sum(ppr_tag),
                Anzahl_Patienten=sum(Patiententage),
                durchschnittliche_Patientenbelegung=mean(Patiententage,na.rm=T),
                Anzahl_Schichten=n_distinct(Tag),
                VK=sum(VK_tag)/n_distinct(Tag))%>%
      mutate(Schicht="Tag")%>%
      rbind(
        ppr_ergebnis_minuten%>%
          group_by(Station2,Monat=month(Tag))%>%
          summarise(Minuten=sum(Minuten_nacht),
                    Anzahl_Patienten=sum(Patienten_mitternacht),
                    durchschnittliche_Patientenbelegung=mean(Patienten_mitternacht,na.rm=T),
                    Anzahl_Schichten=n_distinct(Tag),
                    VK=sum(VK_nacht/Anzahl_Schichten))%>%
          mutate(Schicht="Nacht"))%>%
      select(Station2, Monat, Schicht,Anzahl_Schichten, Anzahl_Patienten,durchschnittliche_Patientenbelegung, VK)%>%
      arrange(Station2, Monat)
    
    ppr_bericht<-
      ppr_bericht_pre1%>%
      left_join(Station%>%
                  select(Station2,Kategorie,Betten, Ausfall1, Ausfall2, Ausfall3), 
                by="Station2")%>%
      mutate(Soll_Besetzung=VK,
             Leitende_Pflege=Soll_Besetzung/50,
             Ausfallzeiten=Steuerung[Steuerung$Kategorie == "Ausfallzeiten",]$Wert,
             Station_GynGeb=ifelse(Station2=="GB2","ja", "nein"),
             Standortkennzeichen="Marienstift",
             FAB_Key="noch_fuellen",
             FAB_Name="noch_fuellen")%>%
      select(Standortkennzeichen,Station2,
             FAB_Key,FAB_Name,Kategorie,
             Monat, Schicht, Anzahl_Schichten,Anzahl_Betten=Betten,
             Anzahl_Patienten,durchschnittliche_Patientenbelegung,Station_GynGeb,
             Soll_Besetzung,Ausfall1, Ausfall2, Ausfall3,Leitende_Pflege)
    
    personalbedarf_station_monat<-
      ppr_bericht%>%
      group_by(Station2, Monat)%>%
      summarise(VK_ges=sum(Soll_Besetzung)+sum(Leitende_Pflege),
                mittlere_Belegung=mean(durchschnittliche_Patientenbelegung))
    
    
    fehlende_daten<-
      ppr_ergebnis_minuten%>%
      group_by(Station2,Monat=month(Tag))%>%
      summarise(Tage=sum(Patiententage),
                Fehlend=sum(Fehlende_Tageswerte))%>%
      mutate(Anteil_fehlend=Fehlend/Tage)
    
  
    
    # Ergebnisse zurückgeben
    list(
      ppr_ergebnis_minuten=ppr_ergebnis_minuten,
      ppr_bericht=ppr_bericht,
      personalbedarf_station_monat = personalbedarf_station_monat,
      Steuerung = Steuerung,
      fehlende_daten = fehlende_daten
    )
  })
  
  # Export der Ergebnisse
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PPR_Ergebnisse_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- processed_data()
      wb <- createWorkbook()
      
      addWorksheet(wb, "ppr_bericht")
      writeData(wb, "ppr_bericht", data$ppr_bericht)
      
      addWorksheet(wb, "ppr_ergebnis")
      writeData(wb, "ppr_ergebnis", data$ppr_ergebnis_minuten)
      
      addWorksheet(wb, "personalbedarf_station_monat")
      writeData(wb, "personalbedarf_station_monat", data$personalbedarf_station_monat)
      
      addWorksheet(wb, "fehlende_daten")
      writeData(wb, "fehlende_daten", data$fehlende_daten)
      
      addWorksheet(wb, "Steuerung")
      writeData(wb, "Steuerung", data$Steuerung)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Shiny-App starten
shinyApp(ui, server)
