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
      fileInput("TNL", "TNL.csv hochladen:", accept = c(".csv")),
      fileInput("L3", "L3.csv hochladen:", accept = c(".csv")),
      fileInput("Steuerung", "Steuerung.csv hochladen:", accept = c(".csv")),
      fileInput("Nachtdienst", "Nachtdienst.csv hochladen:", accept = c(".csv")),
      
      downloadButton("downloadData", "Ergebnisse herunterladen")
    ),
    
    mainPanel(
      h4("Status der hochgeladenen Dateien:"),
      verbatimTextOutput("fileStatus")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Hochgeladenen Datei-Status anzeigen
  output$fileStatus <- renderPrint({
    list(
      TNL = if (!is.null(input$TNL)) "TNL.csv geladen" else "TNL.csv fehlt",
      L3 = if (!is.null(input$L3)) "L3.csv geladen" else "L3.csv fehlt",
      Steuerung = if (!is.null(input$Steuerung)) "Steuerung.csv geladen" else "Steuerung.csv fehlt",
      Nachtdienst = if (!is.null(input$Nachtdienst)) "Nachtdienst.csv geladen" else "Nachtdienst.csv fehlt"
    )
  })
  
  # Datenverarbeitung
  processed_data <- reactive({
    req(input$TNL, input$L3, input$Steuerung, input$Nachtdienst)
    
    # Dateien einlesen
    TNL <- read_delim(input$TNL$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "latin1"))
    L3 <- read_delim(input$L3$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Steuerung <- read_delim(input$Steuerung$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    Nachtdienst <- read_delim(input$Nachtdienst$datapath, delim = ";", locale = locale(decimal_mark = ",",encoding = "UTF-8"))
    
    ### Datenverarbeitungsschritte
    Steuerung$Wert<-as.numeric(Steuerung$Wert)
    Nachtdienst <- data.frame(lapply(Nachtdienst, as.numeric))
    
    # PPR-Daten
    ppr_daten <- TNL %>%
      filter(grepl("^A[0-9]", Kürzel)) %>%
      select(Fallnummer, PID, Name, Kürzel, 
             Dauer_min=`Dauer (Minuten)`, 
             Datum=`Datum (Tag)`, 
             Zeitpunkt=`Datum (Zeit)`,
             anf_ks=`Anf. KS`, 
             anf_kostenstelle=`Anf. Kostenstelle`) %>%
      mutate(
        Tag = as_date(parse_date_time(Datum, orders = c("%d.%m.%y", "%d.%m.%Y"))),
        aufzeit = format(parse_date_time(Zeitpunkt, orders = c("%H:%M:%S", "%H:%M")), "%H:%M:%S")
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
      select(PID, Fallnummer, Station = Station...9, auftag, enttag)
    
    # Masterliste
    tagesliste <- L3 %>%
      filter(!is.na(enttag)) %>%
      rowwise() %>%
      mutate(Tag = list(seq(auftag, enttag, by = "day"))) %>%
      unnest(Tag)
    
    tagesliste<-
      tagesliste[!duplicated(tagesliste[, c("PID", "Tag")], fromLast = TRUE), ]
    
    
    tagesliste2 <- tagesliste %>%
      left_join(ppr_daten %>% select(PID, Kürzel, Tag,anf_kostenstelle), by = c("PID", "Tag")) %>%
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
    
    
    ppr_ergebnis <- tagesliste3 %>%
      group_by(Fallnummer) %>%
      mutate(
        Aufnahmewert = ifelse(Tag == min(Tag), as.numeric(Steuerung[Steuerung$Kategorie == "Aufnahme",]$Wert), 0),
        Entlasswert = ifelse(Tag == max(Tag), lag(Wert, 1) * as.numeric(Steuerung[Steuerung$Kategorie == "Entlassung",]$Wert), 0),
        Gesamtwert = Pflegegrundwert + Aufnahmewert + ifelse(Entlasswert > 0, Entlasswert, Wert),
        nachts_da=ifelse(Tag==max(Tag),0,1)
      )
    
    
    # Personalbedarf berechnen
    arbeitszeit_monat <- 38.5 * (365 / 12 / 7) * 60 * (1 - as.numeric(Steuerung[Steuerung$Kategorie == "Ausfallzeiten",]$Wert))
    
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
                         Minuten_Hilf_nacht_net,
                         Minuten_ex_nacht_geb_net),
                by=c("Patienten_mitternacht"="Patienten"))%>%
      mutate(Minuten_exam_gesamt=ifelse(Station2!="GB2",
                                        ppr_tag* Steuerung[Steuerung$Kategorie=="Anteil_Hilf",]$Wert+Minuten_ex_nacht_net,
                                        ppr_tag* Steuerung[Steuerung$Kategorie=="Anteil_Hilf",]$Wert+Minuten_ex_nacht_geb_net),
             Minuten_hilf_gesamt=ppr_tag*(1- Steuerung[Steuerung$Kategorie=="Anteil_Hilf",]$Wert)+Minuten_Hilf_nacht_net)
    
    
    vk_pro_Monat_Station<-
      ppr_ergebnis_minuten%>%
      group_by(Station2, Jahr=year(Tag),Monat=month(Tag))%>%
      summarise(Examiniert=sum(Minuten_exam_gesamt,na.rm=T),
                Hilf=sum(Minuten_hilf_gesamt,na.rm=T),
                Fehlende_Tageswerte=sum(Fehlende_Tageswerte),
                Patiententage=sum(Patiententage))%>%
      mutate(VK_Exam=Examiniert/arbeitszeit_monat,
             VK_Hilf=Hilf/arbeitszeit_monat,
             Anteil_fehlende_Werte=Fehlende_Tageswerte/Patiententage)
    
    fehlende_daten<-
      ppr_ergebnis_minuten%>%
      group_by(Station2,Monat=month(Tag))%>%
      summarise(Tage=sum(Patiententage),
                Fehlend=sum(Fehlende_Tageswerte))%>%
      mutate(Anteil_fehlend=Fehlend/Tage)
      
      
    
    # Ergebnisse zurückgeben
    list(
      vk_pro_Monat_Station = vk_pro_Monat_Station,
      Steuerung = Steuerung,
      ppr_ergebnis_minuten=ppr_ergebnis_minuten,
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
      
      addWorksheet(wb, "ppr_ergebnis")
      writeData(wb, "ppr_ergebnis", data$ppr_ergebnis_minuten)
      
      addWorksheet(wb, "vk_pro_Monat_Station")
      writeData(wb, "vk_pro_Monat_Station", data$vk_pro_Monat_Station)
      
      addWorksheet(wb, "Steuerung")
      writeData(wb, "Steuerung", data$Steuerung)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Shiny-App starten
shinyApp(ui, server)
