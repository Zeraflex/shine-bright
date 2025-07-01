library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(readxl)
library(plotly)
library(paletteer)
library(httr)
library(jsonlite)
library(waffle)



# Data wrangling ----

# BIP pro Kopf Datensatz
data <- readxl::read_excel("data/datenbank.xlsx", sheet = 1)

# BIP Lange Frist Datensatz
bip_long <- read.csv("https://dam-api.bfs.admin.ch/hub/api/dam/assets/32257625/master", sep = ",", stringsAsFactors = FALSE)
bip_long <- bip_long %>%
  filter(UNIT_MEAS == "MCHF") %>%
  select(PERIOD, VALUE) %>%
  mutate(VALUE = VALUE/1000) %>%
  rename(
    "Jahr" = "PERIOD",
    "Milliarden Franken" = "VALUE"
  )

# Bundesratsparteien
bund_parteien <- read_csv("data/bundesratsparteien_1848-2024.csv")
bund_parteien <- bund_parteien %>%
  rename("Anzahl Sitze" = "Anzahl_Sitze")


# Staatsausgaben 1990 - 2024 in % Total
staatsausgaben <- readxl::read_xlsx("data/ausgaben_efv_1990-2024.xlsx",sheet = 2)

staatsausgaben_long <- staatsausgaben %>%
  select(-Total) %>%
  pivot_longer(
    cols = -Jahr,
    names_to = "Ausgabengebiet",
    values_to = "Ausgaben"
  )


# Index der Zentralbankunabhängigkeit
cbi_data <- read_xlsx("data/CBIData_Romelli_2024.xlsx", sheet = 2)

cbi_data <- cbi_data %>%
  select(country, year, cbie_index) %>%
  filter(year >= 1980,
         country %in% c("Switzerland", "Germany", "France", "Norway", "Argentina", "Brazil")) %>%
  rename("CBI Index" = "cbie_index",
         "Jahr" = "year")


# Stadt Zürich Hundenamen 2024 top10
zh_dogs <- read_csv("https://data.stadt-zuerich.ch/dataset/sid_stapo_hundenamen_od1002/download/KUL100OD1002.csv")
zh_dogs <- zh_dogs %>%
  select(StichtagDatJahr, HundenameText, AnzHunde) %>%
  filter(StichtagDatJahr == 2024)

zh_dogs_sum <- zh_dogs %>%
  group_by(HundenameText) %>%
  summarise(total = sum(AnzHunde), .groups = "drop")

zh_dogs_final <- zh_dogs_sum %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)


# Stadt Zürich Wohnungsproblematik
api_zhwhg <- "https://data.stadt-zuerich.ch/api/3/action/datastore_search?resource_id=c22f8f30-ab9f-486b-a332-c6fe19e4e50f&limit=15000"
leerwohnungen <- fromJSON(content(GET(api_zhwhg), "text", encoding = "UTF-8"))$result$records
leerwohnungen <- leerwohnungen %>%
  select(StichtagDat, KreisSort, AnzWhgStatLeer_noDM, AnzWhgStat) %>%
  mutate(
    StichtagDat = case_match(StichtagDat,
                             "2009-06-01" ~ 2009,
                             "2010-06-01" ~ 2010,
                             "2011-06-01" ~ 2011,
                             "2012-06-01" ~ 2012,
                             "2013-06-01" ~ 2013,
                             "2014-06-01" ~ 2014,
                             "2015-06-01" ~ 2015,
                             "2016-06-01" ~ 2016,
                             "2017-06-01" ~ 2017,
                             "2018-06-01" ~ 2018,
                             "2019-06-01" ~ 2019,
                             "2020-06-01" ~ 2020,
                             "2021-06-01" ~ 2021,
                             "2022-06-01" ~ 2022,
                             "2023-06-01" ~ 2023,
                             "2024-06-01" ~ 2024)
  )

leerwohnungen <- leerwohnungen %>%
  group_by(StichtagDat, KreisSort) %>%
  summarise(AnzLeerWhg = sum(as.numeric(AnzWhgStatLeer_noDM)), 
            AnzWhg = sum(as.numeric(AnzWhgStat)),
            .groups = "drop") %>%
  mutate(Leerwohnungsziffer = (AnzLeerWhg / AnzWhg) * 100) %>%
  rename(
    "Jahr" = "StichtagDat",
    "Kreis" = "KreisSort",
    "Anzahl Leerwohnungen" = "AnzLeerWhg",
    "Anzahl Wohnungen" = "AnzWhg"
  )


# Kanton Zürich Steuerdaten nach Vermögen
ktzh_wealth <- read_csv("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00000723_00003381.csv")
ktzh_wealth <- ktzh_wealth %>%
  mutate(vermoegensklasse = case_match(vermoegensklasse,
                                       "0  -  999" ~ "Unter 1k",
                                       "1000  -  99999" ~ "1-99k",
                                       "100000  -  199999" ~ "100-199k",
                                       "200000  -  299999" ~ "200-299k",
                                       "300000  -  399999" ~ "300-399k",
                                       "400000  -  499999" ~ "400-499k",
                                       "500000  -  599999" ~ "500-599k",
                                       "600000  -  699999" ~ "600-699k",
                                       "700000  -  799999" ~ "700-799k",
                                       "800000  -  899999" ~ "800-899k",
                                       "900000  -  999999" ~ "900-999k",
                                       "1000000  -  1499999" ~ "1-1.5m",
                                       "1500000  -  1999999" ~ "1.5-2m",
                                       "2000000  -  4999999" ~ "2-5m",
                                       "5000000  -  Inf" ~ "Über 5m",
                                       .default = vermoegensklasse))
ktzh_wealth$vermoegensklasse <- factor(ktzh_wealth$vermoegensklasse, levels = unique(ktzh_wealth$vermoegensklasse))


# User Interface ----
ui <- page_navbar(
  title = "Shining",
  nav_panel(title = "Willkommen",
            page_fillable(
              layout_columns(
                layout_column_wrap(
                  width = 1,
                  card(
                    card_header("Salli!"),
                    card_body(
                    h1("Disclaimer"),
                    h4("Not to be taken too seriously. Die verwendeten Daten sind alle Open Source.")),
                    style = "background-color: #eccfff"
                  )
                ),
                layout_column_wrap(
                  width = 1,
                  card(
                    card_header("Splendid work"),
                    card_image("data/ft_gender_divergence.jpg",
                               style = "display: block; margin-left: auto; margin-right: auto; width: 70%;",
                               href = "https://www.ft.com/content/29fd9b5c-2f35-41bf-9d4c-994db4e12998"),
                    card_body("Graphische Aufarbeitung von John Burn-Murdoch"),
                    card_footer("Quelle: Financial Times"),
                    style = "background-color: #FFF1E0"
                  ),
                  card(
                    card_header("just waffling around"),
                    card_body(plotOutput(outputId = "Waffel"))
                  )
                ),
                col_widths = c(4,8)
              )
            )),
  nav_panel(title = "Ökonomisches",
            navset_card_tab(
              nav_panel(title = "BIP pro Kopf",
                        layout_sidebar(
                          sidebar = sidebar(
                            checkboxGroupInput(inputId = "auswahl_var", label= "Wähle die gewünschten Variablen: ",
                                               c("Nominelles BIP/Kopf" = "nom_bpk",
                                                 "Reales BIP/Kopf" = "rel_bpk")
                            ),
                            sliderInput(inputId = "zeitfenster", label = "Zeitfenster:",
                                        min = min(data$jahr), max = max(data$jahr), sep = "", step = 1,
                                        value = c(min(data$jahr), max(data$jahr)))
                          ),
                          plotOutput(outputId = "bip_pro_kopf")
                        ),
              ),
              nav_panel(title = "BIP lange Frist",
                        plotlyOutput(outputId = "bip_lange_frist") # Plotly Output
              ),
              nav_panel(title = "Zentralbankunabhängigkeit",
                        plotlyOutput(outputId = "zentralbank")
              ),
              nav_panel(title = "Vermögen im Kanton Zürich", # Darstellung von Vermögenssteuerdaten
                        layout_sidebar(
                          sidebar = sidebar(
                            h5("Über Geld spricht man"),
                            p("Ein Überblick über die Vermögensverteilung im Kanton Zürich, 
                              basierend auf einem öffentlich zugänglichen Datensatz des Statistischen Amtes."),
                            selectInput(inputId = "select_y_ktzh_wealth",
                                        label = "Wähle eine Variable:",
                                        list("Anzahl Steuerpflichtige" = "numtaxpayers",
                                             "Steuerbares Vermögen (Mio.)" = "wealth",
                                             "Anteil Steuerpflichtige (%)" = "sharetaxpayers",
                                             "Anteil des Gesamtvermögens" = "sharewealth",
                                             "Anteil der Vermögenssteuer" = "sharetax")),
                            sliderInput(inputId = "slider_year_ktzh_wealth",
                                        label = "Wähle ein Jahr:",
                                        min = min(ktzh_wealth$steuerjahr), 
                                        max = max(ktzh_wealth$steuerjahr),
                                        sep = "",
                                        step = 1,
                                        value = max(ktzh_wealth$steuerjahr))),
                          layout_columns(
                            card(
                              card_header("Balkendiagramm"),
                              plotOutput("ktzh_wealth_col"),
                              card_footer("Quelle: Statistisches Amt des Kantons Zürich")
                            ),
                            card(
                              card_header("Kuchendiagramm"),
                              plotOutput("ktzh_wealth_cake"),
                              card_footer("Quelle: Statistisches Amt des Kantons Zürich")
                            )
                          )
                          )
                        )
              )
  ),
  nav_panel(title = "Politisches",
            navset_card_pill(
              nav_panel(title = "Bundesratsparteien",
                        plotlyOutput(outputId = "bundesratsparteien")
              ),
              nav_panel(title = "Staatsausgaben",
                        plotlyOutput(outputId = "staatsausgaben"))
            )),
  nav_panel(title = "Zurigo",
            navset_card_pill(
              nav_panel(
                title = "Hundenamen",
                layout_columns(
                  card(plotOutput(outputId = "hundenamen")),
                  card(
                    card_header("Wie einzigartig ist mein Hund?"),
                    card_body(
                      textInput("meinhund", 
                                "Gib deinen Hundenamen ein und finde heraus, 
                                wie oft er in Zürich vorkommt!", 
                                placeholder = "Mein Hundenamen"),
                      textOutput("anzahl_hundenamen")
                    )
                  )
                )
              ),
              nav_panel(
                title = "Leerwohnungsziffer",
                layout_sidebar(
                  sidebar = sidebar(
                    h3("Wohnungsnot in Zürich"),
                    p("Die Zürcher Wohnungsnot charakterisiert die reichste Stadt der Welt seit Langem.
                      Die folgenden Darstellungen zeigen Entwicklungen einiger Schlüsselziffern
                      über die letzten 15 Jahre auf. Sogar mit Pastelfarbe"),
                    selectizeInput(
                      "zhwhg",
                      "Wähle eine anzuzeigende Variable:",
                      list(
                        "Anzahl Wohnungen total" = "awt",
                        "Anzahl Leerwohnungen" = "alw",
                        "Leerwohnungsziffer" = "lwz"
                      )
                  )),
                  plotlyOutput("leerwohnungsziffer")
                )
              )
            )),
  nav_spacer(),
  nav_item(input_dark_mode())
)



# Server Function ----
server <- function(input, output){
  
  ### Startseite
  output$Waffel <- renderPlot({
    sirup <- c(6,5,1,4,2,3,3,4,2,5,1,6)
    waffle(sirup, rows = 3, legend_pos = "none", size = 1,
           colors = c("#bef7ff", "#bfe9ff", "#c1daff", "#c2cbff", "#c3bcff", "#c5adff",
                      "#c69cff", "#c88bff", "#c978ff", "#cb62ff", "#ce47ff", "#d400ff"))
  })

  
  ### Seite 1 Plot 1 ###
  
  # Reaktiver Dateninput
  data_filtered <- reactive({
    data[data$jahr >= input$zeitfenster[1] & data$jahr <= input$zeitfenster[2], ]
  })
  
  # Plot erstellen
  output$bip_pro_kopf <- renderPlot({
    
    # Reactive Funktion zu Data Frame umwandeln
    data_filtered <- data_filtered()
    
    # Plot Fundament aufbauen mit gleichbleibender x-Achse
    plot <- ggplot(data_filtered, aes(x = jahr))
    
    # Y-Variable wenn "Nominal" ausgewählt ist
    if ("nom_bpk" %in% input$auswahl_var) {
      plot <- plot + geom_line(aes(y = bip_kopf_nom, color = "Nominell"), linewidth = 1.5)
    }
    
    # Y-Variable wenn "Real" ausgewählt ist
    if ("rel_bpk" %in% input$auswahl_var) {
      plot <- plot + geom_line(aes(y = bip_kopf_real, color = "Real"), linewidth = 1.5)
    }
    
    # Plot fertig zusammenbauen
    plot +
      labs(x = "Jahr", y = "CHF", title = "BIP pro Kopf Entwicklung") +
      scale_color_manual(values = c("Nominell" = "brown3", "Real" = "brown4"),
                         name = "Legende") +
      theme_minimal(base_size = 12)
  })
  
  
  
  ### Seite 1 Plot 2 (Plotly) ###
  
  # Plot erstellen
  output$bip_lange_frist <- renderPlotly({
    plot2<- ggplot(bip_long, aes(x = Jahr, y = `Milliarden Franken`)) + # Plot aufbauen
      geom_line(color = "firebrick3", linewidth = 1.5) +
      labs(x = "Jahr", y = "Millionen Franken", title = "Langfristige BIP Entwicklung") +
      scale_y_continuous(labels = label_comma(big.mark = "'")) +
      theme_minimal(base_size = 12)
    
    # Plotly Plot aufrufen
    ggplotly(plot2)
  })
  
  
  
  ### Seite 1, Plot 3 ###
  
  output$bundesratsparteien <- renderPlotly({
    plot3 <- ggplot(bund_parteien, aes(x = Jahr, y = `Anzahl Sitze`, fill = Partei)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Entwicklung der Bundesratssitze seit 1848",
        x = "Jahr",
        y = "Anzahl Sitze",
        fill = "Partei"
      ) +
      theme_minimal(base_size = 12) +
      scale_fill_manual(values = c(
        "FDP" = "#0074BE",
        "CVP" = "#F28C00",
        "SP" = "#E30613",
        "SVP" = "#62B635",
        "BDP" = "#FFD700",
        "LP" = "#A7C7E7",
        "Andere" = "#D3D3D3")) +
      scale_x_continuous(breaks = seq(1848, 2024, by = 10)) +
      scale_y_continuous(breaks = seq(0, 7, by = 1))
    
    ggplotly(plot3)
  })
  

  # Seite Poltio, Plot: Staatsausgaben
  output$staatsausgaben <- renderPlotly({
    plot4 <- ggplot(staatsausgaben_long, aes(x = Jahr, y = Ausgaben, fill = Ausgabengebiet)) +
      geom_area() +
      labs(
        title = "Staatsausgaben nach Ausgabengebiet seit 1990",
        x = "Jahr",
        y = "Anteil"
      ) +
      scale_fill_manual(
        values = c("Beziehungen zum Ausland" = "#004586",
                   "Bildung und Forschung" = "#FF420E",
                   "Finanzen und Steuern" = "#FFD320",
                   "Gesundheit" = "#579D1C",
                   "Institutionelle und Finanzielle Voraussetzungen" = "#7E0021",
                   "Kultur und Freizeit" = "#83CAFF",
                   "Landwirtschaft und Ernährung" = "#314004",
                   "Sicherheit" = "#AECF00",
                   "Soziale Wohlfahrt" = "#4B1F6F",
                   "Umwelt und Raumordnung" = "#FF950E",
                   "Verkehr" = "#C5000B",
                   "Wirtschaft" = "#0084D1")
      ) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(breaks = seq(1990, 2028, by = 5)) +
      geom_rect(aes(xmin = 2025, xmax = 2028, ymin = 0, ymax = 1),
                fill = "grey70", alpha = 0.01, inherit.aes = FALSE)
    
    ggplotly(plot4)
  })
  
  
  # Seite Econ, Plot: Zentralbankunabhängigkeit
  output$zentralbank <- renderPlotly({
    plot_cbi <- ggplot(cbi_data, aes(x = Jahr, y = `CBI Index`)) +
      geom_line(color = "coral3") +
      theme_minimal(base_size = 10) +
      labs(
        title = "Entwicklung der Zentralbankunabhängigkeit",
        x = "Jahr",
        y = "Zentralbankunabhängigkeitsindex"
      ) +
      facet_wrap(~country)
    
    ggplotly(plot_cbi)
  })
  
  # Seite: Diverses, Plot: Hundenamen
  output$hundenamen <- renderPlot({
    ggplot(zh_dogs_final, aes(x = reorder(HundenameText, total), y = total)) +
      geom_col(fill = "blue4") +
      geom_text(aes(label = total), vjust = 2, color = "white") +
      labs(
        x = "Hundenamen",
        y = "Anzahl",
        title = "Zürichs beliebteste Hundenamen 2024"
      ) +
      theme_minimal(base_size = 15) +
      scale_color_manual(values = paletteer_dynamic("cartography::pastel.pal", 12))
  })
  
  # Seite: Diverses, Text: Anzahl Hundenamen
 text_ausgabe <- reactive({
   eingegebener_name <- input$meinhund # Var für eingegebenen Namen
   
   reihenmatch <- zh_dogs_sum %>% 
     filter(HundenameText == eingegebener_name) # Erstellt neuen df mit nur der Zeile des eingegebenen Namens
   
   if (nrow(reihenmatch) == 0){
     paste("Dieser Hundename kommt nicht im Datensatz vor")
   } else{
     paste("Dein Hundename kommt", reihenmatch$total, "Mal in Zürich vor.")
   }
 })
  
 output$anzahl_hundenamen <- renderText({text_ausgabe()})
  
 
 # Seite: Diverses, Plot: Leerwohnungen
 output$leerwohnungsziffer <- renderPlotly({
   plot_leerwohnungen <- ggplot(leerwohnungen, aes(x = Jahr, colour = factor(Kreis, levels = 1:12))) +
     labs(
       x = "Jahr",
       title = "Entwicklung der Wohnsituation in Zürich 2009 bis 2024",
       color = "Kreis"
     ) +
     theme_minimal() +
     scale_color_manual(values = paletteer_dynamic("cartography::pastel.pal", 12))
   
   if ("awt" %in% input$zhwhg) {
     plot_leerwohnungen <- plot_leerwohnungen + 
       geom_line(aes(y = `Anzahl Wohnungen`), linewidth = 1.2) +
       geom_point(aes(y = `Anzahl Wohnungen`), size = 3) +
       labs(y = "Anzahl Wohnungen Total")
   }
   
   if ("alw" %in% input$zhwhg) {
     plot_leerwohnungen <- plot_leerwohnungen + 
       geom_line(aes(y = `Anzahl Leerwohnungen`), linewidth = 1.2) +
       geom_point(aes(y = `Anzahl Leerwohnungen`), size = 3) +
       labs(y = "Anzahl leere Wohnungen")
   }
   
   if ("lwz" %in% input$zhwhg) {
     plot_leerwohnungen <- plot_leerwohnungen + 
       geom_line(aes(y = Leerwohnungsziffer), linewidth = 1.2) +
       geom_point(aes(y = Leerwohnungsziffer), size = 3) +
       labs(y = "Leerwohnungsziffer in Prozenten")
   }
   
   ggplotly(plot_leerwohnungen)
   
   
 })
 
 
 # Seite Econ, Plot Zürcher Vermögensverteilung
 
 # Reaktivfunktion zur Auswahl des Jahres
 ktzh_wealth_yearly <- reactive({
   ktzh_wealth[ktzh_wealth$steuerjahr == input$slider_year_ktzh_wealth, ]
 })
 
 
 # Column Plot
 output$ktzh_wealth_col <- renderPlot({
   
   # Transformation Reaktivfunktionsoutput in Data Frame
   ktzh_wealth_yearly <- ktzh_wealth_yearly()
   
   plot_ktzh_wealth_col <- ggplot(ktzh_wealth_yearly, aes(x = vermoegensklasse)) +
     labs(x = "Vermögensklasse") +
     theme_minimal(base_size = 15) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     scale_y_continuous(labels = comma)

     
   if ("numtaxpayers" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_col <- plot_ktzh_wealth_col +
       geom_col(aes(y = anzahl_pflichtige), fill = "mediumpurple3") +
       labs(y = "Anzahl Steuerpflichtige")
   }
   
   if ("wealth" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_col <- plot_ktzh_wealth_col +
       geom_col(aes(y = steuerbares_vermoegen_in_mio), fill = "mediumpurple3") +
       labs(y = "Steuerbares Vermögen in Millionen")
   }
   
   if ("sharetaxpayers" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_col <- plot_ktzh_wealth_col +
       geom_col(aes(y = anteil_pflichtige), fill = "mediumpurple3") +
       labs(y = "Anteil Steuerzahler:innen")
   }
   
   if ("sharewealth" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_col <- plot_ktzh_wealth_col +
       geom_col(aes(y = anteil_vermoegen), fill = "mediumpurple3") +
       labs(y = "Anteil am Gesamtvermögen")
   }
   
   if ("sharetax" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_col <- plot_ktzh_wealth_col +
       geom_col(aes(y = anteil_steuer), fill = "mediumpurple3") +
       labs(y = "Anteil an Vermögenssteuer")
   }
   plot_ktzh_wealth_col
 }) 
 
 # Kuchendiagramm
 output$ktzh_wealth_cake <- renderPlot({
   
   # Transformation Reaktivfunktionsoutput in Data Frame
   ktzh_wealth_yearly <- ktzh_wealth_yearly()
   
   plot_ktzh_wealth_cake <- ggplot(ktzh_wealth_yearly, aes(x = "", fill = vermoegensklasse)) +
     theme_void(base_size = 15) +
     scale_color_manual(values = paletteer_d("ggthemes::Hue_Circle")) +
     guides(fill = guide_legend(title = "Vermögensklasse"))
   
   if ("numtaxpayers" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_cake <- plot_ktzh_wealth_cake +
       geom_col(aes(y = anzahl_pflichtige)) +
       coord_polar(theta = "y") +
       labs(y = "Anzahl Steuerpflichtige",
            x = "")
   }
   
   if ("wealth" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_cake <- plot_ktzh_wealth_cake +
       geom_col(aes(y = steuerbares_vermoegen_in_mio)) +
       coord_polar(theta = "y") +
       labs(y = "Steuerbares Vermögen in Millionen")
   }
   
   if ("sharetaxpayers" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_cake <- plot_ktzh_wealth_cake +
       geom_col(aes(y = anteil_pflichtige)) +
       coord_polar(theta = "y") +
       labs(y = "Anteil Steuerzahler:innen")
   }
   
   if ("sharewealth" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_cake <- plot_ktzh_wealth_cake +
       geom_col(aes(y = anteil_vermoegen)) +
       coord_polar(theta = "y") +
       labs(y = "Anteil am Gesamtvermögen")
   }
   
   if ("sharetax" %in% input$select_y_ktzh_wealth){
     plot_ktzh_wealth_cake <- plot_ktzh_wealth_cake +
       geom_col(aes(y = anteil_steuer)) +
       coord_polar(theta = "y") +
       labs(y = "Anteil an Vermögenssteuer")
   }
   plot_ktzh_wealth_cake
 })
 
 
}






# Shiny App ----
shinyApp(ui = ui, server = server)
