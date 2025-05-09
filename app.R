library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(readxl)
library(plotly)

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



# User Interface ----
ui <- page_navbar(
  title = "Shining",
  nav_panel(title = "Willkommen",
            page_fillable(
              layout_columns(
                layout_column_wrap(
                  width = 1,
                  card(
                    card_header("Hi there"),
                    card_body("Hier kommt ein bisschen Text")
                  )
                ),
                layout_column_wrap(
                  width = 1,
                  card(
                    card_header("Ein Graph, welcher mir gefällt"),
                    card_image("data/ft_gender_divergence.jpg",
                               style = "display: block; margin-left: auto; margin-right: auto; width: 70%;",
                               href = "https://www.ft.com/content/29fd9b5c-2f35-41bf-9d4c-994db4e12998"),
                    card_body("Diese graphische Aufarbeitung von John Rupert-Murdoch bringt ein Phänomen auf den Punkt, 
                              welches für mich als jungen Mann von grossem Interesse ist."),
                    card_footer("Quelle: Financial Times"),
                    style = "background-color: #FFF1E0"
                  ),
                  card(
                    card_header("Yapp"),
                    card_body("some yapping")
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
                        plotlyOutput(outputId = "zentralbank"))
            )
              
          
  ),
  nav_panel(title = "Politisches",
            navset_card_pill(
              nav_panel(title = "Bundesratsparteien",
                        plotlyOutput(outputId = "bundesratsparteien")
            ))),
  nav_spacer(),
  nav_item(input_dark_mode())
)



# Server Function ----
server <- function(input, output){
  
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
      theme_minimal(base_size = 15)
  })
  
  
  
  ### Seite 1 Plot 2 (Plotly) ###
  
  # Plot erstellen
  output$bip_lange_frist <- renderPlotly({
    plot2<- ggplot(bip_long, aes(x = Jahr, y = `Milliarden Franken`)) + # Plot aufbauen
      geom_line(color = "firebrick3", linewidth = 1.5) +
      labs(x = "Jahr", y = "Millionen Franken", title = "Langfristige BIP Entwicklung") +
      scale_y_continuous(labels = label_comma(big.mark = "'")) +
      theme_minimal(base_size = 15)
    
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
      theme_minimal(base_size = 15) +
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
  
  
  # Seite 1, Plot 4
  
  output$staatsausgaben <- renderPlotly({
    plot4 <- ggplot(staatsausgaben_long, aes(x = Jahr, y = Ausgaben, fill = Ausgabengebiet)) +
      geom_area() +
      labs(
        title = "Staatsausgaben nach Ausgabengebiet seit 1990",
        x = "Jahr",
        y = "Anteil"
      ) +
      scale_fill_manual(
        values = c("Beziehungen zum Ausland" = "antiquewhite3",
                   "Bildung und Forschung" = "cornflowerblue",
                   "Finanzen und Steuern" = "bisque1",
                   "Gesundheit" = "darkolivegreen3",
                   "Institutionelle und Finanzielle Voraussetzungen" = "darksalmon",
                   "Kultur und Freizeit" = "darkorchid",
                   "Landwirtschaft und Ernährung" = "darkslategrey",
                   "Sicherheit" = "gold2",
                   "Soziale Wohlfahrt" = "deeppink3",
                   "Umwelt und Raumordnung" = "deepskyblue3",
                   "Verkehr" = "grey80",
                   "Wirtschaft" = "cyan2")
      ) +
      theme_minimal(base_size = 15) +
      scale_x_continuous(breaks = seq(1990, 2028, by = 5)) +
      geom_rect(aes(xmin = 2025, xmax = 2028, ymin = 0, ymax = 1),
                fill = "grey70", alpha = 0.01, inherit.aes = FALSE)
    
    ggplotly(plot4)
  })
  
  
  # Seite 1, Plot 5
  output$zentralbank <- renderPlotly({
    plot_cbi <- ggplot(cbi_data, aes(x = Jahr, y = `CBI Index`)) +
      geom_line(color = "coral3") +
      theme_minimal(base_size = 15) +
      labs(
        title = "Entwicklung der Zentralbankunabhängigkeit",
        x = "Jahr",
        y = "Zentralbankunabhängigkeitsindex"
      ) +
      facet_wrap(~country)
    
    ggplotly(plot_cbi)
  })
  
}







# Shiny App ----
shinyApp(ui = ui, server = server)
