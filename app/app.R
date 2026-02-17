# Potrzebne pakiety
library(shiny) # Pakiet do budowania aplikacji
library(dplyr) # Manipulacja danymi
library(ggplot2) # Tworzenie wykresów
library(scales) # Formatowanie osi - waluty
library(bslib) # Motywy graficzne - Google Fonts


# 1. WCZYTYWANIE I PRZYGOTOWANIE DANYCH 

# wczytanie surowych danych
dane1 <- read.csv("dane/gdp-per-capita-worldbank.csv") %>%
  rename(pkb_per_capita = "GDP.per.capita..PPP..constant.2021.international...")

dane2 <- read.csv("dane/happiness-cantril-ladder.csv")
kontynent <- read.csv("dane/kontynenty.csv")

# wybór danych w ramach czasowych 2014-2023 i usunięcie braków
pkb <- na.omit(dane1[dane1$Year >= 2014, ])
hap <- na.omit(dane2[dane2$Year <= 2023, ])

# Łączenie i czyszczenie danych 
pkb1 <- pkb %>% semi_join(hap, by=c("Entity","Year")) # [dplyr]
hap1 <- hap %>% semi_join(pkb, by=c("Entity","Year")) # [dplyr]
colnames(hap1)[4]<-"happiness"

# Dołączenie informacji o kontynentach 
# Dodajemy dane z 'kontynent' do 'pkb1' pasujące po kolumnie 'Entity'
pkb2 <- na.omit(pkb1 %>% left_join(kontynent, by="Entity")) # [dplyr]

# Tworzymy ostateczną ramkę danych, 
app_data <- pkb2 %>% 
  inner_join(hap1, by=c("Entity", "Year")) %>% # [dplyr]
  select(Entity, Year, Continent, pkb_per_capita, happiness) # [dplyr]

# Definiujemy stałe kolory dla kontynentów dla spójności
continent_colors <- c(
  "Africa" = "#66C2A5",
  "Asia" = "#FC8D62",
  "Europe" = "#8DA0CB",
  "North America" = "#E78AC3",
  "Oceania" = "#A6D854",
  "South America" = "#FFD92F"
)

# 2. DANE AGREGOWANE 

# Obliczenie średnich dla kontynentów 
continent_avg <- app_data %>%
  group_by(Continent, Year) %>% # [dplyr]
  summarise(
    pkb_per_capita = mean(pkb_per_capita, na.rm = TRUE),
    happiness = mean(happiness, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  rename(Group = Continent) # [dplyr]

# Przygotowanie danych dla 'Świata'
pkb_world_raw <- dane1[dane1$Entity == "World" & dane1$Year >= 2014, ]
hap_world_raw <- dane2[dane2$Entity == "World" & dane2$Year <= 2023, ]

pkb_world_avg <- pkb_world_raw %>%
  select(Year, pkb_per_capita)

hap_world_avg <- hap_world_raw %>%
  select(Year, "Cantril.ladder.score") %>%
  rename(happiness = "Cantril.ladder.score")

# Połączenie danych dla 'Świata' w jedną ramkę
world_avg <- pkb_world_avg %>%
  inner_join(hap_world_avg, by = "Year") %>%
  mutate(Group = "Świat") # [dplyr]

# Połączenie danych kontynentów i świata
avg_data <- rbind(world_avg, continent_avg)

# Zmiana na factor, potrzebne do poprawnej kolejności na wykresach
avg_data$YearFactor <- as.factor(avg_data$Year) 
avg_data$Group <- as.factor(avg_data$Group)
# Ustawienie "Świat" na poziom referencyjny
avg_data$Group <- relevel(avg_data$Group, ref = "Świat")


# 3. UI 
ui <- fluidPage(
  # korzystamy z [bslib] do ustawienia nowoczesnego motywu graficznego
  theme = bs_theme(
    bootswatch = "pulse", 
    version = 5,
    primary = "#6a1b9a", 
    base_font = font_google("Poppins") # wybieramy czcionkę z Google Fonts
  ),
  
  # [shiny] Dołączenie pliku CSS do specyfikacji wyglądu aplikacji
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dane/styles.css")),
  
  # [shiny] Tytuł
  # Zmieniamy zwykły div() na taki z klasą "text-primary"
  titlePanel(div(class = "text-primary", icon("chart-line"), " Interaktywna Analiza: Pieniądze a Szczęście")),
  
  # [shiny] Aplikacja z panelem bocznym i głównym
  sidebarLayout(
    # Panel boczny
    sidebarPanel(
      h3(class = "text-primary",icon("filter"), "Panel Kontrolny"),
      
      # Możliwość globalnego wyboru kontynentu, oprócz zakładki 'Zmiana w czasie' (korzystamy z ID)
      conditionalPanel(
        condition = "input.main_tabs != 'Zmiana w czasie'",
        selectInput("global_continent",
                    "Wybierz region:",
                    choices = c("Cały Świat", unique(app_data$Continent)),
                    selected = "Cały Świat"),
        
        sliderInput("global_year",
                    "Zakres lat:",
                    min = min(app_data$Year),
                    max = max(app_data$Year),
                    value = c(min(app_data$Year), max(app_data$Year)),
                    step = 1, sep = "", round = TRUE)
      ),
      # Odzielenie linią poziomą
      hr(),
      
      # Filtry specyficzne dla zakładek
      
      # Dla Zakładki 1: Wykres Punktowy
      conditionalPanel(
        condition = "input.main_tabs == 'Wykres Punktowy'",
        div(class = "alert alert-info", role = "alert",
            strong("Opcje linii trendu:"),
            radioButtons("trend_type", NULL,
                         choices = list("Brak" = "none", "Liniowa" = "lm", "Logarytmiczna" = "log", "Pierwiastkowa" = "sqrt"), 
                         selected = "log")
        )
      ),
      
      # Dla Zakładki 3: Rozkład i Nierówności
      conditionalPanel(
        condition = "input.main_tabs == 'Rozkład i Nierówności'",
        selectInput("metric_input", "Wybierz wskaźnik:",
                    choices = c("Poziom Szczęścia" = "happiness", "PKB per capita" = "pkb_per_capita"),
                    selected = "happiness")
      ),
      
      # Dla Zakładki 4: Zmiana w Czasie
      conditionalPanel(
        condition = "input.main_tabs == 'Zmiana w czasie'",
        radioButtons("time_metric", "Co analizować?",
                     choices = list("PKB per capita" = "pkb_per_capita", "Poziom Szczęścia" = "happiness"),
                     selected = "pkb_per_capita"),
        
        checkboxGroupInput("time_continents", "Wybierz regiony:",
                           choices = levels(avg_data$Group),
                           selected = avg_data$Group)
      ),
      
      div(style = "text-align: center; color: #888; font-size: 0.8em; margin-top: 20px;",
          "Dane: Our World in Data")
    ),
    
    # Panel główny
    mainPanel(
      # Ustawiamy ID, aby móc kontrolować globalny wybór 
      tabsetPanel(id = "main_tabs", 
                  
                  tabPanel("Wykres Punktowy", icon = icon("globe"),
                           br(), plotOutput("scatterPlot", height = "500px")),
                  
                  tabPanel("Statystyki", icon = icon("table"),
                           br(), verbatimTextOutput("summaryStats")), 
                  
                  tabPanel("Rozkład i Nierówności", icon = icon("chart-bar"),
                           br(), plotOutput("distPlot", height = "500px")),
                  
                  tabPanel("Zmiana w czasie", icon = icon("chart-line"),
                           br(), plotOutput("timePlot", height = "500px"))
      )
    )
  )
)


# 4. SERVER 
server <- function(input, output, session) {
  
  # [shiny] reactive - wykres zmienia się przy doborze kryteriów
  filtered_data_plot <- reactive({
    
    # [dplyr] Wybieramy zakres lat z suwaka
    df <- app_data %>%
      filter(Year >= input$global_year[1] & Year <= input$global_year[2])
    
    # Jeśli nie wybraliśmy "Cały Świat" to działamy na kontynencie
    if (input$global_continent != "Cały Świat") {
      df <- df %>% filter(Continent == input$global_continent)
    }
    return(df)
  })
  
  # Zakładka 1: Wykres Punktowy
  output$scatterPlot <- renderPlot({
    
    # Korzystamy z przefiltrowanych powyżej danych
    data <- filtered_data_plot()
    
    # Tytuł także się zmienia
    plot_title <- paste("Zależność dla:", input$global_continent, 
                        "(Lata:", input$global_year[1], "-", input$global_year[2], ")")
    
    # [ggplot2] Wykres
    p <- ggplot(data, aes(x = pkb_per_capita, y = happiness)) +
      geom_point(aes(color = Continent), alpha = 0.7, size = 3.5) +
      theme_minimal(base_size = 16) +
      labs(title = plot_title, x = "PKB per capita ($)", y = "Szczęście (0-10)") +
      # [scales] Formatowanie osi X dla $
      scale_x_continuous(labels = dollar_format()) +
      scale_color_manual(values = continent_colors) +
      theme(legend.position = "bottom")
    
    # Dodawanie linii trendu
    if (input$trend_type == "lm") {
      p <- p + geom_smooth(method = "lm", color = "#333", size=1.2)
    } else if (input$trend_type == "log") {
      p <- p + geom_smooth(method = "lm", formula = y ~ log(x), color = "#d32f2f", size=1.2)
    } else if (input$trend_type == "sqrt") { 
      p <- p + geom_smooth(method = "lm", formula = y ~ sqrt(x), color = "#7b1fa2", size=1.2)
    }
    print(p)
  })
  
  # Zakładka 2: Statystyki 
  # Dostajemy od razu statystykę dla wybranych wcześniej danych
  output$summaryStats <- renderPrint({
    summary_data <- filtered_data_plot() %>% 
      select(pkb_per_capita, happiness)
    
    cat("--- Analiza dla regionu:", input$global_continent, "---\n")
    summary(summary_data)
  })
  
  # Zakładka 3: Rozkład i Nierówności (BOXPLOT)
  output$distPlot <- renderPlot({
    
    # Bierzemy wszystkie kraje z wybranego zakresu lat
    data <- filtered_data_plot()
    
    # Wybór kolumn
    selected_cols_3 <- c("Continent", input$metric_input)
    data <- data[, selected_cols_3]
    colnames(data)[2] <- "Value"
    
    # 2. Ustawienie nazwy wskaźnika do tytułu
    metric_label <- ifelse(input$metric_input == "happiness", 
                           "Poziom Szczęścia (0-10)", "PKB per capita ($)")
    
    # Wykres Pudełkowy (Boxplot)
    p_box <- ggplot(data, aes(x = Continent, y = Value, fill = Continent)) +
      # Boxplot pokazuje medianę i kwartyle
      geom_boxplot(alpha = 0.7, outlier.shape = NA) + 
      # Jitter dodaje prawdziwe punkty (kraje)
      geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "#333333") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Nierówności w regionach:", metric_label),
           subtitle = paste("Rozkład danych dla lat:", input$global_year[1], "-", input$global_year[2]),
           x = "Region",
           y = metric_label) +
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_fill_manual(values = continent_colors)
    
    # Jeśli wybrano PKB to formatujemy dla $
    if (input$metric_input == "pkb_per_capita") {
      p_box <- p_box + scale_y_continuous(labels = dollar_format())
    }
    
    print(p_box)
  })
  
  # Zakładka 4: Zmiana w Czasie
  output$timePlot <- renderPlot({
    # Filtr wierszy
    data_time <- avg_data %>% filter(Group %in% input$time_continents)
    
    # Wybór kolumn
    selected_cols_4 <- c("Year", "Group", input$time_metric)
    data_time <- data_time[, selected_cols_4]
    colnames(data_time)[3] <- "Value"
    
    # Ustawienie nazwy wskaźnika do tytułu
    metric_label <- ifelse(input$time_metric == "happiness", "Poziom Szczęścia (0-10)", "PKB per capita ($)")
    
    # Wykres
    p_line <- ggplot(data_time, aes(x = Year, y = Value, color = Group)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) + 
      theme_minimal(base_size = 14) +
      labs(title = paste("Zmiana w czasie:", metric_label),
           subtitle = "Analiza trendów dla wybranych regionów",
           x = "Rok",
           y = metric_label,
           color = "Region") +
      scale_color_manual(values = c("Świat" = "#9575cd", continent_colors))
    
    # Jeśli wybrano PKB to formatujemy dla $
    if (input$time_metric == "pkb_per_capita") {
      p_line <- p_line + scale_y_continuous(labels = dollar_format())
    }
    
    print(p_line)
  })
}

shinyApp(ui, server)