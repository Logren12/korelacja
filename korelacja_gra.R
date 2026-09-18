# Instalacja pakietów (do /packages)
#loc <- normalizePath("./packages", mustWork = FALSE)
#if (!dir.exists(loc)) dir.create(loc)
#.libPaths(c(loc, .libPaths()))
#install.packages(c("shiny", "ggplot2", "shinyWidgets"),lib = loc)

library(shiny)
library(shinyWidgets)
library(ggplot2)

# UI
instrukcja = "Instrukcja:\n
  Wygeneruj wykres za pomocą przycisku \"Nowy wykres\". Następnie przewiń strzałkami lub wpisz przewidywaną korelację i naciśnij przycisk \"Zganij!\".
  Jeżeli podana wartość będzie w zakresie +- 0.05 od faktycznej, otrzymasz 100 punktów i regenerację serduszka, a w zakresie 0.1 - 50 punktów. Podanie korelacji różnej o więcej niż 0.1 poskutkuje utratą serduszka. Po utracie 3 serduszek gra się kończy.
  "
ui <- fluidPage(
  setBackgroundColor("darkseagreen"),
  titlePanel("Gra w zgadywanie korelacji"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "guess",
        label = "Korelacja",
        value = 0,
        min = -1,
        max = 1,
        step = 0.01,
        width = '100%'
      ),
      actionButton("następny","Nowy wykres"),
      uiOutput("zgadywanie"),
      uiOutput("reset"),
      uiOutput("następny"),
      textOutput("punkty"),
      textOutput("serce"),
      textOutput("koniec"),
      checkboxInput("odpowiedzi", "Pokaż odpowiedź"),
      helpText(instrukcja)
    ),
    # Main panel
    mainPanel(
      plotOutput(outputId = "wykresik"),
      textOutput("czydobrze"),
      textOutput("odpowiedź")
    )
  )
)

# server
server <- function(input, output, session) {
  # Początkowa konfiguracja:
  # Zmienne globalne do trzymania stanu gry
  PUNKTY <<- 0
  ŻYCIA <<- 3
  KOPIE <<- 0
  hp <- reactive({

  })

  # wczytywanie serduszek
  serce_emoji = renderText({rep(c('\u2764'), ŻYCIA)})
  output$serce <- serce_emoji


  # 1. losowanie parametrów wykresu:
  losowanie <- eventReactive(input$następny,{
    # losowanie współczynników funkcji liniowej
    a <- runif(1, min = 0, max = 20)
    b <- runif(1, min = -10, max = 10)

    # losowanie parametrów rozkładu normalnego do losowania wartości f liniowej
    par_roz <- data.frame(value = runif(2), row.names = c("śr1", "od1"))
    x <- data.frame(x = rnorm(200, par_roz["śr1",], par_roz["od1",]))

    # wartości funkcji liniowej dla wylosowanych argumentów
    y <- data.frame(y = a*x + b)

    # zaburzanie wartości przy pomocy funkcji
    add <- rnorm(200)
    z <- x + add

    # korelacja do zgadnięcia
    korelacja <- round(cor(x = x, y = z), digits = 2)

    # tworzenie ramki danych potrzebnych do wykresu
    dane <- data.frame(x,y,z, rep(korelacja, 200))
    colnames(dane)<-c("x", "y", "z", "k")

    return(dane)
  })


  # 2. rysowanie wykresu
  output$wykresik <- renderPlot({
    ggplot(data = losowanie(), aes(x = x, y = z))+
      geom_point()+
      theme_light() # najważniejsza część programu!
  })

  # 3. Tworzenie przycisku do odpowiadania:
  observeEvent(input$następny,
    {
      if(KOPIE < 1) {
        insertUI(
          selector = "#następny",
          where = "afterEnd",
          ui = actionButton("zgadywanie", label = "Zgadnij!")
          )
        KOPIE <<- KOPIE + 1
      }
      else {
        ŻYCIA <<- ŻYCIA - 1
        output$serce <- serce_emoji
        if(ŻYCIA == 0){
          replicate(KOPIE, {removeUI(selector='#zgadywanie', immediate = T)})
          removeUI(selector='#następny', immediate = T)
          showModal(
            modalDialog(
              title = "Koniec gry!",
              easyClose = TRUE,
              footer = NULL,
              paste0("Wynik: ", PUNKTY)
            )
          )
          insertUI(
            selector = "#następny",
            where = "afterEnd",
            ui = actionButton("reset", label = "Nowa Gra")
          )
          output$koniec <- renderText({paste0("Koniec gry!")})
        }
      }
    }
  )

  # 4. sprawdzenie poprawności odpowiedzi, bilans punktów i serduszek
  test <- eventReactive(input$zgadywanie,{
    if(abs(input$guess - losowanie()[3,4])<= 0.05 ){
      PUNKTY <<- PUNKTY + 100
      if(ŻYCIA < 3){
        ŻYCIA <<- ŻYCIA + 1
      }
      return(100)}
    if(abs(input$guess - losowanie()[3,4])<= 0.1 ){
      PUNKTY <<- PUNKTY + 50
      return(50)}
    else{
      ŻYCIA <<- ŻYCIA - 1
      if(ŻYCIA == 0){
        replicate(KOPIE, {removeUI(selector='#zgadywanie', immediate = T)})
        removeUI(selector='#następny', immediate = T)
        insertUI(selector = "#następny",
                 where = "afterEnd",
                 ui = actionButton("reset", label = "Nowa Gra"))
        output$koniec <- renderText({paste0("Koniec gry!")})
      }
      return(0)}
  })

  # 5. wypisywanie punktów, wyświetlanie serduszek i usuwanie przycisku do zgadywania
  observeEvent(input$zgadywanie, {
    output$punkty <- renderText({paste0("Punkty: ", PUNKTY)})
    output$serce <- serce_emoji
    replicate(KOPIE, {removeUI(selector='#zgadywanie', immediate = T)})
    KOPIE <<- 0
  })

  # 6. feedback i odpowiedź
  output$czydobrze <- renderText({paste0("Prawdziwa korelacja: ", losowanie()[1, "k"], ", podana korelacja: ", input$guess, ", wynik: +", test(), " punktów")})


  output$odpowiedź <- renderText({
  if (input$odpowiedzi) {
    paste0("Odpowiedź: ", losowanie()[1, "k"])
    }
  })

  # Nowa gra
  observeEvent(input$reset,{
    session$reload()
  })
}


shinyApp(ui = ui, server = server)
