library(shiny)
library(shinydashboard)
library(DT)
library(tuneR)
library(SynchWave)
library(plotly)

hamming <- function(N, a = 0.53836, b = 0.46164) {
  a - b * cos(2*pi*0:(N-1) / (N - 1))
}

getNote <- function(frq, snd) {
  closest.freq <- which.min(abs(snd$Frequency - frq))
  return(snd[closest.freq, "Note"])
}

getMaxima <- function(x) {
  which(diff(sign(diff(x)))==-2)+1
}

minMaxNormalize <- function(x) {
  mx <- max(x)
  mn <- min(x)
  return((x - mn) / (mx - mn))
}
makeDfPlot <- function(wav, snd, bck = "sound/bck_voice.wav") {
  bckSnd <- readWave(bck)@left[1:132000]
  wv <- wav@left[1:132000]
  N <- length(wv)
  fft_wav <- fft((wv-bckSnd) * hamming(N))
  dF <- wav@samp.rate/N
  
  df.plot <- data.frame(Frequency = dF*1:round(N/2),
                        Amplitude = abs((fft_wav[1:round(N/2)])^2))
  df.plot$Smoothed <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude, order = 16)$fitted))
  df.plot$Note <- sapply(df.plot$Frequency, getNote, snd)
  return(df.plot)
}
  
makePlot <- function(df.plot, tit = "Widmo") {
  # df.plot <- df.plot[getMaxima(df.plot$Smoothed), ]
  p <- plot_ly(data = df.plot, x = ~Frequency, y = ~Smoothed,
               type = 'scatter', mode = 'lines',
               text = ~paste("Freq:", round(Frequency), "Hz<br>Note:", Note))
  p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
              yaxis = list(type = 'log', title = 'Amplituda [j.u.]'),
              title = tit)
  return(p)
}

makeCompPlot <- function(wav1, wav2, snd, tit = "Widmo", bck = "sound/bck_voice.wav") {
  bckSnd <- readWave(bck)@left[1:132000]
  wv1 <- wav1@left[1:132000]
  wv2 <- wav2@left[1:132000]
  N <- length(wv1)
  dF <- wav1@samp.rate/N
  stopifnot(N == length(wv2))
  fft_wav1 <- fft((wv1 - bckSnd) * hamming(N))
  fft_wav2 <- fft((wv2 - bckSnd) * hamming(N))
  
  df.plot <- data.frame(Frequency = dF*1:round(N/2),
                        Amplitude1 = abs((fft_wav1[1:round(N/2)])^2),
                        Amplitude2 = abs((fft_wav2[1:round(N/2)])^2))
  df.plot$Smoothed1 <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude1, order = 16)$fitted))
  df.plot$Smoothed2 <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude2, order = 16)$fitted))
  df.plot$Note <- sapply(df.plot$Frequency, getNote, snd)
  
  p <- plot_ly(data = df.plot, x = ~Frequency, y = ~Smoothed1, name = "Pierwszy dźwięk",
               type = 'scatter', mode = 'lines', source = "B",
               text = ~paste("Freq:", round(Frequency), "Hz<br>Note:", Note))
  p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
              yaxis = list(type = 'log', title = 'Amplituda [j.u.]'),
              title = tit)
  p <- add_trace(p, y = ~Smoothed2, name = 'Drugi dźwięk', mode = 'lines')
  p
}

snd <- read.csv("sound_list.csv", stringsAsFactors = FALSE)
smp <- read.csv("sample_list.csv", stringsAsFactors = FALSE)
# p <- makePlot(hmn_Dnl_A, snd, tit = "Widmo dźwięku A, Daniel")
# p

ui <- dashboardPage(
  dashboardHeader(title = "Analiza barwy dźwięku"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Widmo", tabName = "spec", icon = icon("signal")),
      menuItem("Porównaj", tabName = "comp", icon = icon("object-group"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'spec',
        fluidRow(
          box(width = 3,
            selectInput('src', 'Źródło dźwięku', choices = unique(smp$source)),
            selectInput('dsc', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
            selectInput('ptc', 'Wysokość dźwięku', choices = unique(smp$pitch)),
            actionButton('btn', 'Narysuj wykres')
          ),
          box(width = 9,
            plotlyOutput('plt'),
            DT::dataTableOutput('tbl'),
            actionButton('rst', 'Resetuj tabelkę')
          )
        )
      ),
      
      tabItem(tabName = 'comp',
        fluidRow(
          column(width = 3,
            box(width = NULL, title = 'Pierwszy dźwięk',
              selectInput('src1', 'Źródło dźwięku', choices = unique(smp$source)),
              selectInput('dsc1', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
              selectInput('ptc1', 'Wysokość dźwięku', choices = unique(smp$pitch))
            ),
            box(width = NULL, title = 'Drugi dźwięk',
              selectInput('src2', 'Źródło dźwięku', choices = unique(smp$source)),
              selectInput('dsc2', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
              selectInput('ptc2', 'Wysokość dźwięku', choices = unique(smp$pitch)),
              actionButton('btn2', 'Narysuj wykres')
            )
          ),
          column(width = 9,
            box(width = NULL, plotlyOutput('plt2'))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  wav <- reactive(readWave(smp$path[smp$source == input$src & smp$snd_desc == input$dsc & smp$pitch == input$ptc]))
  tit <- reactive(sprintf("Widmo dźwięku: %s, o źródle: %s", input$dsc, input$src))
  selPnt <- reactiveVal(data.frame(Note = character(0),
                                   Frequency = numeric(0),
                                   Smoothed = numeric(0)))
  selectedDf <- reactiveVal()
  
  dfPlot <- eventReactive(input$btn, {
    makeDfPlot(wav(), snd)
  })
  observeEvent(input$src, {
    updateSelectInput(session, 'dsc',
                      choices = smp[smp$source == input$src, 'snd_desc'])
  })
  observeEvent(input$btn, {
    req(dfPlot())
    output$plt <- renderPlotly(makePlot(dfPlot(), tit()))
  })
  observeEvent(event_data('plotly_click'), { 
    req(dfPlot())
    df <- dfPlot()
    clck <- event_data('plotly_click')
    
    rw <- df[round(df$Frequency, 2) == round(clck$x, 2),
             c('Note', 'Frequency', 'Smoothed')]
    selPnt(rbind(selPnt(), rw))
    ret <- selPnt()
    ret <- ret[order(ret$Frequency), ]
    ret$RelAmp <- round(10*log10(ret$Smoothed / ret$Smoothed[1]), 3)
    ret$Smoothed <- round((ret$Smoothed), 3)
    ret$Frequency <- round(ret$Frequency)
    colnames(ret) <- c('Nuta', 'Częstotliwość', 'Amplituda', 'Amplituda względna dB')
    selectedDf(ret)
  })
  observeEvent(input$rst, {
    selPnt(data.frame(Note = character(0),
                      Frequency = numeric(0),
                      Smoothed = numeric(0)))
    ret <- selPnt()
    ret$RelAmp <- numeric(0)
    colnames(ret) <- c('Nuta', 'Częstotliwość', 'Amplituda (log)', 'Amplituda względna (log/log)')
    selectedDf(ret)
  })
  output$tbl <- DT::renderDataTable({
    req(selectedDf())
    selectedDf()
  })
  
  wav1 <- reactive(readWave(smp$path[smp$source == input$src1 & smp$snd_desc == input$dsc1 & smp$pitch == input$ptc1]))
  wav2 <- reactive(readWave(smp$path[smp$source == input$src2 & smp$snd_desc == input$dsc2 & smp$pitch == input$ptc2]))
  tit2 <- "Porównanie widm dźwięków"
  observeEvent(input$src1, {
    updateSelectInput(session, 'dsc1',
                      choices = smp[smp$source == input$src1, 'snd_desc'])
  })
  observeEvent(input$src2, {
    updateSelectInput(session, 'dsc2',
                      choices = smp[smp$source == input$src2, 'snd_desc'])
  })
  
  observeEvent(input$btn2, {
    output$plt2 <- renderPlotly(makeCompPlot(wav1(), wav2(), snd, tit2))
  })
}

shinyApp(ui = ui, server = server)
