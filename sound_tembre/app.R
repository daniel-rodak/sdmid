library(shiny)
library(shinydashboard)
library(DT)
library(tuneR)
library(SynchWave)
library(plotly)
library(dplyr)

spinner <- function(spinner.path) {
  if (isTruthy(spinner.path)) {
    conditionalPanel(
      condition = "($('html').hasClass('shiny-busy'))",
      tags$div(id = "spinnerLoading",
               style = "margin: 0px; padding: 0px;
               position: fixed; right: 0px; top: 0px;
               width: 100%; height: 100%; z-index:30001; opacity: 0.8;",
               tags$p(style = "position: absolute; top: 50%; left: 45%;",
                      tags$img(src = spinner.path)))
    )
  } else {
    NULL
  }
}

hamming <- function(N, a = 0.53836, b = 0.46164) {
  a - b * cos(2*pi*0:(N-1) / (N - 1))
}

getNote <- function(frq, snd) {
  closest.freq <- which.min(abs(snd$Frequency - frq))
  return(snd[closest.freq, "Note"])
}

getMultiples <- function(wav.path, f, bck = "sound/bck_voice.wav") {
  wav <- readWave(wav.path)
  wv <- wav@left[1:132000]
  bckSnd <- readWave(bck)@left[1:132000]
  N <- length(wv)
  fft_wav <- fft((wv-bckSnd) * hamming(N))
  dF <- wav@samp.rate/N
  dfr <- data.frame(Frequency = dF*1:round(N/2),
                    Amplitude = abs((fft_wav[1:round(N/2)])^2))
  fMult <- f
  ret <- data.frame(Frequency = numeric(), Amplitude = numeric())
  while(fMult < 8000) {
    amps <- dfr[abs(dfr$Frequency - fMult) < 0.025 * fMult, 'Amplitude']
    ret <- rbind(ret, data.frame(Frequency = fMult, Amplitude = max(amps)))
    fMult <- fMult + f
  }
  ret
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
  df.plot$Smoothed <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude, order = 8)$fitted))
  df.plot$Note <- sapply(df.plot$Frequency, getNote, snd)
  return(df.plot)
}
  
makePlot <- function(df.plot, tit = "Widmo") {
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
  df.plot$Smoothed1 <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude1, order = 8)$fitted))
  df.plot$Smoothed2 <- minMaxNormalize(as.numeric(smooth::sma(df.plot$Amplitude2, order = 8)$fitted))
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

snd <- read.csv("note_list.csv", stringsAsFactors = FALSE)
smp <- read.csv("sample_list.csv", stringsAsFactors = FALSE)
selPntTemplate <- data.frame(Note = character(0), Frequency = numeric(0), Smoothed = numeric(0))
selectedDfTemplate <- function() {
  ret <- data.frame(Note = character(0),
                    Frequency = numeric(0),
                    Smoothed = numeric(0),
                    RelAmp = numeric(0))
  colnames(ret) <- colnames(ret) <- c('Nuta', 'Częstotliwość', 'Amplituda', 'Amplituda względna [dB]')
  ret
}
soundCompFrameTemplate <- data.frame(
  SoundID = character(),
  Source = character(),
  Desc = character(),
  Frequency = numeric(),
  RelAmp = numeric()
)

ui <- dashboardPage(
  dashboardHeader(title = "Analiza barwy dźwięku"),
  dashboardSidebar(
    sidebarMenu(id = 'menu1',
      menuItem("Widmo", tabName = "spec", icon = icon("signal")),
      menuItem("Porównaj", tabName = "comp", icon = icon("object-group")),
      menuItem("Charakterystyka", tabName = "char", icon = icon("braille")),
      menuItem("Automatyczna analiza", tabName = 'auto', icon = icon('magic'))
    ),
    conditionalPanel(
      condition = "input.menu1 == 'spec'",
      selectInput('src', 'Źródło dźwięku', choices = unique(smp$source)),
      selectInput('dsc', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
      selectInput('ptc', 'Wysokość dźwięku', choices = unique(smp$pitch)),
      actionButton('btn', 'Narysuj wykres')
    ),
    conditionalPanel(
      condition = "input.menu1 == 'comp'",
      menuItem("Pierwszy dźwięk", startExpanded = TRUE,
        selectInput('src1', 'Źródło dźwięku', choices = unique(smp$source)),
        selectInput('dsc1', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
        selectInput('ptc1', 'Wysokość dźwięku', choices = unique(smp$pitch))
      ),
      menuItem('Drugi dźwięk', startExpanded = TRUE,
        selectInput('src2', 'Źródło dźwięku', choices = unique(smp$source)),
        selectInput('dsc2', 'Rodzaj dźwięku', choices = unique(smp$snd_desc)),
        selectInput('ptc2', 'Wysokość dźwięku', choices = unique(smp$pitch))
      ),
      actionButton('btn2', 'Narysuj wykres')
    ),
    conditionalPanel(
      condition = "input.menu1 == 'auto'",
      selectInput('ptcAuto', 'Wysokość dźwięku', choices = unique(smp$pitch))
    ),
    tags$div(id = 'spinnerAttr',
             style = 'position: fixed; bottom: 0px; margin: 1%;',
             tags$p(tags$a('spinner by loading.io', href = 'https://loading.io/')))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'spec',
        spinner('Ellipsis.svg'),
        fluidRow(
          box(width = 12, plotlyOutput('plt'))
        ),
        fluidRow(
          box(width = 12, title = "Tabelka z zaznaczonymi pikami",
            DT::dataTableOutput('tbl'),
            actionButton('rst', 'Resetuj tabelkę'),
            actionButton('add', 'Dodaj do charakterystyki')
          )
        )
      ),
      
      tabItem(tabName = 'comp',
        spinner('Ellipsis.svg'),
        fluidRow(
          box(width = 12, plotlyOutput('plt2'))
        )
      ),
      
      tabItem(tabName = 'char',
        spinner('Ellipsis.svg'),
        fluidRow(
          box(width = 12, title = 'Charakterystyka wszystkich dźwięków',
            plotlyOutput('plt3')
          )
        ),
        fluidRow(
          box(width = 6, title = 'Charakterystka rodzajów dźwięków',
            plotlyOutput('pltdsc')    
          ),
          box(width = 6, title = 'Charakterystka źródeł dźwięków',
            plotlyOutput('pltsrc')    
          )
        ),
        fluidRow(
          box(width = 12, title = "Tabela z danymi:",
            DT::dataTableOutput('tbl2'),
            actionButton('rst2', 'Resetuj tabelkę')
          )
        )
      ),
      
      tabItem(tabName = 'auto',
        spinner('Ellipsis.svg'),
        fluidRow(
          box(width = 12, title = 'Charakterystyka wszystkich dźwięków',
              plotlyOutput('plt3Auto')
          )
        ),
        fluidRow(
          box(width = 6, title = 'Charakterystka rodzajów dźwięków',
              plotlyOutput('pltdscAuto')    
          ),
          box(width = 6, title = 'Charakterystka źródeł dźwięków',
              plotlyOutput('pltsrcAuto')    
          )
        ),
        fluidRow(
          box(width = 12, title = "Tabela z danymi:",
              DT::dataTableOutput('tbl2Auto')
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

# spec tab ----------------------------------------------------------------

  wav <- reactive(readWave(smp$path[smp$source == input$src & smp$snd_desc == input$dsc & smp$pitch == input$ptc]))
  selPnt <- reactiveVal(selPntTemplate)
  selectedDf <- reactiveVal(selectedDfTemplate())
  soundCompFrame <- reactiveVal(soundCompFrameTemplate)
  
  dfPlot <- eventReactive(input$btn, {
    makeDfPlot(wav(), snd)
  })
  observeEvent(input$src, {
    dscCache <- input$dsc
    chcs <- smp[smp$source == input$src, 'snd_desc']
    if (!(dscCache %in% chcs)) {
      dscCache <- chcs[1]
    }
    updateSelectInput(session, 'dsc',
                      choices = smp[smp$source == input$src, 'snd_desc'],
                      selected = dscCache)
  })
  observeEvent(input$btn, {
    req(dfPlot())
    tit <- sprintf("Widmo dźwięku: %s, o źródle: %s", input$dsc, input$src)
    output$plt <- renderPlotly(makePlot(dfPlot(), tit))
    # Reset
    selPnt(selPntTemplate)
    selectedDf(selectedDfTemplate())
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
    colnames(ret) <- c('Nuta', 'Częstotliwość', 'Amplituda', 'Amplituda względna [dB]')
    selectedDf(ret)
  })
  observeEvent(input$rst, {
    selPnt(selPntTemplate)
    selectedDf(selectedDfTemplate())
  })
  output$tbl <- DT::renderDataTable({
    req(selectedDf())
    selectedDf()
  })
  observeEvent(input$add, {
    ret <- selPnt()
    ret <- ret[order(ret$Frequency), ]
    ret$RelAmp <- 10*log10(ret$Smoothed / ret$Smoothed[1])
    ret$SoundID <- sprintf('%s_%s_%s', input$src, input$dsc, input$ptc)
    ret$Source <- input$src
    ret$Desc <- input$dsc
    soundCompFrame(rbind(soundCompFrame(), ret[, c('SoundID', 'Source', 'Desc', 'Frequency', 'RelAmp')]))
    # Reset
    selPnt(selPntTemplate)
    selectedDf(selectedDfTemplate())
  })

# comp tab ----------------------------------------------------------------

  observeEvent(input$src1, {
    dscCache <- input$dsc1
    chcs <- smp[smp$source == input$src1, 'snd_desc']
    if (!(dscCache %in% chcs)) {
      dscCache <- chcs[1]
    }
    updateSelectInput(session, 'dsc1',
                      choices = smp[smp$source == input$src, 'snd_desc'],
                      selected = dscCache)
  })
  observeEvent(input$src2, {
    dscCache <- input$dsc2
    chcs <- smp[smp$source == input$src2, 'snd_desc']
    if (!(dscCache %in% chcs)) {
      dscCache <- chcs[1]
    }
    updateSelectInput(session, 'dsc2',
                      choices = smp[smp$source == input$src2, 'snd_desc'],
                      selected = dscCache)
  })
  observeEvent(input$btn2, {
    wav1 <- readWave(smp$path[smp$source == input$src1 & smp$snd_desc == input$dsc1 & smp$pitch == input$ptc1])
    wav2 <- readWave(smp$path[smp$source == input$src2 & smp$snd_desc == input$dsc2 & smp$pitch == input$ptc2])
    tit2 <- "Porównanie widm dźwięków"
    output$plt2 <- renderPlotly(makeCompPlot(wav1, wav2, snd, tit2))
  })

# char tab ----------------------------------------------------------------

  output$tbl2 <- DT::renderDataTable({
    req(soundCompFrame())
    ret <- soundCompFrame()
    ret$Frequency <- round(ret$Frequency)
    ret$RelAmp <- round(ret$RelAmp, 3)
    colnames(ret) <- c('ID dźwięku', 'Źródło', 'Rodzaj', 'Częstotliwość', 'Amplituda względna [dB]')
    ret
  })
  observeEvent(input$rst2, {
    soundCompFrame(soundCompFrameTemplate)
  })
  output$plt3 <- renderPlotly({
    req(soundCompFrame())
    p <- plot_ly(data = soundCompFrame(), x = ~Frequency, y = ~RelAmp, color = ~SoundID)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })
  output$pltdsc <- renderPlotly({
    req(soundCompFrame())
    dfr <- soundCompFrame() %>% 
      mutate(FrequencyRnd = exp(round(log(Frequency), 1))) %>%
      group_by(FrequencyRnd, Desc) %>%
      summarise(RelAmp = mean(RelAmp),
                Frequency = mean(Frequency)) %>%
      as.data.frame()
    p <- plot_ly(data = dfr, x = ~Frequency, y = ~RelAmp, color = ~Desc)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })
  output$pltsrc <- renderPlotly({
    req(soundCompFrame())
    dfr <- soundCompFrame() %>% 
      mutate(FrequencyRnd = exp(round(log(Frequency), 1))) %>%
      group_by(FrequencyRnd, Source) %>%
      summarise(RelAmp = mean(RelAmp),
                Frequency = mean(Frequency)) %>%
      as.data.frame()
    p <- plot_ly(data = dfr, x = ~Frequency, y = ~RelAmp, color = ~Source)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })

# auto tab ----------------------------------------------------------------

  autoCompFrame <- reactive({
    srcs <- smp[grepl("Damski|Meski", smp$source) & smp$pitch == input$ptcAuto, ]
    pitch <- ifelse(input$ptcAuto == 'C4', 261.63, 349.23)
    ret <- soundCompFrameTemplate
    for(i in 1:nrow(srcs)) {
      currDf <- getMultiples(srcs$path[i], pitch)
      currDf$SoundID <- sprintf('%s_%s_%s', srcs$source[i], srcs$snd_desc[i], srcs$pitch[i])
      currDf$Source <- srcs$source[i]
      currDf$Desc <- srcs$snd_desc[i]
      currDf$RelAmp <- 10*log10(currDf$Amplitude / currDf$Amplitude[1])
      ret <- rbind(ret, currDf[, c('SoundID', 'Source', 'Desc', 'Frequency', 'RelAmp')])
    }
    ret
  })
  output$tbl2Auto <- DT::renderDataTable({
    req(autoCompFrame())
    ret <- autoCompFrame()
    ret$Frequency <- round(ret$Frequency)
    ret$RelAmp <- round(ret$RelAmp, 3)
    colnames(ret) <- c('ID dźwięku', 'Źródło', 'Rodzaj', 'Częstotliwość', 'Amplituda względna [dB]')
    ret
  })
  output$plt3Auto <- renderPlotly({
    req(autoCompFrame())
    p <- plot_ly(data = autoCompFrame(), x = ~Frequency, y = ~RelAmp, color = ~SoundID)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })
  output$pltdscAuto <- renderPlotly({
    req(autoCompFrame())
    dfr <- autoCompFrame() %>% 
      mutate(FrequencyRnd = exp(round(log(Frequency), 1))) %>%
      group_by(FrequencyRnd, Desc) %>%
      summarise(RelAmp = mean(RelAmp),
                Frequency = mean(Frequency)) %>%
      as.data.frame()
    p <- plot_ly(data = dfr, x = ~Frequency, y = ~RelAmp, color = ~Desc)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })
  output$pltsrcAuto <- renderPlotly({
    req(autoCompFrame())
    dfr <- autoCompFrame() %>% 
      mutate(FrequencyRnd = exp(round(log(Frequency), 1))) %>%
      group_by(FrequencyRnd, Source) %>%
      summarise(RelAmp = mean(RelAmp),
                Frequency = mean(Frequency)) %>%
      as.data.frame()
    p <- plot_ly(data = dfr, x = ~Frequency, y = ~RelAmp, color = ~Source)
    p <- add_lines(p)
    p <- layout(p, xaxis = list(type = 'log', range = c(log10(200), log10(10000)), title = "Częstotliwość [Hz]"),
                yaxis = list(title = 'Amplituda względna [dB]'))
    p
  })
}

shinyApp(ui = ui, server = server)
