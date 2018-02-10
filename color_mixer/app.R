library(shiny)
library(colourpicker)

RGB2HEX <- function(RGB) {
  toupper(paste0("#", paste(as.character(as.hexmode(RGB)), collapse = "")))
}

mnslFull <- read.csv2('munsell.csv')
TmatDF <- read.csv2('T_matrix.csv')
Tmat <- as.matrix(TmatDF[1:3, 2:37])
mnslChoice <- sort(c(1024, 10, 83, 1168, 1097, 662, 622, 481, 124, 123, 37, 1235))
mnslRef <- mnslFull[mnslChoice, ]
mnslRef$R <- pmax(0, pmin(255, mnslRef$R))
mnslRef$G <- pmax(0, pmin(255, mnslRef$G))
mnslRef$B <- pmax(0, pmin(255, mnslRef$B))
mnslRef$hex <- apply(mnslRef[, c("R", "G", "B")],
                     MARGIN = 1,
                     FUN = RGB2HEX)

addGamma <- function(x) {
  ifelse(x < 0.0031308,
         12.92 * x,
         1.055 * x^(1/2.4) - 0.055
  )
}

colorMixer <- function(curves, Tmat) {
  mixedCurve <- apply(curves, 2, function(x) exp(mean(log(x))))
  linRGB <- Tmat %*% mixedCurve
  sRGB <- addGamma(linRGB)
  RGB <- round(pmax(0, pmin(255, sRGB * 255)))
  HEX <- RGB2HEX(RGB)
  return(HEX)
}

ui <- fluidPage(
  
  titlePanel("Mieszacz kolorów"),
  
  sidebarLayout(
    sidebarPanel(
      lapply(1:12, function(x) 
        fluidRow(
          column(10, colourInput(paste0('cl', x),
                                 paste0(x, '. kolor'),
                                 showColour = 'back',
                                 palette = "limited",
                                 allowedCols = mnslRef$hex,
                                 value = mnslRef$hex[x])),
          column(2, checkboxInput(paste0('ch', x), '', value = ifelse(x < 3, T, F)))
        )
      )
    ),
    
    mainPanel(
      uiOutput('outColorUI')
    )
  )
)

server <- function(input, output, session) {
  
  mixedColor <- reactive({
    selectedColors <- sapply(1:12, function(x) input[[paste0('cl', x)]])
    activeCheckbox <- sapply(1:12, function(x) input[[paste0('ch', x)]])
    activeColors <- selectedColors[activeCheckbox]
    if (length(activeColors) == 0) {
      ret <- '#FFFFFF'
    } else {
      selectedMnsl <- mnslRef[mnslRef$hex %in% activeColors, 6:41]
      ret <- colorMixer(selectedMnsl, Tmat)
    }
    ret
  })
  
  output$outColorUI <- renderUI({
    colourInput('outColor', 'Wynik mieszania', value = mixedColor(), showColour = 'back')
  })
}

shinyApp(ui = ui, server = server)