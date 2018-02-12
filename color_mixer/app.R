library(shiny)
library(colourpicker)
library(shinyjs)

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
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mojaPlastelinka.css")
  ),
  
  titlePanel("Mieszacz kolorów"),
  fluidRow(
    column(6,
      tags$button("Plasteliny", class = "toggle"),
      tags$div(
        class="container",
        tags$div(
          id = "target",
          
          lapply(1:12, function(x) {
            tags$label(
              tags$input(type = "checkbox"),
              tags$img(id = sprintf("pl%0.2d", x),
                       class="plastelinka",
                       src=sprintf("plastelinka/pl%0.2d.png", x))
            )
          })
        ),
        tags$div(id = "bottom")
      )
    ),
    column(
      6,
      tags$div(
        id = "mixResult",
        class = "containerMix",
        tags$img(class = "mix", src = "plastelinka/plMix.png")
      )
    )
  ),
  tags$script(src="myJava.js")
)

server <- function(input, output, session) {
  
  mixedColor <- reactive({
    activeColors <- sapply(1:12, function(x) {
      curr <- pl[[sprintf('pl%0.2d', x)]]
      if (curr > 0)
        TRUE
      else
        FALSE
    })
    if (all(!activeColors)) {
      ret <- '#FFFFFF'
    } else {
      selectedMnsl <- mnslRef[activeColors, 6:41]
      ret <- colorMixer(selectedMnsl, Tmat)
    }
    ret
  })
  
  applyMix <- reactive({
    sprintf('document.getElementById("mixResult").style.backgroundColor="%s";',
            mixedColor())
  })
  
  pl <- do.call(reactiveValues, args = setNames(lapply(1:12, function(x) -1), sprintf('pl%0.2d', 1:12)))
  
  onclick("pl01", {pl$pl01 <- -pl$pl01; runjs(applyMix())})
  onclick("pl02", {pl$pl02 <- -pl$pl02; runjs(applyMix())})
  onclick("pl03", {pl$pl03 <- -pl$pl03; runjs(applyMix())})
  onclick("pl04", {pl$pl04 <- -pl$pl04; runjs(applyMix())})
  onclick("pl05", {pl$pl05 <- -pl$pl05; runjs(applyMix())})
  onclick("pl06", {pl$pl06 <- -pl$pl06; runjs(applyMix())})
  onclick("pl07", {pl$pl07 <- -pl$pl07; runjs(applyMix())})
  onclick("pl08", {pl$pl08 <- -pl$pl08; runjs(applyMix())})
  onclick("pl09", {pl$pl09 <- -pl$pl09; runjs(applyMix())})
  onclick("pl10", {pl$pl10 <- -pl$pl10; runjs(applyMix())})
  onclick("pl11", {pl$pl11 <- -pl$pl11; runjs(applyMix())})
  onclick("pl12", {pl$pl12 <- -pl$pl12; runjs(applyMix())})
}

shinyApp(ui = ui, server = server)