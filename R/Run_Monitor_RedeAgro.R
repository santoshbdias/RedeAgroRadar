#' Iniciar o Painel de Monitoramento da 'RedeAgro' ('RedeAgroRadar')
#'
#' @description Lanca um aplicativo 'shiny' interativo para configurar e monitorar
#' dados de radares meteorologicos da Rede Agropesquisa (Paraná). O painel
#' permite buscar grupos na API do 'Telegram' (<https://core.telegram.org/bots/api>),
#' testar o radar em tempo real e ativar um "robô" de monitoramento que roda a cada
#' 10 minutos para enviar alertas baseados na intensidade da chuva.
#'
#' @details
#' O aplicativo possui duas abas principais:
#' \itemize{
#'   \item \strong{Configuracao do Telegram}: Para conectar um bot, buscar o
#'   ID do chat alvo e estabelecer a ponte de comunicacao.
#'   \item \strong{Painel de Controle}: Permite definir a cidade monitorada,
#'   raio de analise em pixels, acionar o robo de envio de alertas ou fazer
#'   testes de radar de forma manual.
#' }
#'
#' @note Para que a execucao automatica funcione perfeitamente, e imprescindivel
#' que as funcoes internas auxiliares (\code{analisar_radar_PR},
#' \code{executar_alerta_telegram}, \code{gerar_imagem_radar} e
#' \code{status_diario}) estejam carregadas no mesmo ambiente.
#'
#' @param Token String de texto. O token de API gerado pelo '@BotFather' no 'Telegram'.
#' Pode ser deixado em branco e preenchido diretamente na interface web.
#' @param chatID String de texto. O ID do grupo ou chat do 'Telegram' que recebera
#' os alertas. Pode ser deixado em branco e pesquisado via painel.
#' @param limites_personalizados Lista nomeada. Define os limites globais de
#' gatilho para os canais Red e Blue lidos do radar. Espera-se os elementos:
#' \code{vr} (Alerta-Vermelho Red), \code{vb} (Alerta-Vermelho Blue),
#' \code{ar} (Alerta-Amarelo Red) e \code{ab} (Alerta-Amarelo Blue).
#' O padrao e \code{list(vr = 70, vb = 25, ar = 65, ab = 33)}. Chuva forte (vermelho) - (Red > 70 & Blue < 25)
#' Chuva leve (amarelo) - (Red > 65 & Blue < 33). E utilizado a leitura de RGB do mapa do simepar.
#'
#' @return Inicia uma interface web no navegador padrao rodando o app 'shiny'.
#' A funcao nao retorna um objeto do R para o console.
#'
#' @import shiny
#' @import bslib
#' @import httr
#'
#' @export
#'
#' @examples
#' # Todos os exemplos que abrem servidores locais (como o 'shiny') devem
#' # estar dentro de if(interactive()) para nao travarem os testes de servidores.
#'
#' if (interactive()) {
#'   # Exemplo 1: Iniciar o aplicativo sem credenciais iniciais
#'   Run_Monitor_RedeAgro()
#'
#'   # Exemplo 2: Iniciar com Token e ChatID pre-configurados para teste
#'   Run_Monitor_RedeAgro(
#'     Token = "123456:ABC-DEF15858ghIkl-zyx57W2v1u12345ew11",
#'     chatID = "-10686784567890",
#'     limites_personalizados = list(vr = 80, vb = 20, ar = 60, ab = 30)
#'   )
#' }

Run_Monitor_RedeAgro <- function(Token = "", chatID = "", limites_personalizados = list(vr = 70, vb = 25, ar = 65, ab = 33)) {

  buscar_chats_telegram <- function(token) {
    url <- paste0("https://api.telegram.org/bot", token, "/getUpdates")
    res <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
    if (is.null(res) || httr::status_code(res) != 200) return(NULL)

    conteudo <- httr::content(res, as = "parsed")
    if (length(conteudo$result) == 0) return(list())

    chats <- list()
    for (item in conteudo$result) {
      chat_info <- if (!is.null(item$message$chat)) item$message$chat else item$my_chat_member$chat
      if (!is.null(chat_info)) {
        nome <- if (!is.null(chat_info$title)) chat_info$title else if (!is.null(chat_info$first_name)) chat_info$first_name else "Chat Desconhecido"
        id <- as.character(chat_info$id)
        chats[[paste0(nome, " (ID: ", id, ")")]] <- id
      }
    }
    return(chats)
  }

  ui <- fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    titlePanel(tags$strong("RedeAgroRadar - Alertas de chuvas da Rede Agropesquisa")),
    hr(),

    tabsetPanel(id = "abas_principais", type = "pills",
                tabPanel("1. Configurar Telegram", value = "aba_config", icon = icon("cogs"), br(),
                         fluidRow(
                           column(6, bslib::card(
                             bslib::card_header("Passo a Passo: Como criar seu Bot", class = "bg-primary text-white"),
                             HTML("<ol><li>Abra o Telegram e busque por <b>@BotFather</b>.</li><li>Envie a mensagem <code>/newbot</code>.</li><li>De um nome ao seu bot.</li><li>De um nome de usuario terminando em 'bot'.</li><li>O BotFather enviara o <b>Token</b>.</li><li>Crie um grupo no Telegram, adicione o bot la e mande um 'Ola' no grupo.</li></ol>")
                           )),
                           column(6, bslib::card(
                             bslib::card_header("Conectar o Bot", class = "bg-success text-white"),
                             textInput("bot_token", "1. Token do Bot:", value = Token),
                             textInput("chat_id_final", "2. ID do Grupo/Chat:", value = chatID),
                             hr(), actionButton("btn_conectar", "Buscar IDs de Grupos", class = "btn-info"), br(), br(),
                             uiOutput("secao_chat_id")
                           ))
                         )
                ),
                tabPanel("2. Painel de Controle e Teste", value = "aba_painel", icon = icon("chart-line"), br(),
                         bslib::layout_sidebar(
                           sidebar = bslib::sidebar(width = 450, title = "Configuracoes do Alerta",
                                                    selectInput("cidade", "Megaparcela / Cidade:",
                                                                choices = c("Cianorte", "PresidenteCasteloBranco", "PontaGrossa",
                                                                            "Cambe", "Guarapuava", "Toledo", "DoisVizinhos"),
                                                                multiple = FALSE, selected = "Cianorte"),
                                                    numericInput("raio", "Raio de Analise (pixels):", value = 50, min = 10, max = 150),
                                                    hr(), h5(icon("robot"), " Monitoramento Automatico"),
                                                    checkboxInput("ativar_auto", tags$strong("Ligar Robo (Roda a cada 10 min)"), value = FALSE),
                                                    numericInput("min_gatilho", "Minuto de gatilho:", value = 3, min = 0, max = 9),
                                                    textInput("hora_diaria", "Hora da msg diaria (HH:MM):", value = "13:03"),
                                                    hr(), actionButton("btn_testar", "Analisar Radar Agora (Manual)", class = "btn-primary w-100", icon = icon("cloud-rain")))
                           ,
                           bslib::card(
                             bslib::card_header("Resultado do Radar", class = "bg-primary text-white"),
                             verbatimTextOutput("status_text"),
                             imageOutput("radar_plot", width = "100%", height = "auto")
                           )
                         )
                )
    ),

    hr(),
    tags$div(
      style = "text-align: center; font-size: 11px; color: #777; margin-bottom: 15px;",
      "Criado por Prof. Dr. Santos H B Dias", tags$br(),
      tags$a(href = "https://www.santoshbdias.com.br/", target = "_blank", style = "color: #777; text-decoration: none;", "https://www.santoshbdias.com.br/")
    )
  )

  server <- function(input, output, session) {
    estado_auto <- reactiveValues(ultima_hora_diaria = "", ultimo_minuto_radar = -1)

    l_vr <- if(!is.null(limites_personalizados$vr)) limites_personalizados$vr else 70
    l_vb <- if(!is.null(limites_personalizados$vb)) limites_personalizados$vb else 25
    l_ar <- if(!is.null(limites_personalizados$ar)) limites_personalizados$ar else 65
    l_ab <- if(!is.null(limites_personalizados$ab)) limites_personalizados$ab else 33

    limites_v <- c(l_vr, l_vb)
    limites_a <- c(l_ar, l_ab)

    observeEvent(input$btn_conectar, {
      req(input$bot_token)
      showNotification("Buscando no Telegram...", type = "message", id = "notif_con")
      chats_encontrados <- buscar_chats_telegram(input$bot_token)
      if (length(chats_encontrados) > 0) {
        output$secao_chat_id <- renderUI({ tagList(selectInput("chat_id_busca", "Selecione o Grupo:", choices = chats_encontrados), actionButton("btn_proxima_aba", "Ir para o Painel", class = "btn-success w-100")) })
      } else { showNotification("Nenhum grupo encontrado.", type = "warning") }
      removeNotification(id = "notif_con")
    })

    observeEvent(input$chat_id_busca, { updateTextInput(session, "chat_id_final", value = input$chat_id_busca) })
    observeEvent(input$btn_proxima_aba, { updateTabsetPanel(session, "abas_principais", selected = "aba_painel") })

    observe({
      invalidateLater(30000, session)

      if (!isTRUE(input$ativar_auto)) return()
      if (is.null(input$bot_token) || input$bot_token == "") return()
      if (is.null(input$chat_id_final) || input$chat_id_final == "") return()
      if (is.null(input$cidade) || length(input$cidade) == 0) return()

      hora_atual <- format(Sys.time(), "%H:%M")
      minuto_atual <- as.numeric(format(Sys.time(), "%M"))

      if (isTRUE(hora_atual == input$hora_diaria) && isTRUE(estado_auto$ultima_hora_diaria != hora_atual)) {
        try(status_diario(hora_alerta = input$hora_diaria, bot_token = input$bot_token, chat_id = input$chat_id_final, mensagem = "Mensagem diaria: Sistema ativo."), silent = TRUE)
        estado_auto$ultima_hora_diaria <- hora_atual
      }

      gatilho_atual <- input$min_gatilho
      if (!is.numeric(gatilho_atual) || is.na(gatilho_atual)) gatilho_atual <- 3

      if (isTRUE(minuto_atual %% 10 == gatilho_atual) && isTRUE(estado_auto$ultimo_minuto_radar != minuto_atual)) {
        estado_auto$ultimo_minuto_radar <- minuto_atual
        texto_painel <- paste0(format(Sys.time(), "%H:%M:%S"), " - Varredura automatica realizada\n")

        for (cid in input$cidade) {
          tryCatch({
            res_auto <- analisar_radar_PR(mega = cid, raio = input$raio, coords_custom = NULL)
            vr <- if(is.null(res_auto) || is.na(res_auto$R)) 0 else round(res_auto$R, 2)
            vb <- if(is.null(res_auto) || is.na(res_auto$B)) 0 else round(res_auto$B, 2)

            texto_painel <- paste0(texto_painel, cid, ": RED = ", vr, " | BLUE = ", vb, "\n")

            temp_img <- tempfile(fileext = ".png")
            gerar_imagem_radar(cidade = cid, raio = input$raio, caminho_salvar = temp_img)

            output$radar_plot <- renderImage({
              list(src = temp_img, contentType = 'image/png', width = "100%")
            }, deleteFile = FALSE)

          }, error = function(e) {
            texto_painel <- paste0(texto_painel, cid, ": ERRO NA LEITURA\n")
          })

          tryCatch({
            executar_alerta_telegram(mega = cid, chat_id = input$chat_id_final, bot_token = input$bot_token, raio = input$raio, vermelho = limites_v, amarelo = limites_a)
          }, error = function(e) { NULL })

          Sys.sleep(1)
        }

        output$status_text <- renderText({ texto_painel })
      }
    })

    observeEvent(input$btn_testar, {
      req(input$cidade, input$raio)
      tryCatch({
        showNotification("Baixando radar e analisando...", id = "notif_radar", duration = NULL)
        cid_alvo <- input$cidade[1]

        rgb_Res <- analisar_radar_PR(mega = cid_alvo, raio = input$raio, coords_custom = NULL)

        val_r <- if(is.null(rgb_Res) || is.na(rgb_Res$R)) 0 else round(rgb_Res$R, 2)
        val_b <- if(is.null(rgb_Res) || is.na(rgb_Res$B)) 0 else round(rgb_Res$B, 2)

        texto_rgb <- paste0(format(Sys.time(), "%H:%M:%S"), " - ", "Varredura Manual Realizada","\n", cid_alvo, ": RED = ", val_r, " | BLUE = ", val_b)

        temp_img <- tempfile(fileext = ".png")
        gerar_imagem_radar(cidade = cid_alvo, raio = input$raio, caminho_salvar = temp_img)

        output$radar_plot <- renderImage({ list(src = temp_img, contentType = 'image/png', width = "100%") }, deleteFile = FALSE)

        if (input$bot_token != "" && input$chat_id_final != "") {
          executar_alerta_telegram(mega = cid_alvo, chat_id = input$chat_id_final, bot_token = input$bot_token,
                                   raio = input$raio, vermelho = limites_v, amarelo = limites_a)
        }

        output$status_text <- renderText({ texto_rgb })

      }, error = function(e) {
        output$status_text <- renderText({ paste("ERRO CRITICO:", e$message) })
      }, finally = {
        removeNotification(id = "notif_radar")
      })
    })
  }

  shinyApp(ui, server)
}
