#' Envia Mensagem de Status com Imagem do Radar Meteorológico via Telegram
#'
#' @description
#' Esta função captura a imagem atual do radar meteorológico do Paraná e a envia
#' para um chat do Telegram, servindo como um "sinal de vida" diário para indicar
#' que o sistema de monitoramento está ativo e operando corretamente.
#'
#' @details
#' A função depende de \code{baixar_radar_PR()} para obter a imagem atual. O arquivo
#' é salvo em um diretório temporário e apagado automaticamente após o envio ou em
#' caso de erro, respeitando as políticas do CRAN para manipulação de arquivos.
#' A comunicação com o Telegram tem um *timeout* de segurança de 15 segundos.
#'
#' @param bot_token Caractere. Token de autenticação do bot do Telegram (fornecido pelo @BotFather).
#' @param chat_id Caractere ou Numérico. ID do chat ou grupo do Telegram de destino.
#' @param hora_alerta Caractere opcional. Horário programado para envio no formato "HH:MM".
#' Se \code{NULL} (padrão), a mensagem é enviada imediatamente sem verificação de horas.
#' @param mensagem Caractere. Texto da legenda que acompanhará a imagem. O padrão é uma
#' mensagem confirmando que o sistema está ativo.
#'
#' @return Retorna um valor lógico de forma invisível: \code{TRUE} para sucesso no envio,
#' ou \code{FALSE} para falha (seja por erro de download, rede ou na API do Telegram).
#'
#' @importFrom magick image_write
#' @importFrom httr POST upload_file content status_code timeout
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Enviar status imediatamente
#' status_diario(
#'   bot_token = "SEU_TOKEN_AQUI",
#'   chat_id = "-1001234567890",
#'   mensagem = "Teste manual de status do radar."
#' )
#'
#' # Enviar apenas se o horario local for exatamente 13:00
#' status_diario(
#'   bot_token = "SEU_TOKEN_AQUI",
#'   chat_id = "-1001234567890",
#'   hora_alerta = "13:00"
#' )
#' }
status_diario <- function(bot_token, chat_id, hora_alerta = NULL,
                          mensagem = paste0('Mensagem diaria de status. ',
                                            'Sistema de alerta meteorologico ativo e a funcionar perfeitamente.')) {

  # 1. Verificacao de horario (Tornamos opcional para maior flexibilidade)
  if (!is.null(hora_alerta)) {
    hora_atual <- format(Sys.time(), "%H:%M")
    if (hora_atual != hora_alerta) {
      return(invisible(FALSE))
    }
  }

  # 2. Obtencao da imagem do radar
  radar_img <- tryCatch(
    baixar_radar_PR(),
    error = function(e) {
      warning("Erro ao descarregar a imagem do radar: ", e$message)
      return(NULL)
    }
  )

  if (is.null(radar_img)) {
    message("Imagem nao disponivel. O status nao foi enviado.")
    return(invisible(FALSE))
  }

  # 3. Utilizacao de Ficheiros Temporarios (Obrigatorio no CRAN)
  caminho_temp <- tempfile(pattern = "status_radar_", fileext = ".png")

  on.exit(if(file.exists(caminho_temp)) file.remove(caminho_temp), add = TRUE)

  img_salva <- tryCatch({
    magick::image_write(radar_img, path = caminho_temp, format = "png")
    TRUE
  }, error = function(e) {
    warning("Erro ao guardar imagem temporaria: ", e$message)
    FALSE
  })

  if (!img_salva) return(invisible(FALSE))

  # 4. Envio via API do Telegram com Timeout de Seguranca
  resposta <- tryCatch({
    httr::POST(
      url = paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto"),
      body = list(
        chat_id = chat_id,
        photo = httr::upload_file(caminho_temp),
        caption = mensagem,
        parse_mode = "Markdown"
      ),
      httr::timeout(15) # Evita que a funcao bloqueie infinitamente
    )
  }, error = function(e) {
    warning("Erro de ligacao a API do Telegram: ", e$message)
    return(NULL)
  })

  # 5. Validacao da Resposta
  if (!is.null(resposta) && httr::status_code(resposta) == 200) {
    message("\u2705 Mensagem diaria de status enviada com sucesso.")
    return(invisible(TRUE))
  } else {
    erro_msg <- if(!is.null(resposta)) httr::content(resposta)$description else "Falha de rede ou timeout"
    warning("Falha ao enviar mensagem no Telegram. Detalhes: ", erro_msg)
    return(invisible(FALSE))
  }
}
