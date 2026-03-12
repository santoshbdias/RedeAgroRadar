#' Avalia Radar e Dispara Alerta Fotográfico via Telegram
#'
#' @description Avalia os valores médios de cor (RGB) de uma imagem de radar
#' para uma determinada região. Se os valores ultrapassarem os limites
#' pré-estabelecidos para chuva leve (amarelo) ou forte (vermelho), a função
#' gera uma imagem do radar e a envia como alerta em um grupo do Telegram.
#'
#' @details A função depende das rotinas auxiliares \code{analisar_radar_PR()}
#' e \code{gerar_imagem_radar()} para extrair as cores e montar o mapa.
#' A comunicação com a API do Telegram é feita via requisição HTTP POST.
#'
#' @param mega Caractere. Nome da cidade ou ponto de interesse (ex: "Cianorte"). Padrão é "Cianorte".
#' @param chat_id Caractere ou Numérico. ID do grupo/chat de destino no Telegram. Obrigatório.
#' @param bot_token Caractere. Token do bot gerado pelo @BotFather. Obrigatório.
#' @param raio Numérico. Raio de análise ao redor das coordenadas centrais em pixels. Padrão é 50.
#' @param vermelho Vetor numérico de tamanho 2. Valores limite para considerar chuva forte.
#' O primeiro valor é o mínimo para o canal Red (R), o segundo é o máximo para o canal Blue (B). Padrão é \code{c(70, 25)}.
#' @param amarelo Vetor numérico de tamanho 2. Valores limite para considerar chuva leve.
#' Padrão é \code{c(65, 33)}.
#' @param coords_custom Lista nomeada opcional com valores de \code{x} e \code{y}. Usado se a cidade não estiver no dicionário padrão.
#'
#' @return Retorna invisivelmente \code{TRUE} se um alerta de chuva foi disparado com sucesso, ou \code{FALSE} se não houver condição de alerta.
#'
#' @importFrom httr POST upload_file
#' @export
#'
#' @examples
#'  \dontrun{
#' # Exemplo necessita de token e chatID 'Telegram'
#' executar_alerta_telegram(
#'   mega = "Cianorte",
#'   chat_id = "-1001951367890",
#'   bot_token = "SEU_TOKEN_AQUI",
#'   raio = 50,
#'   vermelho = c(80, 20),
#'   amarelo = c(60, 40)
#' )
#' }

executar_alerta_telegram <- function(mega = "Cianorte", chat_id, bot_token, raio = 50,
                                     vermelho = c(70, 25), amarelo = c(65, 33), coords_custom = NULL) {

  rgb_Res <- analisar_radar_PR(mega = mega, raio = raio, coords_custom = coords_custom)

  val_r <- if(is.null(rgb_Res) || is.na(rgb_Res$R) || is.nan(rgb_Res$R)) 0 else rgb_Res$R
  val_b <- if(is.null(rgb_Res) || is.na(rgb_Res$B) || is.nan(rgb_Res$B)) 0 else rgb_Res$B

  resultado <- if (val_r > vermelho[1] & val_b < vermelho[2]) {
    "Chuva forte (vermelho)"
  } else if (val_r > amarelo[1] & val_b < amarelo[2]) {
    "Chuva leve (amarelo)"
  } else {
    "Sem chuva"
  }

  if (resultado %in% c("Chuva leve (amarelo)", "Chuva forte (vermelho)")) {
    legenda <- paste0("\U0001F6A8 Alerta meteorologico em *", mega, "*:\n", resultado)
    caminho_temp <- tempfile(pattern = "alerta_", fileext = ".png")

    gerar_imagem_radar(cidade = mega, raio = raio,coords_custom = coords_custom, caminho_salvar = caminho_temp)

    tryCatch({
      httr::POST(
        url = paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto"),
        body = list(chat_id = chat_id, photo = httr::upload_file(caminho_temp), caption = legenda, parse_mode = "Markdown")
      )
    }, error = function(e) NULL)

    return(TRUE)
  } else {
    return(FALSE)
  }
}
