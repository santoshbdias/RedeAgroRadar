#' Baixar Imagem do Radar Meteorológico do Paraná ('Simepar')
#'
#' @description
#' Faz o download da imagem mais recente do radar meteorológico do Paraná,
#' disponibilizada publicamente peloo 'Simepar' (<https://www.simepar.br/simepar/radar_msc>),
#' e a carrega diretamente na memória do 'R' como um objeto de imagem.
#'
#' @details
#' A função realiza uma requisição HTTP \code{GET} utilizando cabeçalhos (headers)
#' customizados para evitar bloqueios do servidor. O carregamento é feito diretamente
#' na memória (em formato \code{raw}), sem a necessidade de gravar arquivos temporários
#' no disco, o que otimiza a performance. Inclui também uma trava de tempo (\code{timeout})
#' para evitar travamentos infinitos caso o servidor do radar esteja fora do ar,
#' uma prática exigida pelas políticas do 'CRAN'.
#'
#' @param url Caractere. String contendo a URL direta da imagem. O padrão aponta para
#' o produto atual de radar do 'Simepar'.
#' @param timeout Numérico. Tempo máximo de espera em segundos pela resposta do
#' servidor antes de cancelar a operação. Padrão é 10.
#'
#' @return Retorna um objeto de imagem da classe \code{magick-image}. Em caso de falha
#' na conexão, esgotamento do tempo limite (timeout) ou erro de formatação,
#' retorna \code{NULL} e exibe uma mensagem de alerta.
#'
#' @importFrom httr GET add_headers status_code content timeout
#' @importFrom magick image_read
#'
#' @export
#'
#' @examples
#' # Baixar a imagem atual do radar com timeout padrao (10s)
#' radar_img <- baixar_radar_PR()
#'
#' # Exibir a imagem no painel de plots, se o download foi bem-sucedido
#' if (!is.null(radar_img)) {
#'   plot(radar_img)
#' }
#'
#' # Tentar baixar com um timeout maior caso a internet esteja lenta
#' radar_img_lento <- baixar_radar_PR(timeout = 20)
#'


baixar_radar_PR <- function(url = "https://lb01.simepar.br/riak/pgw-radar/product1.jpeg",
                            timeout = 10) {

  # 1. Tentativa de conexao com tratamento de erro e timeout
  resposta <- tryCatch({
    httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "Referer" = "https://www.simepar.br/simepar/radar_msc"
      ),
      httr::timeout(timeout)
    )
  }, error = function(e) {
    warning("Falha na conexao com o servidor Simepar: ", e$message)
    return(NULL)
  })

  # 2. Verificacao de Status
  if (is.null(resposta) || httr::status_code(resposta) != 200) {
    message("[ALERTA] Imagem nao disponivel. Status: ",
            if(is.null(resposta)) "Timeout/Erro" else httr::status_code(resposta))
    return(NULL)
  }

  # 3. Leitura direta da memoria (Otimizacao de Performance)
  img <- tryCatch({
    magick::image_read(httr::content(resposta, "raw"))
  }, error = function(e) {
    message("Erro ao processar o formato da imagem.")
    return(NULL)
  })

  return(img)
}
