#' Analisar a Cor Média em uma Região Circular do Radar
#'
#' @description
#' Esta função baixa a imagem atual do radar meteorológico do Paraná e extrai
#' os valores médios dos canais RGB (Red, Green, Blue) em uma área circular
#' ao redor de uma coordenada específica (cidade ou ponto de interesse).
#'
#' @details
#' A função depende de \code{baixar_radar_PR()} para obter a imagem de satélite
#' do 'Simepar'. A varredura dos pixels é feita utilizando coordenadas polares
#' para cobrir a área definida pelo raio estipulado.
#'
#' @param mega Caractere. Nome da cidade ou ponto de interesse. Padrão é \code{'PresidenteCasteloBranco'}.
#' @param raio Numérico. Raio em pixels da área circular a ser analisada ao redor do ponto central. Padrão é 50.
#' @param coords_custom Lista nomeada opcional. Permite definir coordenadas customizadas
#' com valores de \code{x} e \code{y} (ex: \code{list("Cascavel" = list(x = 200, y = 300))}).
#'
#' @return Retorna uma lista nomeada contendo os valores numéricos médios de \code{R},
#' \code{G} e \code{B}, além do número total de pixels validados (\code{n_pixels}).
#' Caso a imagem não possa ser baixada, retorna \code{FALSE}.
#'
#' @importFrom magick image_data
#' @export
#'
#' @examples
#' # Analisar a chuva na regiao de Cianorte com raio padrao (50px)
#' resultado <- analisar_radar_PR(mega = "Cianorte")
#'
#' # Verifica se o resultado e uma lista (garante que baixou a imagem sem erro de internet)
#' if (is.list(resultado)) {
#'   print(resultado$R) # Valor do canal Vermelho (chuva forte)
#' }
#'
#' # Analisar com um raio maior (salvando em variavel para evitar impressoes soltas)
#' res_ponta_grossa <- analisar_radar_PR(mega = "PontaGrossa", raio = 80)
#'
#' # Inserir uma cidade que nao esta na lista padrao
#' novas_coords <- list("Cascavel" = list(x = 250, y = 310))
#' res_cascavel <- analisar_radar_PR(mega = "Cascavel", raio = 50,
#'                                   coords_custom = novas_coords)
#'

analisar_radar_PR <- function(mega = 'PresidenteCasteloBranco', raio = 50, coords_custom = NULL) {

  # 1. Base de dados de coordenadas (Pode ser movido para um arquivo de dados do pacote)
  coords <- list(
    'Cianorte' = list(x = 388, y = 240),
    'PresidenteCasteloBranco' = list(x = 437, y = 190),
    'PontaGrossa' = list(x = 613, y = 361),
    'Cambe' = list(x = 509, y = 185),
    'Guarapuava' = list(x = 483, y = 405),
    'Toledo' = list(x = 308, y = 335),
    'DoisVizinhos' = list(x = 340, y = 420)
  )

  if (!is.null(coords_custom)) coords <- append(coords, coords_custom)

  if (!(mega %in% names(coords))) {
    stop("Localidade '", mega, "' nao encontrada na base de coordenadas.")
  }

  cx <- coords[[mega]]$x
  cy <- coords[[mega]]$y

  img <- tryCatch(baixar_radar_PR(), error = function(e) NULL)
  if (is.null(img)) return(FALSE)

  # 2. Extracao de dados da imagem
  img_data <- magick::image_data(img, channels = "rgb")

  dims <- dim(img_data)
  largura <- dims[2]
  altura  <- dims[3]

  r_vals <- c(); g_vals <- c(); b_vals <- c()

  for (theta in seq(0, 2*pi, length.out = 360)) {
    for (vr in seq(1, raio, by=5)) {
      x <- round(cx + vr * cos(theta))
      y <- round(cy + vr * sin(theta))

      r_vals <- c(r_vals, as.numeric(img_data[1, x, y]))
      g_vals <- c(g_vals, as.numeric(img_data[2, x, y]))
      b_vals <- c(b_vals, as.numeric(img_data[3, x, y]))
    } }

  return(list(
    R = round(mean(r_vals, na.rm = TRUE),2),
    G = round(mean(g_vals, na.rm = TRUE),2),
    B = round(mean(b_vals, na.rm = TRUE),2),
    n_pixels = length(r_vals)
  ))
}
