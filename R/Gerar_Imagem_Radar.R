#' Gerar Imagem do Radar Meteorológico com Marcações Circulares
#'
#' @description
#' Esta função baixa a imagem do radar meteorológico do Paraná (ou utiliza uma já fornecida)
#' e adiciona marcações circulares (raio de análise) ao redor das coordenadas de uma
#' cidade ou ponto de interesse específico.
#'
#' @param cidade Caractere. Nome da cidade ou ponto de interesse (ex: "Cianorte").
#' Deve corresponder a uma chave na lista de coordenadas interna ou em \code{coords_custom}.
#' @param raio Numérico. Raio da área de análise em pixels que será desenhado na imagem.
#' @param img_radar Objeto \code{magick-image} opcional. Imagem de radar previamente baixada.
#' Se \code{NULL} (padrão), a função tentará baixar uma nova imagem usando \code{baixar_radar_PR()}.
#' @param coords_custom Lista nomeada opcional. Permite adicionar novas coordenadas além das
#' predefinidas. Formato esperado: \code{list("NomeDaCidade" = list(x = 100, y = 200))}.
#' @param caminho_salvar Caractere opcional. Caminho completo (incluindo o nome do arquivo e
#' extensão .png) para salvar a imagem gerada. Se \code{NULL}, salva em um diretório temporário.
#'
#' @return Retorna um objeto de imagem (\code{magick-image}) com as marcações desenhadas,
#' ou \code{NULL} em caso de falha na obtenção da imagem ou cidade não encontrada.
#' Como efeito colateral, salva a imagem gerada no disco.
#'
#' @importFrom magick image_draw image_write
#' @importFrom graphics points symbols
#' @importFrom grDevices dev.off
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Gerar imagem para Cianorte com raio de 50 pixels (salva em tempfile)
#' img_cianorte <- gerar_imagem_radar(cidade = "Cianorte", raio = 50)
#'
#' # Adicionar uma nova cidade e salvar em um caminho especifico
#' novas_coords <- list("Londrina" = list(x = 520, y = 175))
#' gerar_imagem_radar(
#'   cidade = "Londrina",
#'   raio = 60,
#'   coords_custom = novas_coords,
#'   caminho_salvar = "C:/temp/radar_londrina.png"
#' )
#' }
gerar_imagem_radar <- function(cidade,
                               raio,
                               img_radar = NULL,
                               coords_custom = NULL,
                               caminho_salvar = NULL) {

  # 1. Base de dados de coordenadas
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

  if (!(cidade %in% names(coords))) {
    warning("Cidade '", cidade, "' nao encontrada nas coordenadas.")
    return(NULL)
  }

  x_centro <- coords[[cidade]]$x
  y_centro <- coords[[cidade]]$y

  # 2. Obtencao da Imagem (Reaproveita se ja existir)
  if (is.null(img_radar)) {
    img_radar <- tryCatch(
      baixar_radar_PR(),
      error = function(e) {
        warning("Erro ao baixar a imagem do radar: ", e$message)
        return(NULL)
      }
    )
  }

  if (is.null(img_radar)) return(NULL)

  # 3. Desenho Rapido Vetorizado (Substitui os FOR loops e seno/cosseno)
  img_plot <- magick::image_draw(img_radar)

  graphics::points(x_centro, y_centro, col = "red2", pch = 19, cex = 0.5)

  raios_seq <- seq(5, raio, by = ((raio - 5) / 3))

  graphics::symbols(x = rep(x_centro, length(raios_seq)),
                    y = rep(y_centro, length(raios_seq)),
                    circles = raios_seq,
                    inches = FALSE, add = TRUE,
                    fg = "royalblue1", lty = 3, lwd = 1.5)

  grDevices::dev.off()

  # 4. Gestao Segura de Ficheiros (Adequado ao CRAN)
  if (is.null(caminho_salvar)) {
    caminho_salvar <- tempfile(pattern = paste0("radar_", cidade, "_"), fileext = ".png")
  } else {
    dir_salvar <- dirname(caminho_salvar)
    if (!dir.exists(dir_salvar)) dir.create(dir_salvar, recursive = TRUE)
  }

  tryCatch({
    magick::image_write(img_plot, path = caminho_salvar, format = "png")
  }, error = function(e) {
    warning("Erro ao guardar a imagem no caminho especificado: ", e$message)
  })

  return(img_plot)
}
