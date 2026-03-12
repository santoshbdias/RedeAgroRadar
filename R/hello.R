# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#############################################################################################
#########################CRIANDO MEU PACOTE#################################################
#


## Adiciona o pacote httr como dependencia oficial
#usethis::use_package("httr")
#usethis::use_package("magick")
#usethis::use_package("shiny")
#usethis::use_package("bslib")

#rm(list = ls()); gc(); graphics.off(); cat("\014")#Limpar todos os dados e abas


# Atualiza os arquivos de documentacao com a nova funcao exportada
#devtools::document()

# Faz o pacote ficar disponivel para voce testar agora mesmo
#devtools::load_all()

# Testa o aplicativo!
#Run_Monitor_RedeAgro(Token='7935384745:AAET5JvZdH6qCnpfPrMEi-plgrVMHEx_Eo8',chatID='-4952627577', limites_personalizados = list(vr = 70, vb = 25, ar = 65, ab = 33)) # Você pode omitir o amarelo (ar, ab), ele puxa o padrão!

Run_Monitor_RedeAgro(Token='7935384745:AAET5JvZdH6qCnpfPrMEi-plgrVMHEx_Eo8',
                     chatID='-4952627577',
                     limites_personalizados = list(vr = 70, vb = 25, ar = 65, ab = 33)) # Você pode omitir o amarelo (ar, ab), ele puxa o padrão!

Run_Monitor_RedeAgro(Token='7935384745:AAET5JvZdH6qCnpfPrMEi-plgrVMHEx_Eo8',chatID='-4952627577')


#PUBLICAÇÃO CRAN
#devtools::check_win_release()
#devtools::check_win_devel()

#usethis::use_cran_comments()

#devtools::spell_check()

#devtools::release()

#
# hello <- function() {
#   print("Hello, world!")
# }
