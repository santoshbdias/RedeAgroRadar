# RedeAgroRadar

O objetivo do pacote **RedeAgroRadar** é fornecer ferramentas para o monitoramento em tempo real de radares meteorológicos e envio de alertas automatizados. Ele facilita a análise de imagens do Simepar (Paraná, Brasil) para identificar áreas de chuva e notificar grupos de pesquisa ou produtores via **Telegram**. 

O pacote foi desenhado especificamente para apoiar a **Rede Agropesquisa** no monitoramento hidrológico de megaparcelas experimentais.

## Instalação dos softwares

Antes de utilizar o pacote, certifique-se de que você tem o **R** e o **RStudio** instalados:

- **R (CRAN):** Acesse [https://cran.r-project.org](https://cran.r-project.org) e baixe a versão mais recente.
- **RStudio (IDE recomendada):** Acesse [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/).

## Instalação do pacote

Você pode instalar a versão de desenvolvimento do RedeAgroRadar a partir do [GitHub](https://github.com/santoshbdias/RedeAgroRadar) com:

``` r
# Instale devtools se ainda não tiver
install.packages("devtools")

# Instale o RedeAgroRadar diretamente do GitHub
devtools::install_github("santoshbdias/RedeAgroRadar")
```

## Exemplo

Este é um exemplo básico que mostra como iniciar o painel de monitoramento interativo:

``` r
library(RedeAgroRadar)

# Iniciar o dashboard interativo
Run_Monitor_RedeAgro()


```
