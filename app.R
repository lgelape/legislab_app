# ---------------------------------------------------------------------------
# Matriz de Interesse x Poder  --  LegisLab
#
# App Shiny para construir manualmente a matriz de mapeamento de atores
# (partes interessadas) descrita no "Guia de Estudos de Caso" do LegisLab
# (Estudo de Caso 1 - "Meios envolvidos: mapear os atores afetados na matriz
# de interesse x poder"), na tradicao da matriz de Mendelow / BID.
#
# Eixo X = INTERESSE (baixo -> alto)   |   Eixo Y = PODER (baixo -> alto)
#
# Execute com:  shiny::runApp("app.R")
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Paleta LegisLab -------------------------------------------------------
# Cores da identidade visual (as mesmas do logotipo):
#   azuis  #03438E  #72A2CA  #009DD4
#   ouros  #F3A717  #FBD394
# Fundo dos quadrantes: um unico matiz (o azul medio, diluido em branco), do
# mais claro (menor prioridade de engajamento) ao mais escuro (maior). Os
# textos usam tinta neutra ou o azul institucional, nunca a cor do quadrante;
# o ouro fica reservado para os acentos da interface.
LEGISLAB <- list(
  azul       = "#03438E",
  azul_medio = "#72A2CA",
  ciano      = "#009DD4",
  ouro       = "#F3A717",
  ouro_claro = "#FBD394"
)

COR <- list(
  superficie = "#ffffff",          # fundo do painel e dos rotulos
  plano      = "#ffffff",          # fundo da figura
  tinta      = "#0b0b0b",          # texto primario
  tinta2     = "#52514e",          # texto secundario
  tinta3     = "#898781",          # rotulos de eixo e linhas de chamada
  titulo     = LEGISLAB$azul,      # titulo e nomes dos eixos
  eixo       = "#b1cce2",          # divisores e borda do painel
  destaque   = LEGISLAB$azul,      # pontos dos atores
  acento     = LEGISLAB$ouro       # acentos da interface
)

# --- Logotipo --------------------------------------------------------------
# Embutido no HTML como data URI: assim a imagem aparece mesmo que o app seja
# executado de outro diretorio de trabalho (quando o www/ nao e servido).
LOGO <- local({
  candidatos <- c("www/Legislab.jpg", "Legislab.jpg",
                  file.path(dirname(sys.frame(1)$ofile %||% "."), "Legislab.jpg"))
  arq <- candidatos[file.exists(candidatos)]
  if (length(arq) == 0) {
    warning("Legislab.jpg nao encontrado; o logotipo nao sera exibido.")
    ""
  } else if (requireNamespace("base64enc", quietly = TRUE)) {
    paste0("data:image/jpeg;base64,", base64enc::base64encode(arq[1]))
  } else {
    "Legislab.jpg"  # exige o arquivo em www/
  }
})

# Mesmo arquivo, agora como raster, para a assinatura da figura exportada.
# O JPG traz uma larga moldura branca; recorta-la faz o simbolo ocupar melhor
# a faixa reservada, sem precisar aumentar a area do logotipo.
LOGO_RASTER <- local({
  candidatos <- c("www/Legislab.jpg", "Legislab.jpg")
  arq <- candidatos[file.exists(candidatos)]
  if (length(arq) == 0 || !requireNamespace("jpeg", quietly = TRUE)) return(NULL)

  img <- jpeg::readJPEG(arq[1])
  tinta <- apply(img, c(1, 2), min) < 0.92        # pixels que nao sao brancos
  linhas <- which(rowSums(tinta) > 0)
  colunas <- which(colSums(tinta) > 0)
  if (length(linhas) == 0 || length(colunas) == 0) return(img)

  folga <- round(0.02 * max(dim(img)[1:2]))       # respiro de 2% em volta
  img[
    max(1, min(linhas) - folga):min(dim(img)[1], max(linhas) + folga),
    max(1, min(colunas) - folga):min(dim(img)[2], max(colunas) + folga), ,
    drop = FALSE
  ]
})

# --- Definicao dos quadrantes ---------------------------------------------
QUADRANTES <- data.frame(
  id         = c("gerenciar", "satisfeito", "informado", "monitorar"),
  eixos      = c("Alto poder / Alto interesse",
                 "Alto poder / Baixo interesse",
                 "Baixo poder / Alto interesse",
                 "Baixo poder / Baixo interesse"),
  estrategia = c("Gerenciar com atenção",
                 "Manter satisfeito",
                 "Manter informado",
                 "Monitorar"),
  xmin = c(5, 0, 5, 0),
  xmax = c(10, 5, 10, 5),
  ymin = c(5, 5, 0, 0),
  ymax = c(10, 10, 5, 5),
  # rampa sequencial de #72A2CA sobre branco: 30% / 15% / 15% / 6%
  fill = c("#d6e3ef", "#eaf1f7", "#eaf1f7", "#f7fafc"),
  stringsAsFactors = FALSE
)

ESCOLHAS_QUAD <- setNames(
  QUADRANTES$id,
  paste0(QUADRANTES$eixos, " — ", QUADRANTES$estrategia)
)

# Margens internas do quadrante (em unidades do grafico), para que nem o ponto
# nem o rotulo acima dele encostem na borda ou no nome da estrategia.
PAD_X <- 0.75
PAD_INF <- 0.55
PAD_SUP <- 1.60

# Deslocamento vertical do rotulo em relacao ao ponto do ator.
OFFSET_ROTULO <- 0.55

# O logotipo mora na linha do titulo e e dimensionado por ela: sua altura e um
# multiplo da altura do titulo, para que marca e texto se equilibrem sem que a
# linha precise crescer muito. TAM_LOGO_CM so entra quando nao ha titulo.
TAM_TITULO_PT   <- 16
FATOR_LOGO      <- 1.5     # altura do logotipo / altura da linha do titulo
TAM_LOGO_CM     <- 0.9     # altura minima, usada quando o titulo esta vazio
FOLGA_LOGO_MM   <- 3

#' Converte a posicao relativa dentro do quadrante (0-100) em coordenadas
#' do grafico (0-10 em cada eixo). Vetorizada.
posicionar <- function(id, p_interesse, p_poder) {
  q <- QUADRANTES[match(id, QUADRANTES$id), ]
  data.frame(
    x = q$xmin + PAD_X + (p_interesse / 100) * ((q$xmax - q$xmin) - 2 * PAD_X),
    y = q$ymin + PAD_INF + (p_poder / 100) * ((q$ymax - q$ymin) - PAD_INF - PAD_SUP)
  )
}

VAZIO <- data.frame(
  ator = character(0), quadrante = character(0), eixos = character(0),
  estrategia = character(0), p_interesse = numeric(0), p_poder = numeric(0),
  stringsAsFactors = FALSE
)

# --- Assinatura da figura ---------------------------------------------------
#' Acrescenta o logotipo do LegisLab FORA da area do grafico, no canto
#' superior direito da figura, na mesma faixa do titulo. Em vez de sobrepor a
#' marca ao painel, ela ocupa a linha do titulo (esticada, se preciso, para
#' caber), de modo que nunca dispute espaco com quadrantes, pontos ou rotulos.
#' Devolve um gtable -- o Shiny (print) e o ggsave (grid.draw) desenham igual.
com_logotipo <- function(p, esc = 1) {
  if (is.null(LOGO_RASTER)) return(p)

  prop  <- dim(LOGO_RASTER)[2] / dim(LOGO_RASTER)[1]   # largura / altura
  folga <- unit(FOLGA_LOGO_MM, "mm")

  g <- ggplotGrob(p)

  linha <- g$layout$t[g$layout$name == "title"]
  if (length(linha) == 0) return(p)

  # A altura da marca deriva da propria linha do titulo -- assim ela acompanha
  # TAM_TITULO_PT e a escala da exportacao sem nenhum ajuste manual. O minimo
  # em cm cobre o caso de titulo vazio, quando a linha teria altura zero.
  alt <- grid::unit.pmax(
    g$heights[linha] * FATOR_LOGO,
    unit(TAM_LOGO_CM * esc, "cm")
  )
  g$heights[linha] <- alt

  # Coluna propria a direita, em vez de sobrepor: assim um titulo longo para
  # antes da marca em vez de correr por baixo dela. Como coord_fixed() ja deixa
  # sobra lateral, a faixa quase nunca custa tamanho ao painel.
  g <- gtable::gtable_add_cols(g, alt * prop + folga, pos = -1)
  coluna <- ncol(g)

  # O plot.background nasceu sem essa coluna; estica-lo evita uma faixa vazada.
  fundo <- which(g$layout$name == "background")
  if (length(fundo) == 1) g$layout$r[fundo] <- coluna

  g <- gtable::gtable_add_grob(
    g,
    grid::rasterGrob(
      LOGO_RASTER, interpolate = TRUE,
      width = alt * prop, height = alt,
      x = unit(1, "npc") - folga, y = unit(0.5, "npc"),
      hjust = 1, vjust = 0.5
    ),
    t = linha, l = coluna, r = coluna, clip = "off", name = "logotipo"
  )

  class(g) <- c("figura_legislab", class(g))
  g
}

#' O renderPlot() do Shiny desenha o resultado com print(), e o print.gtable
#' apenas descreve o objeto em texto -- dai o metodo proprio. O ggsave() usa
#' grid.draw() e ja funcionaria sem ele.
print.figura_legislab <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}

# --- Construcao do grafico -------------------------------------------------
matriz_plot <- function(dados,
                        titulo = NULL, subtitulo = NULL, fonte = NULL,
                        tam_rotulo = 3.6, mostrar_estrategias = TRUE,
                        evitar_sobreposicao = FALSE, escala = 1) {

  esc <- escala  # multiplica todos os tamanhos de texto (usado na exportacao)

  p <- ggplot() +
    # Quadrantes
    geom_rect(
      data = QUADRANTES,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      colour = NA
    ) +
    scale_fill_identity() +
    # Linhas divisoras (recessivas)
    geom_hline(yintercept = 5, colour = COR$eixo, linewidth = 0.4) +
    geom_vline(xintercept = 5, colour = COR$eixo, linewidth = 0.4)

  # Nome da estrategia de cada quadrante
  if (isTRUE(mostrar_estrategias)) {
    p <- p + geom_text(
      data = QUADRANTES,
      aes(x = (xmin + xmax) / 2, y = ymax - 0.42, label = toupper(estrategia)),
      colour = COR$tinta2, size = 3.1 * esc, fontface = "bold"
    )
  }

  # Atores: um ponto na posicao exata, ligado por uma linha ao rotulo (geom_label)
  if (nrow(dados) > 0) {
    pos <- posicionar(dados$quadrante, dados$p_interesse, dados$p_poder)
    d <- cbind(dados, pos)

    args_label <- list(
      data = d,
      mapping = aes(x = x, y = y, label = ator),
      fill = COR$superficie,
      colour = COR$tinta,
      size = tam_rotulo * esc,
      label.size = 0.25,
      label.r = unit(0.16, "lines"),
      label.padding = unit(0.30, "lines"),
      lineheight = 0.95
    )

    if (isTRUE(evitar_sobreposicao) && requireNamespace("ggrepel", quietly = TRUE)) {
      # O ggrepel afasta os rotulos sobrepostos e desenha sozinho a linha de
      # chamada ate o ponto (min.segment.length = 0 garante a linha sempre).
      p <- p + do.call(
        ggrepel::geom_label_repel,
        c(args_label, list(
          nudge_y = OFFSET_ROTULO, segment.colour = COR$tinta3,
          segment.size = 0.3, min.segment.length = 0, box.padding = 0.2,
          point.padding = 0.15, max.overlaps = Inf, seed = 42
        ))
      )
    } else {
      # Linha de chamada do ponto ate o centro do rotulo; o rotulo, opaco e
      # desenhado por ultimo, cobre o trecho final da linha.
      args_label$mapping <- aes(x = x, y = y + OFFSET_ROTULO, label = ator)
      p <- p +
        geom_segment(
          data = d,
          aes(x = x, y = y, xend = x, yend = y + OFFSET_ROTULO),
          colour = COR$tinta3, linewidth = 0.3
        ) +
        do.call(geom_label, args_label)
    }

    # O ponto vai por cima da linha, com anel branco para nao colar no fundo
    p <- p + geom_point(
      data = d, aes(x = x, y = y),
      shape = 21, fill = COR$destaque, colour = COR$superficie,
      size = 2.2 * esc, stroke = 0.6
    )
  }

  p <- p +
    scale_x_continuous(
      limits = c(0, 10), breaks = c(0, 5, 10),
      labels = c("Baixo", "Médio", "Alto"),
      expand = expansion(mult = 0.012)
    ) +
    scale_y_continuous(
      limits = c(0, 10), breaks = c(0, 5, 10),
      labels = c("Baixo", "Médio", "Alto"),
      expand = expansion(mult = 0.012)
    ) +
    labs(
      x = "INTERESSE", y = "PODER",
      title = if (nzchar(titulo %||% "")) titulo else NULL,
      subtitle = if (nzchar(subtitulo %||% "")) subtitulo else NULL,
      caption = if (nzchar(fonte %||% "")) fonte else NULL
    ) +
    coord_fixed(ratio = 1, clip = "off") +
    theme_minimal(base_size = 12 * esc) +
    theme(
      plot.background   = element_rect(fill = COR$plano, colour = NA),
      panel.background  = element_rect(fill = COR$superficie, colour = NA),
      panel.grid        = element_blank(),
      panel.border      = element_rect(fill = NA, colour = COR$eixo, linewidth = 0.5),
      axis.title.x      = element_text(colour = COR$titulo, face = "bold",
                                       size = 10.5 * esc, margin = margin(t = 5 * esc)),
      axis.title.y      = element_text(colour = COR$titulo, face = "bold",
                                       size = 10.5 * esc, margin = margin(r = 5 * esc)),
      axis.text         = element_text(colour = COR$tinta3, size = 9 * esc),
      axis.ticks        = element_blank(),
      plot.title        = element_text(colour = COR$titulo, face = "bold",
                                       size = TAM_TITULO_PT * esc,
                                       margin = margin(b = 3 * esc)),
      plot.subtitle     = element_text(colour = COR$tinta2, size = 10.5 * esc,
                                       margin = margin(b = 7 * esc)),
      plot.caption      = element_text(colour = COR$tinta3, size = 8.5 * esc,
                                       hjust = 0, margin = margin(t = 6 * esc)),
      plot.margin       = margin(10, 8, 4, 5)   # topo folgado: o logotipo mora la
    )

  com_logotipo(p, esc)
}

# --- Interface -------------------------------------------------------------
ui <- page_sidebar(

  # Titulo a esquerda, logotipo do LegisLab no canto superior direito
  title = div(
    class = "lg-marca",
    span(class = "lg-titulo", "Matriz de Interesse × Poder"),
    img(src = LOGO, class = "lg-logo", alt = "LegisLab")
  ),

  theme = bs_theme(
    version = 5, bg = "#ffffff", fg = "#0b0b0b",
    primary = LEGISLAB$azul, secondary = LEGISLAB$azul_medio,
    info = LEGISLAB$ciano, warning = LEGISLAB$ouro
  ),

  # Ajustes finos da identidade visual sobre o Bootstrap
  tags$head(tags$style(HTML(sprintf('
    :root {
      --lg-azul: %s; --lg-azul-medio: %s; --lg-ciano: %s;
      --lg-ouro: %s; --lg-ouro-claro: %s;
      --lg-navy: #062244;          /* texto sobre o azul medio */
      --lg-navy-2: #10355e;        /* texto de apoio sobre o azul medio */
    }

    /* Variaveis do bslib. Atencao: o bslib declara a variavel do titulo no
       proprio .bslib-page-sidebar, e uma declaracao no elemento vence a
       herdada de :root -- por isso ela precisa ser redefinida AQUI. */
    .bslib-page-sidebar {
      --bslib-page-sidebar-title-bg: var(--lg-azul);
      --bslib-page-sidebar-title-color: #ffffff;
    }
    :root {
      --bslib-sidebar-bg: var(--lg-azul-medio);
      --bslib-sidebar-fg: var(--lg-navy);
      --bslib-sidebar-main-bg: var(--lg-azul-medio);
    }

    /* --- Barra superior (azul escuro, filete ouro) ------------------------ */
    .bslib-page-sidebar > .navbar {
      background-color: var(--lg-azul);
      border-bottom: 3px solid var(--lg-ouro);
    }
    /* o bslib insere o titulo direto no .container-fluid da navbar */
    .navbar > .container-fluid { flex-wrap: nowrap; padding: 0.5rem 1.25rem; }
    .lg-marca {
      display: flex; align-items: center; justify-content: space-between;
      width: 100%%; gap: 1rem;
    }
    .lg-titulo {
      color: #ffffff; font-weight: 700; font-size: 1.3rem;
      letter-spacing: -0.01em;
    }
    /* o logotipo e um JPG de fundo branco: uma placa branca o acomoda */
    .lg-logo {
      height: 76px; width: auto; background: #ffffff;
      border-radius: 8px; padding: 4px 12px;
    }

    /* --- Area de trabalho: azul medio, conteudo em cartoes brancos -------- */
    body { background-color: var(--lg-azul-medio); }
    .bslib-sidebar-layout > .main { padding: 0.9rem; }
    .bslib-sidebar-layout { border-color: var(--lg-azul); }
    .card {
      border: none; border-radius: 8px;
      box-shadow: 0 2px 8px rgba(3, 67, 142, .22);
    }
    .card-header {
      background-color: #ffffff; color: var(--lg-azul); font-weight: 600;
      border-bottom: 2px solid var(--lg-azul-medio); border-radius: 8px 8px 0 0;
    }

    /* --- Barra lateral no azul medio -------------------------------------- */
    /* Sobre o azul medio o texto branco fica em ~2,7:1; a tinta escura da
       identidade rende ~5,9:1, entao os rotulos vao em azul-marinho. */
    .bslib-sidebar-layout > .sidebar { border-right: 3px solid var(--lg-ouro); }
    .sidebar .control-label, .sidebar .form-label,
    .sidebar .shiny-input-container > label,
    .sidebar .checkbox label, .sidebar label {
      color: var(--lg-navy); font-weight: 600;
    }
    .sidebar .help-block, .sidebar .form-text { color: var(--lg-navy-2); }
    .sidebar hr { border-color: rgba(6, 34, 68, .40); opacity: 1; }

    /* Acordeao: faixa branca translucida com texto escuro */
    .sidebar .accordion, .sidebar .accordion-item {
      background: transparent; border-color: rgba(6, 34, 68, .30);
    }
    .sidebar .accordion-button {
      background: rgba(255, 255, 255, .40); color: var(--lg-navy);
      font-weight: 700;
    }
    .sidebar .accordion-button:not(.collapsed) {
      background: rgba(255, 255, 255, .62); color: var(--lg-azul);
      box-shadow: none;
    }
    .sidebar .accordion-button:focus {
      box-shadow: 0 0 0 .2rem rgba(3, 67, 142, .35);
    }
    .sidebar .accordion-body { background: transparent; }

    /* --- Botoes ------------------------------------------------------------ */
    /* Definem as variaveis do proprio Bootstrap, para que hover/active/focus
       venham de graca e sobreponham o .btn-default do actionButton.
       O ouro sobre o azul medio tem contraste baixo (~1,2:1), entao o filete
       azul-marinho e quem desenha a borda do botao -- e o rotulo usa a mesma
       tinta do filete (7,8:1 sobre o ouro; 11,2:1 sobre o ouro claro). */
    .btn.btn-ouro, .btn.btn-ouro-claro, .btn.btn-vermelho,
    .btn.btn-ouro:hover, .btn.btn-ouro-claro:hover, .btn.btn-vermelho:hover,
    .btn.btn-ouro:focus, .btn.btn-ouro-claro:focus, .btn.btn-vermelho:focus {
      border-color: var(--lg-navy); font-weight: 600;
    }
    /* As variaveis --bs-btn-* dependem de o .btn-ouro vir depois do .btn e do
       .btn-default na cascata (mesma especificidade). Para o rotulo NUNCA
       sumir no estado de repouso, cor e fundo tambem sao declarados direto,
       com o seletor duplo .btn.btn-* -- especificidade 0,2,0, que vence
       .btn e .btn-default independentemente da ordem das folhas. */
    .btn-ouro { --bs-btn-focus-shadow-rgb: 243, 167, 23; }
    .btn.btn-ouro,
    .btn.btn-ouro:visited {
      color: var(--lg-navy); background-color: var(--lg-ouro);
    }
    .btn.btn-ouro:hover, .btn.btn-ouro:focus, .btn.btn-ouro:active {
      color: var(--lg-navy); background-color: #d9930f;
    }

    .btn-ouro-claro { --bs-btn-focus-shadow-rgb: 251, 211, 148; }
    .btn.btn-ouro-claro,
    .btn.btn-ouro-claro:visited {
      color: var(--lg-navy); background-color: var(--lg-ouro-claro);
    }
    .btn.btn-ouro-claro:hover, .btn.btn-ouro-claro:focus,
    .btn.btn-ouro-claro:active {
      color: var(--lg-navy); background-color: #f6c477;
    }

    /* Acao destrutiva: o vermelho e escuro demais para a tinta do filete,
       entao aqui o rotulo vai em branco (6,6:1). */
    .btn-vermelho { --bs-btn-focus-shadow-rgb: 176, 42, 37; }
    .btn.btn-vermelho,
    .btn.btn-vermelho:visited {
      color: #ffffff; background-color: #b02a25;
    }
    .btn.btn-vermelho:hover, .btn.btn-vermelho:focus,
    .btn.btn-vermelho:active {
      color: #ffffff; background-color: #94211d;
    }

    /* --- Sliders no ciano da identidade ---------------------------------- */
    .irs--shiny .irs-bar, .irs--shiny .irs-single,
    .irs--shiny .irs-from, .irs--shiny .irs-to {
      background: var(--lg-ciano); border-top-color: var(--lg-ciano);
      border-bottom-color: var(--lg-ciano);
    }
    .irs--shiny .irs-handle > i:first-child { background-color: var(--lg-azul); }
    .irs--shiny .irs-line { background: #eaf1f7; }
    .sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max {
      background: rgba(255, 255, 255, .55); color: var(--lg-navy);
    }

    /* --- Tabela ----------------------------------------------------------- */
    table.dataTable thead th {
      color: var(--lg-azul); border-bottom-color: var(--lg-azul-medio);
    }
    table.dataTable tbody tr.selected td {
      box-shadow: inset 0 0 0 9999px rgba(3, 67, 142, .12) !important;
    }
  ', LEGISLAB$azul, LEGISLAB$azul_medio, LEGISLAB$ciano,
     LEGISLAB$ouro, LEGISLAB$ouro_claro)))),

  sidebar = sidebar(
    width = 380,
    accordion(
      open = c("ator", "figura"),

      accordion_panel(
        "Adicionar ator", value = "ator", icon = NULL,

        textInput("nome", "Nome do ator político",
                  placeholder = "Ex.: Ministério da Saúde"),

        selectInput("quadrante", "Quadrante", choices = ESCOLHAS_QUAD),

        helpText("Posição aproximada dentro do quadrante escolhido:"),

        sliderInput("p_interesse", "Interesse (0 = borda esquerda, 100 = direita)",
                    min = 0, max = 100, value = 50, step = 5, ticks = FALSE),

        sliderInput("p_poder", "Poder (0 = borda inferior, 100 = superior)",
                    min = 0, max = 100, value = 50, step = 5, ticks = FALSE),

        actionButton("adicionar", "Adicionar ator",
                     class = "btn-ouro w-100", icon = icon("plus")),

        tags$hr(),
        div(
          class = "d-flex gap-2",
          actionButton("remover", "Remover selecionado(s)",
                       class = "btn-ouro-claro btn-sm flex-fill"),
          actionButton("limpar", "Limpar tudo",
                       class = "btn-vermelho btn-sm flex-fill")
        ),
        div(class = "form-text mt-2",
            "Clique nas linhas da tabela para selecionar o que remover.")
      ),

      accordion_panel(
        "Figura", value = "figura",
        textInput("titulo", "Título", value = "Matriz de Interesse × Poder"),
        textInput("subtitulo", "Subtítulo (opcional)", value = ""),
        textInput("autores", "Elaborado por (nomes do grupo)",
                  placeholder = "Ex.: Ana Souza, Bruno Lima e Carla Dias"),
        textInput("fonte", "Nota de rodapé (opcional)",
                  value = "Fonte: elaboração própria."),
        div(class = "form-text mb-2",
            "Os nomes entram como uma linha de assinatura no rodapé da figura."),
        sliderInput("tam_rotulo", "Tamanho dos rótulos",
                    min = 2, max = 6, value = 3.6, step = 0.2, ticks = FALSE),
        checkboxInput("estrategias", "Mostrar a estratégia de cada quadrante", TRUE),
        checkboxInput("repel", "Evitar sobreposição de rótulos", FALSE)
      ),

      accordion_panel(
        "Exportar", value = "exportar",
        numericInput("larg", "Largura (cm)", value = 26, min = 8, max = 60, step = 1),
        numericInput("alt", "Altura (cm)", value = 26, min = 8, max = 60, step = 1),
        numericInput("dpi", "Resolução (dpi)", value = 300, min = 72, max = 600, step = 10),
        sliderInput("escala", "Escala do texto na imagem exportada",
                    min = 1, max = 2, value = 1.3, step = 0.05, ticks = FALSE),
        div(class = "form-text mb-3",
            "A prévia na tela usa escala 1. Na imagem exportada os textos são ",
            "ampliados por este fator, para não ficarem pequenos demais quando ",
            "a figura for reduzida no relatório. Se os rótulos se sobrepuserem, ",
            "aumente as dimensões ou ligue \u201cEvitar sobreposição\u201d em ",
            "\u201cFigura\u201d."),
        downloadButton("baixar_png", "PNG", class = "btn-ouro w-100 mb-2"),
        downloadButton("baixar_pdf", "PDF", class = "btn-ouro w-100 mb-2"),
        downloadButton("baixar_csv", "CSV com os atores", class = "btn-ouro-claro w-100")
      )
    )
  ),

  layout_columns(
    col_widths = c(12, 12),
    row_heights = c("minmax(480px, 4fr)", "minmax(180px, 1.4fr)"),

    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          "Matriz",
          downloadButton("baixar_grafico", "Baixar gráfico (PNG)",
                         class = "btn-ouro btn-sm")
        )
      ),
      card_body(plotOutput("matriz", height = "100%"), padding = 2)
    ),

    card(
      full_screen = TRUE,
      card_header("Atores mapeados"),
      card_body(DTOutput("tabela"), padding = 6)
    )
  )
)

# --- Servidor --------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(dados = VAZIO)

  observeEvent(input$adicionar, {
    nome <- trimws(input$nome %||% "")

    if (!nzchar(nome)) {
      showNotification("Informe o nome do ator antes de adicionar.",
                       type = "warning")
      return()
    }
    if (tolower(nome) %in% tolower(rv$dados$ator)) {
      showNotification(sprintf("\"%s\" já está na matriz.", nome),
                       type = "warning")
      return()
    }

    q <- QUADRANTES[match(input$quadrante, QUADRANTES$id), ]

    rv$dados <- rbind(rv$dados, data.frame(
      ator        = nome,
      quadrante   = input$quadrante,
      eixos       = q$eixos,
      estrategia  = q$estrategia,
      p_interesse = input$p_interesse,
      p_poder     = input$p_poder,
      stringsAsFactors = FALSE
    ))

    updateTextInput(session, "nome", value = "")
  })

  observeEvent(input$remover, {
    sel <- input$tabela_rows_selected
    if (length(sel) == 0) {
      showNotification("Selecione ao menos uma linha da tabela.", type = "warning")
      return()
    }
    rv$dados <- rv$dados[-sel, , drop = FALSE]
  })

  observeEvent(input$limpar, {
    rv$dados <- VAZIO
  })

  # Rodape da figura: assinatura do grupo (se preenchida) + nota de fonte
  rodape <- function() {
    autores <- trimws(input$autores %||% "")
    fonte   <- trimws(input$fonte %||% "")
    paste(c(
      if (nzchar(autores)) paste0("Elaborado por: ", autores, "."),
      if (nzchar(fonte)) fonte
    ), collapse = "\n")
  }

  grafico <- function(escala = 1) {
    matriz_plot(
      rv$dados,
      titulo              = input$titulo,
      subtitulo           = input$subtitulo,
      fonte               = rodape(),
      tam_rotulo          = input$tam_rotulo,
      mostrar_estrategias = input$estrategias,
      evitar_sobreposicao = input$repel,
      escala              = escala
    )
  }

  output$matriz <- renderPlot({
    grafico()
  }, res = 96)

  output$tabela <- renderDT({
    d <- rv$dados
    tab <- data.frame(
      d$ator, d$eixos, d$estrategia, d$p_interesse, d$p_poder,
      stringsAsFactors = FALSE
    )
    names(tab) <- c("Ator", "Quadrante", "Estratégia",
                    "Interesse (0–100)", "Poder (0–100)")

    datatable(
      tab,
      rownames = FALSE,
      selection = "multiple",
      options = list(
        dom = "tp", pageLength = 8, ordering = FALSE,
        language = list(
          emptyTable = "Nenhum ator adicionado ainda.",
          paginate = list(previous = "Anterior", `next` = "Próxima")
        )
      )
    )
  })

  # --- Downloads -----------------------------------------------------------
  salvar <- function(arquivo, dispositivo = NULL) {
    ggsave(
      filename = arquivo, plot = grafico(escala = input$escala),
      device = dispositivo,
      width = input$larg, height = input$alt, units = "cm",
      dpi = input$dpi, bg = COR$plano
    )
  }

  # Botao em destaque no cabecalho do card; mesmas dimensoes do painel "Exportar"
  output$baixar_grafico <- downloadHandler(
    filename = function() paste0("matriz-interesse-poder-", Sys.Date(), ".png"),
    content  = function(file) salvar(file)
  )

  output$baixar_png <- downloadHandler(
    filename = function() paste0("matriz-interesse-poder-", Sys.Date(), ".png"),
    content  = function(file) salvar(file)
  )

  output$baixar_pdf <- downloadHandler(
    filename = function() paste0("matriz-interesse-poder-", Sys.Date(), ".pdf"),
    content  = function(file) {
      dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      salvar(file, dispositivo = dev)
    }
  )

  output$baixar_csv <- downloadHandler(
    filename = function() paste0("atores-matriz-", Sys.Date(), ".csv"),
    content  = function(file) {
      utils::write.csv(rv$dados, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
