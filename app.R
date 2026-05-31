library(shiny)
library(ggplot2)

# ── Temas ────────────────────────────────────────────────────────────────────
tema_continua <- theme_gray() +
  theme(legend.position="none", plot.margin=margin(20,24,16,20),
        plot.title=element_text(face="bold", size=16))

tema_discreta <- theme_gray() +
  theme(legend.position="none", plot.margin=margin(20,24,16,20),
        plot.title=element_text(face="bold", size=16))

vline_media   <- function(v) geom_vline(xintercept=v, color="#d9534f", linetype="dashed",  linewidth=0.9)
vline_mediana <- function(v) geom_vline(xintercept=v, color="#5cb85c", linetype="dotdash", linewidth=0.9)
vline_moda    <- function(v) geom_vline(xintercept=v, color="#f0ad4e", linetype="dotted",  linewidth=1.1)

svg_line <- function(color, dasharray) {
  tags$svg(width="36", height="12", style="flex-shrink:0;",
    tags$line(x1="0",y1="6",x2="36",y2="6",
              style=paste0("stroke:",color,";stroke-width:2;stroke-dasharray:",dasharray,";")))
}

leyenda <- tags$div(style="margin-top:10px;",
  tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
           svg_line("#d9534f","5,3"),     tags$span("Media",   style="color:#555;font-size:0.9em;")),
  tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
           svg_line("#5cb85c","7,3,2,3"), tags$span("Mediana", style="color:#555;font-size:0.9em;")),
  tags$div(style="display:flex;align-items:center;gap:8px;",
           svg_line("#f0ad4e","2,2"),     tags$span("Moda",    style="color:#555;font-size:0.9em;"))
)

stats_panel <- function(id_media, id_mediana, id_moda, id_varianza) {
  tagList(hr(),
    helpText(strong("Media:"),    textOutput(id_media,    inline=TRUE)),
    helpText(strong("Mediana:"),  textOutput(id_mediana,  inline=TRUE)),
    helpText(strong("Moda:"),     textOutput(id_moda,     inline=TRUE)),
    helpText(strong("Varianza:"), textOutput(id_varianza, inline=TRUE)),
    leyenda)
}

inline_input <- function(id, label, value, step, min=NA, max=NA) {
  tags$div(style="display:flex;align-items:center;gap:6px;",
    tags$span(label, style="white-space:nowrap;font-size:0.9em;color:#333;"),
    tags$input(id=id, type="number", value=value, step=step,
               min=if(!is.na(min)) min, max=if(!is.na(max)) max,
               class="form-control input-sm", style="width:90px;"))
}

calc_panel <- function(prefix, x_min=-10, x_max=10, x_val=0, x_step=0.1, p_val=0.95, p_step=0.01) {
  tagList(hr(), strong("Probabilidad acumulada"), helpText("P(X \u2264 x) para:"),
    tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
      inline_input(paste0(prefix,"_x"), "x =", x_val, x_step, x_min, x_max),
      actionButton(paste0(prefix,"_calc_p"), "Calcular", class="btn-sm btn-primary")),
    verbatimTextOutput(paste0(prefix,"_prob")),
    hr(), strong("Percentil"), helpText("x tal que P(X \u2264 x) = p:"),
    tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
      inline_input(paste0(prefix,"_p"), "p =", p_val, p_step, 0.001, 0.999),
      actionButton(paste0(prefix,"_calc_q"), "Calcular", class="btn-sm btn-primary")),
    verbatimTextOutput(paste0(prefix,"_quant")))
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  withMathJax(),
  titlePanel("Distribuciones de Probabilidad"),

  tabsetPanel(

    # ════════════════════════════════════════════════════════════════════════
    # DISCRETAS
    # ════════════════════════════════════════════════════════════════════════
    tabPanel("Discretas",
      br(),
      tabsetPanel(

        # D1. Binomial
        tabPanel("Binomial", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("bin_n","n (ensayos)", min=1, max=100, value=20, step=1),
            sliderInput("bin_p","p (prob. éxito)", min=0.01, max=0.99, value=0.5, step=0.01),
            withMathJax(helpText("$$P(X=k)=\\binom{n}{k}p^k(1-p)^{n-k}$$")),
            stats_panel("bin_media","bin_mediana","bin_moda","bin_varianza"),
            calc_panel("bin", x_min=0, x_max=100, x_val=10, x_step=1)
          )),
          column(9, plotOutput("plot_bin", height="420px"))
        )),

        # D2. Poisson
        tabPanel("Poisson", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("poi_lambda","\\(\\lambda\\)", min=0.1, max=30, value=5, step=0.1),
            withMathJax(helpText("$$P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!},\\quad k=0,1,2,\\ldots$$")),
            stats_panel("poi_media","poi_mediana","poi_moda","poi_varianza"),
            calc_panel("poi", x_min=0, x_max=60, x_val=5, x_step=1)
          )),
          column(9, plotOutput("plot_poi", height="420px"))
        )),

        # D3. Binomial negativa
        tabPanel("Binomial negativa", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("nb_r","r (éxitos)", min=1, max=30, value=5, step=1),
            sliderInput("nb_p","p (prob. éxito)", min=0.01, max=0.99, value=0.5, step=0.01),
            withMathJax(helpText("$$P(X=k)=\\binom{k+r-1}{k}(1-p)^k p^r$$")),
            stats_panel("nb_media","nb_mediana","nb_moda","nb_varianza"),
            calc_panel("nb", x_min=0, x_max=100, x_val=5, x_step=1)
          )),
          column(9, plotOutput("plot_nb", height="420px"))
        )),

        # D4. Geométrica
        tabPanel("Geométrica", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("geo_p","p (prob. éxito)", min=0.01, max=0.99, value=0.3, step=0.01),
            withMathJax(helpText("$$P(X=k)=(1-p)^{k-1}p,\\quad k=1,2,\\ldots$$")),
            stats_panel("geo_media","geo_mediana","geo_moda","geo_varianza"),
            calc_panel("geo", x_min=1, x_max=50, x_val=3, x_step=1)
          )),
          column(9, plotOutput("plot_geo", height="420px"))
        )),

        # D5. Hipergeométrica
        tabPanel("Hipergeométrica", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("hg_N","N (tamaño población)", min=10, max=200, value=50, step=1),
            sliderInput("hg_K","K (éxitos en población)", min=1, max=199, value=20, step=1),
            sliderInput("hg_n","n (tamaño muestra)", min=1, max=100, value=10, step=1),
            withMathJax(helpText("$$P(X=k)=\\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}$$")),
            stats_panel("hg_media","hg_mediana","hg_moda","hg_varianza"),
            calc_panel("hg", x_min=0, x_max=50, x_val=4, x_step=1)
          )),
          column(9, plotOutput("plot_hg", height="420px"))
        ))
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # CONTINUAS
    # ════════════════════════════════════════════════════════════════════════
    tabPanel("Continuas",
      br(),
      tabsetPanel(

        tabPanel("Uniforme", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("unif_a","Mínimo (a)", min=0, max=99,  value=0, step=1),
            sliderInput("unif_b","Máximo (b)", min=1,  max=100, value=1, step=1),
            withMathJax(helpText("$$f(x)=\\frac{1}{b-a},\\quad a\\le x\\le b$$")),
            stats_panel("unif_media","unif_mediana","unif_moda","unif_varianza"),
            calc_panel("unif", x_min=0, x_max=100, x_val=0.5)
          )),
          column(9, plotOutput("plot_unif", height="420px"))
        )),

        tabPanel("Beta", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("beta_a","Forma \\(\\alpha\\)", min=1, max=100, value=2, step=1),
            sliderInput("beta_b","Forma \\(\\beta\\)",  min=1, max=100, value=5, step=1),
            withMathJax(helpText("$$f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)},\\;0<x<1$$")),
            stats_panel("beta_media","beta_mediana","beta_moda","beta_varianza"),
            calc_panel("beta", x_min=0, x_max=1, x_val=0.5, x_step=0.01)
          )),
          column(9, plotOutput("plot_beta", height="420px"))
        )),

        tabPanel("Normal", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("norm_mu",   "Media \\(\\mu\\)",           min=0, max=100, value=0, step=1),
            sliderInput("norm_sigma","Desv. típica \\(\\sigma\\)", min=0.1, max=10,  value=1, step=0.1),
            withMathJax(helpText("$$f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}}\\,e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$")),
            stats_panel("norm_media","norm_mediana","norm_moda","norm_varianza"),
            calc_panel("norm", x_min=0, x_max=130, x_val=5)
          )),
          column(9, plotOutput("plot_norm", height="420px"))
        )),

        tabPanel("Exponencial", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("exp_lambda","Tasa \\(\\lambda\\)", min=0.1, max=5, value=1, step=0.1),
            withMathJax(helpText("$$f(x)=\\lambda\\,e^{-\\lambda x},\\quad x\\ge 0$$")),
            stats_panel("exp_media","exp_mediana","exp_moda","exp_varianza"),
            calc_panel("exp", x_min=0, x_max=20, x_val=1, x_step=0.1)
          )),
          column(9, plotOutput("plot_exp", height="420px"))
        )),

        tabPanel("Gamma", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("gam_shape","Forma \\(\\alpha\\)", min=0.1, max=10, value=2, step=0.1),
            sliderInput("gam_rate", "Tasa \\(\\beta\\)",   min=0.1, max=5,  value=1, step=0.1),
            withMathJax(helpText("$$f(x)=\\frac{\\beta^{\\alpha}\\,x^{\\alpha-1}\\,e^{-\\beta x}}{\\Gamma(\\alpha)},\\;x>0$$")),
            stats_panel("gam_media","gam_mediana","gam_moda","gam_varianza"),
            calc_panel("gam", x_min=0, x_max=30, x_val=2, x_step=0.1)
          )),
          column(9, plotOutput("plot_gam", height="420px"))
        )),

        tabPanel("Chi-cuadrado", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("chi_df","Grados de libertad (k)", min=1, max=30, value=5, step=1),
            withMathJax(helpText("$$f(x)=\\frac{x^{k/2-1}\\,e^{-x/2}}{2^{k/2}\\,\\Gamma(k/2)},\\;x>0$$")),
            stats_panel("chi_media","chi_mediana","chi_moda","chi_varianza"),
            calc_panel("chi", x_min=0, x_max=60, x_val=5, x_step=0.1)
          )),
          column(9, plotOutput("plot_chi", height="420px"))
        )),

        tabPanel("t de Student", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("t_df","Grados de libertad \\(\\nu\\)", min=1, max=50, value=5, step=1),
            withMathJax(helpText("$$f(x)=\\frac{\\Gamma\\!\\left(\\frac{\\nu+1}{2}\\right)}{\\sqrt{\\nu\\pi}\\;\\Gamma\\!\\left(\\frac{\\nu}{2}\\right)}\\left(1+\\frac{x^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}$$")),
            stats_panel("t_media","t_mediana","t_moda","t_varianza"),
            calc_panel("t_st", x_min=-10, x_max=10, x_val=1.96)
          )),
          column(9, plotOutput("plot_t", height="420px"))
        )),

        tabPanel("F de Fisher", br(), fluidRow(
          column(3, wellPanel(
            sliderInput("f_df1","GL numerador \\(d_1\\)",   min=1, max=30, value=5,  step=1),
            sliderInput("f_df2","GL denominador \\(d_2\\)", min=1, max=30, value=10, step=1),
            withMathJax(helpText("$$f(x)=\\frac{\\sqrt{\\frac{(d_1 x)^{d_1}d_2^{d_2}}{(d_1 x+d_2)^{d_1+d_2}}}}{x\\,B\\!\\left(\\frac{d_1}{2},\\frac{d_2}{2}\\right)},\\;x>0$$")),
            stats_panel("f_media","f_mediana","f_moda","f_varianza"),
            calc_panel("f_dist", x_min=0, x_max=10, x_val=3, x_step=0.01)
          )),
          column(9, plotOutput("plot_f", height="420px"))
        ))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  observe({ if (input$unif_a >= input$unif_b) updateSliderInput(session,"unif_b",value=input$unif_a+0.5) })
  observe({ if (input$hg_K  >= input$hg_N)   updateSliderInput(session,"hg_K",  value=input$hg_N-1) })
  observe({ if (input$hg_n  >= input$hg_N)   updateSliderInput(session,"hg_n",  value=input$hg_N-1) })

  make_calc <- function(prefix, pfun, qfun) {
    output[[paste0(prefix,"_prob")]] <- renderText({
      input[[paste0(prefix,"_calc_p")]]
      isolate({
        x <- as.numeric(input[[paste0(prefix,"_x")]])
        if (is.null(x)||is.na(x)) return("Introduce un valor de x")
        paste0("P(X \u2264 ",round(x,4),") = ",round(pfun(x),6))
      })
    })
    output[[paste0(prefix,"_quant")]] <- renderText({
      input[[paste0(prefix,"_calc_q")]]
      isolate({
        p <- as.numeric(input[[paste0(prefix,"_p")]])
        if (is.null(p)||is.na(p)||p<=0||p>=1) return("p debe estar entre 0 y 1")
        paste0("P(X \u2264 ",round(qfun(p),6),") = ",round(p,4))
      })
    })
  }

  # ── Función auxiliar para gráficos discretos ─────────────────────────────
  plot_discreta <- function(k, prob, media, mediana, moda_val, titulo) {
    df <- data.frame(k=k, prob=prob)
    p <- ggplot(df, aes(x=k, y=prob)) +
      geom_col(fill="#428bca", width=0.6, alpha=0.8) +
      geom_col(data=subset(df, k==round(moda_val)), aes(x=k,y=prob),
               fill="#2c6fad", width=0.6) +
      vline_media(media) + vline_mediana(mediana) + vline_moda(moda_val) +
      labs(title=titulo, x="k", y="P(X = k)") + tema_discreta
    p
  }

  # ── D1. Binomial ─────────────────────────────────────────────────────────
  output$plot_bin <- renderPlot({
    n <- input$bin_n; p <- input$bin_p
    k <- 0:n
    plot_discreta(k, dbinom(k,n,p), n*p, qbinom(0.5,n,p),
                  floor((n+1)*p),
                  paste0("Distribución Binomial  —  n=",n,", p=",p))
  })
  output$bin_media    <- renderText({ round(input$bin_n*input$bin_p, 4) })
  output$bin_mediana  <- renderText({ qbinom(0.5, input$bin_n, input$bin_p) })
  output$bin_moda     <- renderText({ floor((input$bin_n+1)*input$bin_p) })
  output$bin_varianza <- renderText({ round(input$bin_n*input$bin_p*(1-input$bin_p), 4) })
  observe({ make_calc("bin",
    pfun=function(x) pbinom(floor(x), input$bin_n, input$bin_p),
    qfun=function(p) qbinom(p, input$bin_n, input$bin_p)) })

  # ── D2. Poisson ───────────────────────────────────────────────────────────
  output$plot_poi <- renderPlot({
    lambda <- input$poi_lambda
    k_max  <- qpois(0.999, lambda)
    k      <- 0:k_max
    plot_discreta(k, dpois(k,lambda), lambda, qpois(0.5,lambda),
                  floor(lambda),
                  paste0("Distribución Poisson  —  \u03bb=",lambda))
  })
  output$poi_media    <- renderText({ input$poi_lambda })
  output$poi_mediana  <- renderText({ qpois(0.5, input$poi_lambda) })
  output$poi_moda     <- renderText({ floor(input$poi_lambda) })
  output$poi_varianza <- renderText({ input$poi_lambda })
  observe({ make_calc("poi",
    pfun=function(x) ppois(floor(x), input$poi_lambda),
    qfun=function(p) qpois(p, input$poi_lambda)) })

  # ── D3. Binomial negativa ─────────────────────────────────────────────────
  # X = nº de fracasos antes del r-ésimo éxito
  output$plot_nb <- renderPlot({
    r <- input$nb_r; p <- input$nb_p
    k_max <- qnbinom(0.999, size=r, prob=p)
    k     <- 0:k_max
    media    <- r*(1-p)/p
    mediana  <- qnbinom(0.5, size=r, prob=p)
    moda_val <- if (r>1) floor((r-1)*(1-p)/p) else 0
    plot_discreta(k, dnbinom(k,size=r,prob=p), media, mediana, moda_val,
                  paste0("Binomial Negativa  —  r=",r,", p=",p))
  })
  output$nb_media    <- renderText({ round(input$nb_r*(1-input$nb_p)/input$nb_p, 4) })
  output$nb_mediana  <- renderText({ qnbinom(0.5, size=input$nb_r, prob=input$nb_p) })
  output$nb_moda     <- renderText({ r<-input$nb_r; p<-input$nb_p; if(r>1) floor((r-1)*(1-p)/p) else 0 })
  output$nb_varianza <- renderText({ round(input$nb_r*(1-input$nb_p)/input$nb_p^2, 4) })
  observe({ make_calc("nb",
    pfun=function(x) pnbinom(floor(x), size=input$nb_r, prob=input$nb_p),
    qfun=function(p) qnbinom(p, size=input$nb_r, prob=input$nb_p)) })

  # ── D4. Geométrica ────────────────────────────────────────────────────────
  # X = nº de ensayos hasta el primer éxito (k = 1,2,...)
  output$plot_geo <- renderPlot({
    p     <- input$geo_p
    k_max <- qgeom(0.999, prob=p) + 1
    k     <- 1:k_max
    media   <- 1/p
    mediana <- ceiling(-log(2)/log(1-p))
    plot_discreta(k, dgeom(k-1, prob=p), media, mediana, 1,
                  paste0("Distribución Geométrica  —  p=",p))
  })
  output$geo_media    <- renderText({ round(1/input$geo_p, 4) })
  output$geo_mediana  <- renderText({ ceiling(-log(2)/log(1-input$geo_p)) })
  output$geo_moda     <- renderText({ "1" })
  output$geo_varianza <- renderText({ round((1-input$geo_p)/input$geo_p^2, 4) })
  observe({ make_calc("geo",
    pfun=function(x) pgeom(floor(x)-1, prob=input$geo_p),
    qfun=function(p) qgeom(p, prob=input$geo_p)+1) })

  # ── D5. Hipergeométrica ───────────────────────────────────────────────────
  output$plot_hg <- renderPlot({
    N <- input$hg_N; K <- min(input$hg_K, N-1); n <- min(input$hg_n, N-1)
    k_min <- max(0, n+K-N); k_max <- min(K, n)
    k     <- k_min:k_max
    media    <- n*K/N
    mediana  <- round(n*K/N)  # aproximación
    moda_val <- floor((n+1)*(K+1)/(N+2))
    plot_discreta(k, dhyper(k,K,N-K,n), media, mediana, moda_val,
                  paste0("Hipergeométrica  —  N=",N,", K=",K,", n=",n))
  })
  output$hg_media    <- renderText({ N<-input$hg_N; K<-input$hg_K; n<-input$hg_n; round(n*K/N,4) })
  output$hg_mediana  <- renderText({ N<-input$hg_N; K<-input$hg_K; n<-input$hg_n; round(n*K/N) })
  output$hg_moda     <- renderText({ N<-input$hg_N; K<-input$hg_K; n<-input$hg_n; floor((n+1)*(K+1)/(N+2)) })
  output$hg_varianza <- renderText({ N<-input$hg_N; K<-input$hg_K; n<-input$hg_n; round(n*(K/N)*((N-K)/N)*((N-n)/(N-1)),4) })
  observe({ make_calc("hg",
    pfun=function(x) phyper(floor(x), input$hg_K, input$hg_N-input$hg_K, input$hg_n),
    qfun=function(p) qhyper(p, input$hg_K, input$hg_N-input$hg_K, input$hg_n)) })

  # ── Continuas ────────────────────────────────────────────────────────────

  # Uniforme
  output$plot_unif <- renderPlot({
    a <- input$unif_a; b <- input$unif_b; if(a>=b) return(NULL)
    media <- (a+b)/2; margen <- (b-a)*0.4
    x <- seq(a-margen, b+margen, length.out=1000); y <- dunif(x,min=a,max=b)
    df <- data.frame(x=x,y=y)
    ggplot(df,aes(x,y)) +
      geom_ribbon(data=subset(df,x>=a&x<=b),aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3) +
      geom_line(color="#2c6fad",linewidth=1.2) +
      vline_media(media)+vline_mediana(media)+vline_moda(media) +
      labs(title=paste0("Distribución Uniforme  —  U(a=",a,", b=",b,")"),x="x",y="f(x)")+tema_continua
  })
  output$unif_media    <- renderText({ a<-input$unif_a;b<-input$unif_b; round((a+b)/2,4) })
  output$unif_mediana  <- renderText({ a<-input$unif_a;b<-input$unif_b; round((a+b)/2,4) })
  output$unif_moda     <- renderText({ "cualquier valor en [a, b]" })
  output$unif_varianza <- renderText({ a<-input$unif_a;b<-input$unif_b; round((b-a)^2/12,4) })
  observe({ make_calc("unif", pfun=function(x) punif(x,input$unif_a,input$unif_b),
                               qfun=function(p) qunif(p,input$unif_a,input$unif_b)) })

  # Beta
  output$plot_beta <- renderPlot({
    a<-input$beta_a; b<-input$beta_b
    media<-a/(a+b); mediana<-qbeta(0.5,a,b); moda_val<-if(a>1&&b>1)(a-1)/(a+b-2) else NA
    x<-seq(0.001,0.999,length.out=1000); y<-dbeta(x,a,b)
    df<-data.frame(x=x,y=pmin(y,15))
    p<-ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(media)+vline_mediana(mediana)
    if(!is.na(moda_val)) p<-p+vline_moda(moda_val)
    p+labs(title=paste0("Distribución Beta  —  \u03b1=",a,", \u03b2=",b),x="x",y="f(x)")+tema_continua
  })
  output$beta_media    <- renderText({ a<-input$beta_a;b<-input$beta_b; round(a/(a+b),4) })
  output$beta_mediana  <- renderText({ round(qbeta(0.5,input$beta_a,input$beta_b),4) })
  output$beta_moda     <- renderText({ a<-input$beta_a;b<-input$beta_b
    if(a>1&&b>1) round((a-1)/(a+b-2),4) else if(a<=1&&b<=1) "0 y 1 (bimodal)" else if(a<=1) "0" else "1" })
  output$beta_varianza <- renderText({ a<-input$beta_a;b<-input$beta_b; round((a*b)/((a+b)^2*(a+b+1)),4) })
  observe({ make_calc("beta", pfun=function(x) pbeta(x,input$beta_a,input$beta_b),
                               qfun=function(p) qbeta(p,input$beta_a,input$beta_b)) })

  # Normal
  output$plot_norm <- renderPlot({
    mu<-input$norm_mu; sigma<-input$norm_sigma
    x<-seq(mu-4*sigma,mu+4*sigma,length.out=1000); y<-dnorm(x,mean=mu,sd=sigma)
    df<-data.frame(x=x,y=y)
    ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(mu)+vline_mediana(mu)+vline_moda(mu)+
      labs(title=paste0("Distribución Normal  —  \u03bc=",mu,", \u03c3=",sigma),x="x",y="f(x)")+tema_continua
  })
  output$norm_media    <- renderText({ round(input$norm_mu,4) })
  output$norm_mediana  <- renderText({ round(input$norm_mu,4) })
  output$norm_moda     <- renderText({ round(input$norm_mu,4) })
  output$norm_varianza <- renderText({ round(input$norm_sigma^2,4) })
  observe({ make_calc("norm", pfun=function(x) pnorm(x,input$norm_mu,input$norm_sigma),
                               qfun=function(p) qnorm(p,input$norm_mu,input$norm_sigma)) })

  # Exponencial
  output$plot_exp <- renderPlot({
    lambda<-input$exp_lambda; media<-1/lambda; mediana<-log(2)/lambda
    x<-seq(0,qexp(0.999,rate=lambda),length.out=1000); y<-dexp(x,rate=lambda)
    df<-data.frame(x=x,y=y)
    ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(media)+vline_mediana(mediana)+vline_moda(0)+
      labs(title=paste0("Distribución Exponencial  —  \u03bb=",lambda),x="x",y="f(x)")+tema_continua
  })
  output$exp_media    <- renderText({ round(1/input$exp_lambda,4) })
  output$exp_mediana  <- renderText({ round(log(2)/input$exp_lambda,4) })
  output$exp_moda     <- renderText({ "0" })
  output$exp_varianza <- renderText({ round(1/input$exp_lambda^2,4) })
  observe({ make_calc("exp", pfun=function(x) pexp(x,rate=input$exp_lambda),
                              qfun=function(p) qexp(p,rate=input$exp_lambda)) })

  # Gamma
  output$plot_gam <- renderPlot({
    shape<-input$gam_shape; rate<-input$gam_rate
    media<-shape/rate; mediana<-qgamma(0.5,shape=shape,rate=rate)
    moda_val<-if(shape>=1)(shape-1)/rate else 0
    x<-seq(0.001,qgamma(0.999,shape=shape,rate=rate),length.out=1000); y<-dgamma(x,shape=shape,rate=rate)
    df<-data.frame(x=x,y=y)
    ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(media)+vline_mediana(mediana)+vline_moda(moda_val)+
      labs(title=paste0("Distribución Gamma  —  \u03b1=",shape,", \u03b2=",rate),x="x",y="f(x)")+tema_continua
  })
  output$gam_media    <- renderText({ round(input$gam_shape/input$gam_rate,4) })
  output$gam_mediana  <- renderText({ round(qgamma(0.5,shape=input$gam_shape,rate=input$gam_rate),4) })
  output$gam_moda     <- renderText({ shape<-input$gam_shape;rate<-input$gam_rate
    if(shape>=1) round((shape-1)/rate,4) else "0 (moda en el límite)" })
  output$gam_varianza <- renderText({ round(input$gam_shape/input$gam_rate^2,4) })
  observe({ make_calc("gam", pfun=function(x) pgamma(x,shape=input$gam_shape,rate=input$gam_rate),
                              qfun=function(p) qgamma(p,shape=input$gam_shape,rate=input$gam_rate)) })

  # Chi-cuadrado
  output$plot_chi <- renderPlot({
    k<-input$chi_df; media<-k; mediana<-qchisq(0.5,df=k); moda_val<-max(k-2,0)
    x<-seq(0.01,qchisq(0.999,df=k),length.out=1000); y<-dchisq(x,df=k)
    df<-data.frame(x=x,y=y)
    ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(media)+vline_mediana(mediana)+
      {if(moda_val>0) vline_moda(moda_val) else NULL}+
      labs(title=paste0("Distribución Chi-cuadrado  —  k=",k),x="x",y="f(x)")+tema_continua
  })
  output$chi_media    <- renderText({ input$chi_df })
  output$chi_mediana  <- renderText({ round(qchisq(0.5,df=input$chi_df),4) })
  output$chi_moda     <- renderText({ k<-input$chi_df; if(k>=2) k-2 else "0 (k < 2)" })
  output$chi_varianza <- renderText({ 2*input$chi_df })
  observe({ make_calc("chi", pfun=function(x) pchisq(x,df=input$chi_df),
                              qfun=function(p) qchisq(p,df=input$chi_df)) })

  # t de Student
  output$plot_t <- renderPlot({
    nu<-input$t_df
    x<-seq(qt(0.001,df=nu),qt(0.999,df=nu),length.out=1000); y<-dt(x,df=nu)
    df<-data.frame(x=x,y=y)
    ggplot(df,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_media(0)+vline_mediana(0)+vline_moda(0)+
      labs(title=paste0("Distribución t de Student  —  \u03bd=",nu),x="x",y="f(x)")+tema_continua
  })
  output$t_media    <- renderText({ if(input$t_df>1) "0" else "indefinida (\u03bd \u2264 1)" })
  output$t_mediana  <- renderText({ "0" })
  output$t_moda     <- renderText({ "0" })
  output$t_varianza <- renderText({ nu<-input$t_df
    if(nu>2) round(nu/(nu-2),4) else if(nu>1) "\u221e (\u03bd \u2264 2)" else "indefinida (\u03bd \u2264 1)" })
  observe({ make_calc("t_st", pfun=function(x) pt(x,df=input$t_df),
                               qfun=function(p) qt(p,df=input$t_df)) })

  # F de Fisher
  output$plot_f <- renderPlot({
    d1<-input$f_df1; d2<-input$f_df2
    media<-if(d2>2) d2/(d2-2) else NA; mediana<-qf(0.5,df1=d1,df2=d2)
    moda_val<-if(d1>2)((d1-2)/d1)*(d2/(d2+2)) else NA
    x<-seq(0.001,qf(0.999,df1=d1,df2=d2),length.out=1000); y<-df(x,df1=d1,df2=d2)
    dff<-data.frame(x=x,y=y)
    p<-ggplot(dff,aes(x,y))+geom_ribbon(aes(ymin=0,ymax=y),fill="#428bca",alpha=0.3)+
      geom_line(color="#2c6fad",linewidth=1.2)+vline_mediana(mediana)
    if(!is.na(media))    p<-p+vline_media(media)
    if(!is.na(moda_val)) p<-p+vline_moda(moda_val)
    p+labs(title=paste0("Distribución F de Fisher  —  d\u2081=",d1,", d\u2082=",d2),x="x",y="f(x)")+tema_continua
  })
  output$f_media    <- renderText({ d2<-input$f_df2; if(d2>2) round(d2/(d2-2),4) else "indefinida (d\u2082 \u2264 2)" })
  output$f_mediana  <- renderText({ round(qf(0.5,df1=input$f_df1,df2=input$f_df2),4) })
  output$f_moda     <- renderText({ d1<-input$f_df1;d2<-input$f_df2
    if(d1>2) round(((d1-2)/d1)*(d2/(d2+2)),4) else "0 (d\u2081 \u2264 2)" })
  output$f_varianza <- renderText({ d1<-input$f_df1;d2<-input$f_df2
    if(d2>4) round((2*d2^2*(d1+d2-2))/(d1*(d2-2)^2*(d2-4)),4)
    else if(d2>2) "\u221e (d\u2082 \u2264 4)" else "indefinida (d\u2082 \u2264 2)" })
  observe({ make_calc("f_dist", pfun=function(x) pf(x,df1=input$f_df1,df2=input$f_df2),
                                 qfun=function(p) qf(p,df1=input$f_df1,df2=input$f_df2)) })
}

shinyApp(ui, server)