#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  # reprodutivel
  set.seed(1)

  da <- shiny::reactive({
    input$gerar
    shiny::isolate({
      N <- 100
      x <- mvtnorm::rmvnorm(2*N, sigma = diag(stats::runif(2)))
      y <- 2 + input$beta1*x[,1] + input$beta2*x[,2] + stats::rnorm(N, sd = .01)
      list(x = x, y = y)
    })

  })

  p_curva <- shiny::reactive({
    y <- da()$y
    x <- da()$x

    perda <- function(beta1, beta2, y, x, beta0 = 2) {
      sum((y - beta0 - x[,1]*beta1 - x[,2]*beta2)^2)
    }

    da_plot <- list(beta1 = seq(-5, 5, 0.05), beta2 = seq(-5, 5, 0.05)) %>%
      purrr::cross_df() %>%
      dplyr::mutate(loss = purrr::map2_dbl(beta1, beta2, perda, y, x))

    shiny::isolate({

      da_plot %>%
        ggplot2::ggplot(ggplot2::aes(beta1, beta2)) +
        ggplot2::geom_contour(
          ggplot2::aes(
            z = loss,
            colour = stat(level)
          ),
          bins = 40,
          show.legend = FALSE,
          size = .3
        ) +
        ggplot2::annotate(
          "point",
          input$beta1,
          input$beta2,
          colour = "red"
        ) +
        ggplot2::scale_colour_distiller(
          palette = "Purples"
        )
      })

  })


  otimo <- shiny::reactive({
    B <- input$restricao
    p_curva()$data %>%
      dplyr::filter(abs(beta1) + abs(beta2) <= B) %>%
      dplyr::arrange(loss) %>%
      dplyr::slice(1)
  })

  output$grafico <- plotly::renderPlotly({


    B <- input$restricao
    polyg <- tibble::tibble(
      beta1 = c(-B, 0, B, 0),
      beta2 = c(0, B, 0, -B)
    )

    p_final <- p_curva() +
      ggplot2::geom_polygon(
        alpha = .5,
        data = polyg,
        fill = "purple",
      ) +
      ggplot2::annotate(
        "point",
        otimo()$beta1,
        otimo()$beta2,
        colour = "blue",
        size = 2
      ) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::theme_minimal() +
      ggplot2::coord_equal(xlim = c(-5, 5), ylim = c(-5, 5))

    plotly::ggplotly(p_final)

  })

  output$tab_lasso <- shiny::renderTable({

    otimo()

  })


}
