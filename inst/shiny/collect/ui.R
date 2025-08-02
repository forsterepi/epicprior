#' UI of Domain Expertise Collector `Shiny` app
#'
#' @noRd
ui <- bslib::page_navbar(
  title = "Domain Expertise Collector",
  window_title = "epicprior",
  navbar_options = bslib::navbar_options(
    bg = "#2D89C8",
    collapsible = FALSE
  ),
  bslib::nav_panel(
    title = "Instructions",
    bslib::navset_pill_list(
      widths = c(3, 9),
      bslib::nav_panel(
        "What is domain expertise?",
        shiny::markdown("When conducting a statistical analysis, we always
        have a research question in mind that we want to answer.
        A statistical analysis that is useful in this regard needs to
        incorporate domain knowledge. The analyst, however, knows little
        about the topic of research compared to the domain experts, e.g.,
        physicians. Unfortunately, for domain expertise to be useful
        in statistical modelling, it needs to be translated into
        statistical distributions. This Domain Expertise Collector
        tries to help with this task. In this file, domain experts
        need to provide their knowledge by specifying thresholds.")
      ),
      bslib::nav_panel(
        "Thresholds",
        shiny::markdown("Thresholds are specified for each model
        parameter. Ideally, domain experts and analyst already talked
        about the preferred parameterisation, i.e., which parameters
        are used and what they mean. Thresholds separate reasonable
        parameter values from extreme parameter values. Usually, we
        need two thresholds for each parameter: The first one separates
        reasonable from extremely low values and the second one
        separates reasonable from extremely high values. Another way
        of thinking about extreme and reasonable values is imagining
        one’s reaction when learning that a certain value is actually
        the true value. If you would be surprised, it’s an extreme
        value; otherwise, it's reasonable and you might react with:
        'OK, I can see that, maybe not my first guess but I believe it.'"),
        shiny::markdown("A threshold in this context is a numerical value,
        e.g., if our lower threshold is 0.4, we assume that values <0.4
        are extremely low, while values between 0.4 and the upper
        threshold are reasonable. Obviously, defining a hard border
        between reasonable and extreme with a single value leads to
        weird conclusions, e.g., that 0.399999 is an extreme value
        but that 0.400001 is reasonable. This is, however, not the
        interpretation we aim for. The threshold needs to be somewhere
        in the range of values where the (slow) transition from
        reasonable to extreme takes place. Any value in that range
        will do. The precision we achieve this way is usually good enough.")
      ),
      bslib::nav_panel(
        "Example",
        shiny::markdown("As an example, let's estimate the height of the
        Eiffel Tower. It was built around 1900, so it's probably not
        much higher than, let's say, 500m. Even if it's 400m, I would be
        impressed, but I cannot really rule it out. Everything above 500m would
        be really surprising though. It's probably not smaller than
        150m. I think it was the tallest building in the world for
        some time and aren't the tallest pyramids already that tall?"),
        shiny::markdown("That's what someone who can only rely on common
        knowledge would think. However, someone who knows that Eiffel's
        goal was to build the first tower that is ~300m (1000 feet) tall, would
        come to a different conclusion: Since Eiffel achieved his goal,
        the tower must be above 300m. And since it was difficult to
        build that high, it's probably not much taller than that.
        Everything above 350m would be surprising. The thresholds
        would, therefore, be 300 and 350.")
      ),
      bslib::nav_panel(
        "Workflow",
        shiny::markdown("
        1) First, select the theoretically possible range of values that
        your parameter can take. The range is important as it determines
        what type of distributions are possible. There are three possible ranges:

        *	0 to 1: Parameters like proportions or probabilities can only
        take values from 0 to 1 (or from 0% to 100%)

        *	0 to Infinity (Inf), i.e., 0 and all positive numbers:
        Variables like age, height, weight, etc. can only take positive values

        *	-Infinity to Infinity, i.e., all numbers: Parameters that, e.g.,
        have a direction, such as regression coefficients, can take
        negative values as well

        2)	Your task is to select a lower threshold and an upper
        threshold for each parameter. Ideally, the parameter is rooted
        in reality and you have an idea of what it means. If this is not
        the case, tell your analyst that a re-parameterisation is necessary.

        3)	In case your parameter has range '0 to 1', special cases may arise:

        *	If reasonable values are close to either 0 or 1, you can select
        0 or 1 as one of the thresholds. In this case, the app offers
        two possible shapes ('Beta' and 'Half-Normal'). Their main difference is
        how fast values become unlikely when moving away from 0 or 1.
        For 'Beta', they become unlikely very quickly, while for
        'Half-Normal', the likelihood decreases more slowly.

        *	If values from 0 to 1 seem reasonable, you can set
        both thresholds to 0 and 1. In this case, you need to
        additionally select the mean, i.e., average, i.e., most
        likely value (roughly), of the distribution. We recommend
        selecting a value between 25% and 75% in 5%-steps, i.e.,
        25%, 30%, 35%, etc. When your thresholds are 0 and 1, you do
        not have to select a shape ('Beta' or 'Half-Normal'). This is
        only necessary if one of your thresholds is 0 or 1 and the other is not.

        4)	In case your parameter has range '0 to Infinity', you
        can also select a shape. Available options are 'Gamma',
        'Log-Normal', and 'Truncated Normal', but unfortunately,
        they differ in more complicated ways. In general, for
        'Gamma' and 'Log-Normal', values closer to 0 are more
        likely, compared to 'Truncated Normal' which is more
        symmetrical. For Odds Ratios, 'Log-Normal' is ideal.

        5)	In case your parameter has range '-Infinity to Infinity',
        there are no additional requirements. Just specify your thresholds.

        6)	Always compare the quantiles from the plot (numbers next
        to the dashed lines) to your thresholds. Ideally, they should
        be the same. When they are not, you did not find the ideal
        distribution for your thresholds. For parameters with range
        '0 to Infinity', you can try to change the shape or the
        starting values in the 'Advanced Options' ('Gamma' and
        'Log-Normal' only). When all starting values are 0, the
        default starting values provided by package *fitdistrplus*
        are used. See the console for error messeges related to the
        *fitdistrplus* functions, e.g., regarding starting values. The title
        of the plot specifies the displayed distribution.
      ")
      )
    )
  ),
  bslib::nav_panel(
    title = "Select Prior",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput("range",
          label = "Select Range of Values",
          choices = list(
            "0 to 1" = 1,
            "0 to Inf" = 2,
            "-Inf to Inf" = 3
          ),
          selected = 1
        ),
        shiny::conditionalPanel(
          condition = "input.range == '1'",
          shiny::sliderInput("t01",
            label = "Thresholds",
            min = 0,
            max = 1,
            step = 0.01,
            value = c(0.4, 0.6)
          ),
          shiny::uiOutput("meanbetaui"),
          shiny::uiOutput("type01ui")
        ),
        shiny::conditionalPanel(
          condition = "input.range == '2'",
          shiny::numericInput("lt0inf",
            label = "Lower Threshold",
            value = 1,
            step = 0.2,
            min = 0
          ),
          shiny::numericInput("ut0inf",
            label = "Upper Threshold",
            value = 2,
            step = 1,
            min = 0
          ),
          shiny::selectInput("type0inf",
            label = "Select Shape",
            choices = list(
              "Gamma" = 1,
              "Log-Normal" = 2,
              "Truncated Normal" = 3
            ),
            selected = 1
          )
        ),
        shiny::conditionalPanel(
          condition = "input.range == '3'",
          shiny::numericInput("ltinfinf",
            label = "Lower Threshold",
            value = 1
          ),
          shiny::numericInput("utinfinf",
            label = "Upper Threshold",
            value = 2
          )
        ),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            "Advanced Options",
            shiny::numericInput("lq",
              label = "Lower Quantile",
              value = 0.01,
              min = 0.005,
              max = 0.995,
              step = 0.005
            ),
            shiny::numericInput("uq",
              label = "Upper Quantile",
              value = 0.99,
              min = 0.005,
              max = 0.995,
              step = 0.005
            ),
            shiny::conditionalPanel(
              condition = "input.range == '2' & input.type0inf == '1'",
              shiny::numericInput("startshape",
                label = "Starting Value: shape",
                value = 1,
                min = 0,
                step = 0.5
              ),
              shiny::numericInput("startrate",
                label = "Starting Value: rate",
                value = 1,
                min = 0,
                step = 0.5
              )
            ),
            shiny::conditionalPanel(
              condition = "input.range == '2' & input.type0inf == '2'",
              shiny::numericInput("startmeanlog",
                label = "Starting Value: meanlog",
                value = 1,
                min = 0,
                step = 0.5
              ),
              shiny::numericInput("startsdlog",
                label = "Starting Value: sdlog",
                value = 1,
                min = 0,
                step = 0.5
              )
            )
          )
        )
      ),
      shiny::plotOutput(outputId = "distplot")
    )
  )
)
