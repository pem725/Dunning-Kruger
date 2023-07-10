#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fabricatr)
library(fmsb)
library(ggplot2)
library(gridExtra)
library(xkcd)
library(xkcdcolors)
library(extrafont)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulating the Dunning-Kruger Effect"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "Sample Size:",
                        min = 40,
                        max = 1000,
                        value = 84),
            sliderInput("rxx",
                        "Relationship Between Actual and Perceived Abilities:",
                        min = 0,
                        max = 1,
                        value = .7),
            sliderInput("bias",
                        "Bias in Self-Reporting Ability (Percent):",
                        min=0,
                        max=100,
                        value=0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Original Dunning-Kruger Plot", plotOutput("p1")),
                tabPanel("Bell-Volkmann Plot", plotOutput("p2")),
                tabPanel("Pazicni-Bauer Plot", plotOutput("p3")),
                tabPanel("Histogram of Self-Assessed Accuracy", plotOutput("p4")),
                tabPanel("The Calibration Curve", plotOutput("p5"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df.l <- reactive({
        Noise <- rnorm(input$N)
        Ab <- rnorm(input$N)
        PerAb <- scale(Ab)*sqrt(input$rxx) + (scale(residuals(lm(Noise~Ab))) * sqrt(1-input$rxx)) - (input$bias/100*scale(Ab))
        Ab.quant <- as.numeric(as.character(split_quantile(Ab,4)))
        Ab.thirds <- split_quantile(Ab,3)
        Ab.perc <- percentile(Ab)
        PerAb.quant <- split_quantile(PerAb,4)
        PerAb.thirds <- split_quantile(PerAb,3)
        PerAb.perc <- percentile(PerAb)
        Delta <- PerAb.perc - Ab.perc
        #df.w <- data.frame(Noise,Ab,PerAb,Ab.quant,Ab.thirds,Ab.perc,PerAb.quant,PerAb.thirds,PerAb.perc,Delta)
        data.frame(Qtls=c(Ab.quant,Ab.quant),Meas=gl(2,length(Ab),labels=c("Ab","PerAb")),Percs=c(Ab.perc,PerAb.perc))
    })
    
    df.w <- reactive({
        Noise <- rnorm(input$N)
        Ab <- rnorm(input$N)
        PerAb <- scale(Ab)*sqrt(input$rxx) + (scale(residuals(lm(Noise~Ab))) * sqrt(1-input$rxx)) - (input$bias/100*scale(Ab))
        Ab.quant <- as.numeric(as.character(split_quantile(Ab,4)))
        Ab.thirds <- as.numeric(as.character(split_quantile(Ab,3)))
        Ab.perc <- percentile(Ab)
        PerAb.quant <- split_quantile(PerAb,4)
        PerAb.thirds <- split_quantile(PerAb,3)
        PerAb.perc <- percentile(PerAb)
        Delta <- PerAb.perc - Ab.perc
        #df.w <- data.frame(Noise,Ab,PerAb,Ab.quant,Ab.thirds,Ab.perc,PerAb.quant,PerAb.thirds,PerAb.perc,Delta)
        data.frame(Ability=Ab.perc,Perception=PerAb.perc,Quants=Ab.quant,Thirds=Ab.thirds,Delta=Delta)
    })
    
    output$p1 <- renderPlot({
        #df.w <- df.w()
        #df.w.1 <- df.w[df.w$Quants==1,]
        h2.t <- t.test(Pair(Ability, Perception)~1, data=df.w()[df.w()$Quants==1,])
        p1 <- ggplot(df.l(),aes(x=Qtls,y=Percs,colour=Meas)) + 
            stat_summary(fun="mean", geom="line", size=2, aes(group=factor(Meas))) + 
            stat_summary(fun="mean", geom = "point", size=5) +
            ylab("Percentile") +
            scale_color_discrete(name=NULL, labels=c("Actual Ability", "Perceived Ability")) +
            theme(legend.position = c(.7,.2), 
                  legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid", colour ="darkblue"),
                  legend.text = element_text(size=20,face="bold"), 
                  axis.text = element_text(face="bold", size=20),
                  axis.ticks.x = element_blank()) +
            scale_x_continuous(name=NULL,breaks=c(1.1,2,3,3.9),labels = c("Bottom\nQuartile","2nd\nQuartile","3rd\nQuartile","Top\nQuartile")) +
            scale_y_continuous(breaks=seq(0,100,10)) + 
            annotate("text", x=1.2, y=80, size=7,label=paste("t(",input$N - 1,") = ",round(h2.t$statistic,2), "\np = ",round(h2.t$p.value,2),sep="")) +
            theme_xkcd()
        plot(p1)
    })
    
    output$p2 <- renderPlot({
        p2 <- ggplot(df.w(),aes(x=Thirds,y=Delta)) + 
            #geom_col() +
            stat_summary(fun="mean", geom="bar", size=2, aes(group=factor(Thirds))) + 
            ylab("Mean (Perceived - Actual)") + 
            theme(axis.text = element_text(face="bold", size=20),
                  axis.ticks.x = element_blank()) +
            scale_x_continuous(name=NULL,breaks=c(1,2,3),labels = c("Lowest\nThird","Middle\nThird","Highest\nThird")) +
            scale_y_continuous(breaks=seq(0,100,10)) +
            geom_hline(yintercept=0,colour="red",size=2) +
            annotate("text", x=2, y=2, size=20, col="red", label = "Overconfidence", family="xkcd") +
            annotate("text", x=2, y=-2, size=20, col="blue", label = "Underconfidence", family="xkcd") + 
            theme_xkcd()
        plot(p2)
    })
    
    output$p3 <- renderPlot({
        dat <- df.w()
        p3 <- ggplot(dat,aes(x=Ability,y=Delta)) + 
            geom_point() +
            geom_smooth(method=lm,se=T,col="blue") +
            ylab("Mean (Perceived - Actual)") + 
            theme(axis.text = element_text(face="bold", size=20),
                  axis.ticks.x = element_blank()) +
            scale_x_continuous(name="Ability Scores",breaks=seq(0,100,10)) +
            scale_y_continuous(breaks=seq(-100,100,10)) +
            geom_hline(yintercept=0,colour="red",size=2) +
            annotate("text", x=5, y=-20, size=10, col="blue", label = paste("r =",round(cor(dat$Ability,dat$Delta,use="pairwise.complete.obs"),2)), family="xkcd") +
            theme_xkcd()
        plot(p3)
    })
    
    output$p4 <- renderPlot({
        dat <- df.w()
        p4 <- ggplot(dat,aes(Delta)) + 
            geom_histogram(aes(y=(..count..)/sum(..count..)),bins=20,colour="black",fill="darkred") +
            stat_bin(aes(y=(..count..)/sum(..count..),
                     label=paste0(round((..count..)/sum(..count..)*100),"%")),
                     geom="text", size=5, bins = 20, vjust=-1) +
            xlab(expression(paste("Perceived Percentile - Actual Percentile\n(Higher Values Indicate Overconfidence)"))) + 
            ylab("Percent of Sample") +
            theme(axis.text = element_text(face="bold", size=21),
                  axis.ticks.x = element_blank()) +
            scale_x_continuous(name="Ability Scores",breaks=seq(-100,100,10)) +
            xlim(-100,100) +
            ylim(0,.5) +
            theme_xkcd()
        plot(p4)
    })
    
    output$p5 <- renderPlot({
        #dat <- df.w()
        p5 <- ggplot(df.w(),aes(Ability,Perception)) + 
            geom_smooth() +
            geom_abline(intercept=0,slope=1,colour="red",size=1,linetype="longdash") +
            ylab("Perceived Percentile") + 
            xlab("Actual Percentile") +
            theme(axis.text = element_text(face="bold", size=21)) +
            #scale_x_continuous(name="Ability Percentile",breaks=seq(0,100,10)) +
            #scale_y_continuous(name="Perceived Ability Percentile",breaks=seq(0,100,10)) +
            xlim(0,100) +
            ylim(0,100) +
            annotate("text", x=30, y=75, size=15, col="red", label = "OVERCONFIDENCE", family="xkcd") +
            annotate("text", x=70, y=25, size=15, col="blue", label = "UNDERCONFIDENCE", family="xkcd") +
            theme_xkcd()
        plot(p5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
