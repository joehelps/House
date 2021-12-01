#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Buying or renting?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h1("Expected house and mortage:"),
            numericInput("housePrice","House price:",min=0,max=Inf,value=300000),
            numericInput("expectedRent","Rent (monthly) if not buying",min=0,max=Inf,value=1250),
            h1("Mortgage:"),
            numericInput("deposit","Deposit amount",min=0,max=Inf,value=30000),
            numericInput("mortgageAmount","Mortgage value",min=0,max=Inf,value=270000),
            numericInput("mortgageTerm","Mortgage term",min=0,max=100,value=30),
            numericInput("mortgageInterest","Mortage interest (%)",min=0,max=Inf,value=2),
            h1("Rees related to buying:"),
            checkboxInput("firstTimeBuyer","First time buyer?",value=TRUE),
            numericInput("solicitorsFees","Solicitor's fees:",min = 1,max = Inf,value = 1000),
            numericInput("valuationFee","Valuation fee:",min=0,max=Inf,value=750),
            numericInput("SurveyorsFee","Surveyor's fee:",min=0,max=Inf,value=600),
            h1("Market fees:"),
            numericInput("averageHousePriceIncrease","Annual house price increase (%)",min=-Inf,max=Inf,value=5),
            numericInput("stockMarketInterest","Stock market interest",min=-Inf,max=Inf,value=4),
            h1("Selling fees:"),
            numericInput("estateAgent","Estate agent fee (%)",min=0,max=100,value=2),
            h1("Maintenance fees:"),
            numericInput("newRepairs","New repairs:",min=0,max=Inf,value=5750),
            numericInput("maintenance","Maintenance per year (% of house price):",min=0,max=10,value=1),
            numericInput("homeOwnersInsurance","Building + contents insurance:",min=0,max=Inf,value=140),
            h1("How many years to simulate?"),
            numericInput("nyears","Number of years:",min=1,max=Inf,value=10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("housePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$housePlot <- renderPlot({
        
        # Calculate the costs each year
        df = data.frame(Year = seq_len(input$nyears+1)-1)
        
        # Calculate rent: cost = amount paid, benefit = interest from stock market
        rent = input$expectedRent * 12 * df$Year
        stock_market = input$deposit * (1 + input$stockMarketInterest/100)^df$Year
        df$rent = stock_market - rent
        
        # Work out how much stamp duty
        stamp_duty = 0
        if(input$firstTimeBuyer & input$housePrice < 500000){
            if(input$housePrice > 300000) stamp_duty = 0.05 * (input$housePrice - 300000)
        } else {
            if(input$housePrice > 125000){
                amount = ifelse(input$housePrice < 250000,input$housePrice - 125000,250000-125000)
                stamp_duty = stamp_duty + 0.02 * amount
            }
            if(input$housePrice > 250000){
                amount = ifelse(input$housePrice < 925000,input$housePrice - 250000,925000-250000)
                stamp_duty = stamp_duty + 0.05 * amount
            }
            if(input$housePrice > 925000){
                amount = ifelse(input$housePrice < 1500000,input$housePrice - 925000,1500000-925000)
                stamp_duty = stamp_duty + 0.1 * amount
            }
            if(input$housePrice > 1500000){
                amount = input$housePrice - 1500000
                stamp_duty = stamp_duty + 0.12 * amount
            }
        }
        
        # Calculate amount based on house
        df$buying = NA
        # Take out immediate one-off costs
        df$buying[1] = -(input$solicitorsFees + input$valuationFee + input$SurveyorsFee + input$newRepairs + stamp_duty + input$deposit)
        
        # Work out total amount of mortgage to be paid off
        total_mortgage = input$mortgageAmount * ((1 + input$mortgageInterest/100)^input$mortgageTerm)
        
        # Work out monthly repayments - this is an income
        monthly_repayments = total_mortgage / input$mortgageTerm / 12
        
        # This becomes an income - you're paying yourself
        for(i in 2:nrow(df)) df$buying[i] = df$buying[i-1] + monthly_repayments * 12
        
        # Estimate how much you could sell the house for
        selling_price = input$housePrice * (1 + input$averageHousePriceIncrease/100)^df$Year
        # Estate agent's feeds
        estate_agent_fee = selling_price * input$estateAgent/100
        
        df$buying = df$buying - estate_agent_fee
        
        # Calculate maintenance costs
        maintenance = selling_price * input$maintenance / 100
        # Exclude maintenance costs
        df$buying = df$buying - maintenance
        
        plot(df$rent,x=df$Year,type="o",xlab="Year",ylab="Balance",ylim=range(df[,-1]))
        lines(df$buying,x=df$Year,type="o",lty=2)
        legend("topleft",lty=c(1,2),c("Renting","Buying"))
        abline(h=0)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
