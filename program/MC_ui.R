library(shiny)
library(grid)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

ui<-navbarPage(
	title = "Auswertung Macrocycle von Lea",
	tabPanel(
	"PCA on PC1 and PC2",
pageWithSidebar(
	headerPanel("Select Data"),
	sidebarPanel(
		#h2("Data Preparation:"),
		h2("Data to Display:"),
		
		checkboxInput("all_data",label=c("All Data?"),value=FALSE),
		actionButton("go_1","Go_1"),actionButton("go_2","Go_2"),actionButton("go_3","Go_3"),actionButton("go_4","Go_4"),actionButton("go_5","Go_5"),actionButton("go_6","Go_6"),actionButton("go_7","Go_7"),
		checkboxGroupInput("structure", label=h4("Choose Structures:"),choices=list("all"=c("all_structures"),"1(cpd 7)"=c("1"),"5(cpd 1)"=c("5"),"15(cpd 2)"=c("15"),"22(cpd 5)"=c("25"),"24(cpd 6)"=c("24"),"26(cpd 3)"=c("26"),"28(cpd 4)"=c("28")),selected=c(""),inline=TRUE),
		checkboxGroupInput("process_type",label=h4("Process:"), 
		choices =list("all"=c("all_process"),"MD at 300K"=c("MD"),"MD at 400K"=c("MD400K"), 
		"MD at 500K"=c("MD500K"),"long MD"=c("longMD"),"SimAn_TopConf"=c("SA"),
		"SimAn_divStruc"=c("revSA"),"BEST"=c("BEST"), "Minimised MD" = c("miniMD"),"Minimised Prime" = c("miniPrime"),"Minimised Conformator" = c("miniConf"),"post-selected low Energy Conformers (only cpd 22)" =c("lowE_MD"), "post-selected low Energy Conformers (only cpd 22) MINI" =c("mini_lowE_MD")) ,selected=c(""),inline=TRUE),
		checkboxGroupInput("ChargeStatus",label=h4("ChargeStatus"),choices=list("all"=c("all_charged"),"neutral"=c("neutral"),"charged"=c("charged")),selected=c(""),inline=TRUE),
checkboxGroupInput("Start_structure",label=h4("Start_structure:"),choices=list("all"=c("all_start_structure"),"1"=c("1"),"2"=c("2"),"3"=c("3"),"4"=c("4"),"5"=c("5"),"6"=c("6"),"7"=c("7"),"8"=c("8"),"9"=c("9"),"10"=c("10"),"TopConf"=c("TopConf")),selected=c("all"),inline=TRUE),
checkboxGroupInput("Solvent",label=h4("Solvent:"),choices=list("all"=c("all_solvents"),"water"=c("Water"),"CHCL3"=c("CHCL3"),"DMSO"=c("DMSO")),selected=c("all"),inline=TRUE),
checkboxGroupInput("Temperature", label=h4("Simulation   Temperature") ,choices=list("all"=c("all_temp"), "300K"=c("300"),"400K"=c("400"), "500K"=c("500")),inline=TRUE),
checkboxGroupInput("energy_measure", label=h6("Measured in:"), choices=list("kj/mol"=c("kj"),"kcal/mol"=c("kcal")),inline=TRUE,selected=c("")),
uiOutput("slider_energy_kj"),
uiOutput("slider_energy_kcal"),

		
#sliderInput(inputId="Energy",label="Energy",min=-100, max=450, value=450),
helpText("Note: only points with an energy <= chosen Energy",
	"will be displayed. energy_lowpoint is always ",
	"the smallest energy in sepcified subset"),
checkboxGroupInput("color_switch_energy",label=h6("Color Switch for binned Energy:"),choices=list("normal"=c("1"),"reverse"=c("-1")),inline=TRUE),
selectInput("Color","Color by:", c("ChargeStatus:"="charge_status", "Start_structure:"="start_structure_number", "Solvent:"="solvent", "Energy binned:"="Energy_Unnamed","Energy continious:"="Energy_Unnamed_c","Process:"="process","Structure_derieved:"="structure_derieved","Temperature"="Temperature", "Total Hbonds" ="Total_HBonds","PolarSurfaceArea"="PSA","Phasen der SA:"="Phase")),
checkboxInput("saves_1","Save Plot 1"),
checkboxInput("saves_2","Save Plot 2"),
checkboxInput("saves_3","Save Plot 3"),
checkboxInput("saves_4","Save Plot 4"),
checkboxInput("saves_5","Save Plot 5"),
checkboxInput("saves_6","Save Plot 6"),
checkboxInput("saves_7","Save Plot 7"),
radioButtons("radio",label="Format",choices=list("Powerpoint"="powerpoint","Paper_2"="paper_2", "Paper_3"="paper_3"),inline=TRUE)
),
	
	mainPanel(
		column(6,plotOutput(outputId="PCA_plot_1")),
		column(6,plotOutput(outputId="PCA_plot_2")),
		column(6,plotOutput(outputId="PCA_plot_3")),
		column(6,plotOutput(outputId="PCA_plot_4")),
		column(6,plotOutput(outputId="PCA_plot_5")),
		column(6,plotOutput(outputId="PCA_plot_6")),
		column(6,plotOutput(outputId="PCA_plot_7"))
		
		#verbatimTextOutput("summary")
		#h4("Differential PCA"),
		#plotOutput(outputId="diff_PCA_plot")
		)
)#page with sidebar zuende
),#tabpanel zuende
tabPanel("Density Maps",
	sidebarPanel(
	uiOutput("slider1"),
	uiOutput("slider2"),
	uiOutput("slider3"),
	uiOutput("slider4"),
	uiOutput("slider5"),
	uiOutput("slider6"),
	uiOutput("slider7"),
	#sliderInput(inputId="bin_size",label="Bin size",min=-100, max=450, value=300),
	helpText("Note: change number to set the binsize to calculate density."),
	actionButton("go_density","Go"),
	checkboxInput("density_saves_1","Save Plot 1"),
	checkboxInput("density_saves_2","Save Plot 2"),
	checkboxInput("density_saves_3","Save Plot 3"),
	checkboxInput("density_saves_4","Save Plot 4"),
	checkboxInput("density_saves_5","Save Plot 5"),
	checkboxInput("density_saves_6","Save Plot 6"),
	checkboxInput("density_saves_7","Save Plot 7")
	),
	mainPanel(
		column(6,plotOutput(outputId="PCA_density_1")),
		column(6,plotOutput(outputId="PCA_density_2")),
		column(6,plotOutput(outputId="PCA_density_3")),
		column(6,plotOutput(outputId="PCA_density_4")),
		column(6,plotOutput(outputId="PCA_density_5")),
		column(6,plotOutput(outputId="PCA_density_6")),
		column(6,plotOutput(outputId="PCA_density_7"))
		
	)
), #tabPanel zuende
tabPanel("lightPCA",
sidebarPanel(sliderInput(inputId="cut_size",label="Cut size",min=0, max=100, value=25),
	sliderInput(inputId="threshold",label="Max number of points in single box allowed", min=0,  max=1000, value=100),
	sliderInput(inputId="jitter_factor",label="variable to adjust the degree of noise to points",min=0, max=10, value=1),
	actionButton("go_update","GO"),
	checkboxInput("light_saves_1","Save Plot 1"),
	checkboxInput("light_saves_2","Save Plot 2"),
	checkboxInput("light_saves_3","Save Plot 3"),
	checkboxInput("light_saves_4","Save Plot 4"),
	checkboxInput("light_saves_5","Save Plot 5"),
	checkboxInput("light_saves_6","Save Plot 6"),
	checkboxInput("light_saves_7","Save Plot 7")
),
mainPanel(
	column(6,plotOutput(outputId="PCA_light_1")) ,
	column(6,plotOutput(outputId="PCA_light_2")),
	column(6,plotOutput(outputId="PCA_light_3")),
	column(6,plotOutput(outputId="PCA_light_4")),
	column(6,plotOutput(outputId="PCA_light_5")),
	column(6,plotOutput(outputId="PCA_light_6")),
	column(6,plotOutput(outputId="PCA_light_7"))
)

), #tabpanel zu ende
tabPanel("Loadings",
         sidebarPanel(
           sliderInput(inputId="PC_x",label="PC on x-Axis",min=1, max=32, value=1),
           sliderInput(inputId="PC_y",label="PC on y-Axis",min=1, max=32, value=2)
         ),
	basicPage(plotOutput(outputId="Loadings",height=800),
	checkboxInput("save_loadings","Save"))
),#tabPanel zuende
tabPanel("EnergyDiagram",
sidebarPanel(
	actionButton("go_Energy_1","Go_1"),
	actionButton("go_Energy_2","Go_2"),
	actionButton("go_Energy_3","Go_3"),
	actionButton("go_Energy_4","Go_4"),
	actionButton("go_Energy_5","Go_5"),
	actionButton("go_Energy_6","Go_6"),
	actionButton("go_Energy_7","Go_7"),
	checkboxGroupInput("structure_energy", label=h4("Choose Structures:"), choices= list("all"=c("all_structures") , "1"=c("1"), "5"=c("5"), "15"=c("15"), "22"=c("25"), "24"=c("24"), "26"=c("26"), "28"=c("28")),selected=c(""),inline=TRUE),
		checkboxGroupInput("ChargeStatus_energy",label=h4("ChargeStatus"),choices=list("all"=c("all_charged"),"neutral"=c("neutral"),"charged"=c("charged")),selected=c(""),inline=TRUE),

checkboxGroupInput("Solvent_energy",label=h4("Solvent:"),choices=list("all"=c("all_solvents"),"water"=c("Water"),"CHCL3"=c("CHCL3")),selected=c("all"),inline=TRUE),
	uiOutput("slider_energy"),
	uiOutput("slider_bin_energy"),
	radioButtons("radio_energy",label="Format", choices=list( "Powerpoint" ="powerpoint","Paper_2"="paper_2","Paper_3"="paper_3"),  inline=TRUE),
	checkboxInput("energy_saves_1","Save Plot 1"),
	checkboxInput("energy_saves_2","Save Plot 2"),
	checkboxInput("energy_saves_3","Save Plot 3"),
	checkboxInput("energy_saves_4","Save Plot 4"),
	checkboxInput("energy_saves_5","Save Plot 5"),
	checkboxInput("energy_saves_6","Save Plot 6"),
	checkboxInput("energy_saves_7","Save Plot 7")
),
mainPanel(
	column(6,plotOutput(outputId="energyPlot_1")),
	column(6,plotOutput(outputId="energyPlot_2")),
	column(6,plotOutput(outputId="energyPlot_3")),
	column(6,plotOutput(outputId="energyPlot_4")),
	column(6,plotOutput(outputId="energyPlot_5")),
	column(6,plotOutput(outputId="energyPlot_6")),
	column(6,plotOutput(outputId="energyPlot_7"))
)

),#tabpanel zu ende
tabPanel("PSA Histogramme",
sidebarPanel(
checkboxInput("histoPSA_saves_1","Save Plot 1"),
	checkboxInput("histoPSA_saves_2","Save Plot 2"),
	checkboxInput("histoPSA_saves_3","Save Plot 3"),
	checkboxInput("histoPSA_saves_4","Save Plot 4"),
	checkboxInput("histoPSA_saves_5","Save Plot 5"),
	checkboxInput("histoPSA_saves_6","Save Plot 6"),
	checkboxInput("histoPSA_saves_7","Save Plot 7")
),
mainPanel(
	column(6,plotOutput(outputId="histoPSA_1")),
	column(6,plotOutput(outputId="histoPSA_2")),
	column(6,plotOutput(outputId="histoPSA_3")),
	column(6,plotOutput(outputId="histoPSA_4")),
	column(6,plotOutput(outputId="histoPSA_5")),
	column(6,plotOutput(outputId="histoPSA_6")),
	column(6,plotOutput(outputId="histoPSA_7"))
)
)#tabpanel zuende

)#navbar zuende
