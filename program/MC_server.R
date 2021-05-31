library(shiny)
library(grid)
library(RColorBrewer)
library(dplyr)
library(ggExtra)
library(ggplot2)
library(vegan)
library(ggpubr)

server<-function(input,output){
###############################################################################################################################
#Saving Function
set_panel_size <- function(p=NULL, file=NULL, output="powerpoint"){
  if("ggExtraPlot"%in%(class(p))){
    g=p
    margin = unit(10,"mm")
  }else{
    g=ggplotGrob(p)
    margin = unit(1,"mm")
    }
	##options
	if(output=="powerpoint"){
	width=unit(12, "cm")
	height=unit(12, "cm")
	}else if (output=="paper_2"){
	width=unit(6, "cm")
	height=unit(6, "cm")
	}else{
	width=unit(4, "cm")
	height=unit(4, "cm")
	}
  	panels <- grep("panel", g$layout$name)
  	panel_index_w<- unique(g$layout$l[panels])
  	panel_index_h<- unique(g$layout$t[panels])
  	nw <- length(panel_index_w)
  	nh <- length(panel_index_h)
   	g$widths[panel_index_w] <-  rep(width,  nw)
   	g$heights[panel_index_h] <- rep(height, nh)
	
	if(!is.null(file))
    		ggsave(file, g, dpi=600, device="png",
           	width = convertWidth(sum(g$widths) + margin, 
                                unitTo = "in", valueOnly = TRUE),
           	height = convertHeight(sum(g$heights) + margin,  
                                  unitTo = "in", valueOnly = TRUE))
	#print(p) #remove and you see plot even when saving
}

################################################################################################################################
OverlapDist<<-list()	
##Setup data and ranges
	PCA_results_all<-as.data.frame(PCA_results_all_data$x)
	PCA_results_all$Energy_kj=as.numeric(PCA_results_all$Energy_kj)
	PCA_results_all$Energy_kcal=as.numeric(PCA_results_all$Energy_kcal)
	global_min<-round(min(min(PCA_results_all$PC1),min(PCA_results_all$PC2)))
	global_max<-round(max(max(PCA_results_all$PC1),max(PCA_results_all$PC2)))
	x_lim<-c(global_min,global_max)
	y_lim<-c(global_min,global_max)
	PCA_results_all$Temperature<-gsub("K","",PCA_results_all$Temperature)
	a<-dim(PCA_results_all)
	print(paste0("orginial DataFrames dimensions: ",a))
	maxEnergy_kj=max(PCA_results_all$Energy_kj)
	maxEnergy_kcal=max(PCA_results_all$Energy_kcal)
	
	output$slider_energy_kj<-renderUI({sliderInput(inputId="energy_slider_kj",label="Energy range kj", min=0 , max=maxEnergy_kj+1, value=maxEnergy_kj,round=T)})
	output$slider_energy_kcal<-renderUI({sliderInput("energy_slider_kcal","Energy range kcal", min=0 , max=maxEnergy_kcal+1, value=maxEnergy_kcal)})
################################################################################################################################
	##put all data into right data type
PCA_results_all$Energy_Unnamed<-as.numeric(PCA_results_all$Energy_Unnamed) #wird mit relativer Energie Ersetzt
PCA_results_all$Energy_ABS<-as.numeric(PCA_results_all$Energy_Unnamed) #absolute Energie
PCA_results_all$structure_derieved<-as.character(PCA_results_all$structure_derieved)
PCA_results_all$solvent<-as.character(PCA_results_all$solvent)
PCA_results_all$charge_status<-as.character(PCA_results_all$charge_status)
PCA_results_all$start_structure_number<-as.character(PCA_results_all$start_structure_number)
PCA_results_all$Temperature<-as.numeric(PCA_results_all$Temperature)
#PCA_results_all$PSA<-cut(PCA_results_all$PSA,breaks=c(30,80,100,180),include.lowest=TRUE)
PCA_results_all$Total_HBonds<-as.character(PCA_results_all$Total_HBonds)
PCA_results_all$Phase<-as.factor(PCA_results_all$Phase)
reorder_df<-c("MD","MD400K","MD500K","longMD","revSA","SA", "miniMD", "ConfGen","BEST" ,"miniPrime","miniConf","lowE_MD","mini_lowE_MD","BEST_new","Prime_new","ConfGen_new")
PCA_results_all<-left_join(data.frame(process=reorder_df),PCA_results_all,by="process")
PCA_results_all$process<-as.character(PCA_results_all$process)
position_plot<<-0
###############################################TAB1 PCA PLOT####################################################################
#PCA plots
results<-"test"  
#direction_color<-as.numeric(input$color_switch_energy)
##Which plot position
#position<-reactive({
#list(input$go_1,input$go_2,input$go_3,input$go_4,input$go_5,input$go_6,input$go_7)
#})

button_clicked<-function(structure=input$structure,process=input$process_type, charge=input$ChargeStatus ,start=input$Start_structure, solvent=input$Solvent ,temperature=input$Temperature, energy_kj=input$energy_slider_kj, energy_kcal=input$energy_slider_kcal, color=input$Color, energy_measure=input$energy_measure,plot_only=TRUE,Paper_Plotting=input$Paper_Plotting,MarginalHist=input$MarginalHist){

##get user input
		a<-structure
		b<-process
		c<-charge
		d<-start
		e<-solvent
		f<-temperature
		g<-as.numeric(energy_kj)
		print(g)
##specific grey colors for energy
	direction_color<-as.numeric(input$color_switch_energy)
	#grey_colors<-c("#2b2b2b","#707070","#b2b2b2")
	grey_colors<-viridis::viridis(3)#called grey colors due to historic reasons
	if(direction_color==1){grey_colors=rev(grey_colors)}

	
##subset settings
	options<-c(a,b,c,d,e,f,g)
	if("all_structures" %in% a){a<-c("1","5","15","25","24","26","28")}
	if("all_process" %in% b){b<-c("MD","MD400K","MD500K","longMD","revSA","SA","ConfGen","miniMD","BEST","miniPrime","miniConf","lowE_MD","mini_lowE_MD","BEST_new","Prime_new","ConfGen_new")}
	if("all_charged" %in% c){c<-c("neutral","charged")}
	if("all_start_structure" %in% d){d<-c("1","2","3","4","5","6","7","8","9","10","TopConf","one","neutral","charged")} ##erstmal unwichtig aber falsch in matrix
	if("all_solvents" %in% e){e<-c("Water","DMSO","CHCL3")}
	if("all_temp" %in% f){f<-c("300","400","500","0")}
##select data based on user input
	if(input$all_data == TRUE){
		pass
		#selected_data=PCA_results_all # achtung hier
	}else{
		print("data will be subsetted")
		print(options)
		selected_data<-subset(PCA_results_all, subset= (structure_derieved==a[1] | structure_derieved==a[2] |structure_derieved==a[3] |structure_derieved==a[4] |structure_derieved==a[5] |structure_derieved==a[6] |structure_derieved==a[7])& (process==b[1]|process==b[2]|process==b[3]|process==b[4]|process==b[5]|process==b[6]|process==b[7]|process==b[8]|process==b[9]|process==b[10]|process==b[11]|process==b[12]|process==b[13]|process==b[14]|process==b[15]|process==b[16])&(charge_status==c[1]|charge_status==c[2])&
		(start_structure_number==d[1] |start_structure_number==d[2]|start_structure_number==d[3] |start_structure_number==d[4] |start_structure_number==d[5] |start_structure_number==d[6]|start_structure_number==d[7]|start_structure_number==d[8]|start_structure_number==d[9]|start_structure_number==d[10]|start_structure_number==d[11]|start_structure_number==d[12]|start_structure_number==d[13]|start_structure_number==d[14])&(solvent==e[1]|solvent==e[2]|solvent==e[3])&(Temperature==f[1]|Temperature==f[2]|Temperature==f[3]|Temperature==f[4])
	)}
	print(dim(selected_data))
##when only 2 way comparison different colors
	if(length(unique(selected_data[,color]))==2){palette=c("Set2"); grey_colors=viridis::viridis(length(unique(selected_data[,color])));grey_colors[grey_colors=="#FDE725FF"]="#e6b710" }else{palette=c("Spectral")}
	if(length(unique(selected_data[,color]))==1){palette=c("grey")}
	print(palette)
##adapt Energy based input based on slider
	title<-paste0(as.character(options),collapse="_")
	PCA_results<-selected_data
	color_user<-color
	print(color_user)
	kj_flag=FALSE
	kcal_flag=FALSE
	if(energy_measure == c("kj")){
		energy<-as.numeric(energy_kj)
		PCA_results<-subset(PCA_results,Energy_kj<=energy)
		PCA_results$Energy_Unnamed<-PCA_results$Energy_kj
		kj_flag=TRUE
	}else{
		energy<-as.numeric(energy_kcal)
		PCA_results<-subset(PCA_results,Energy_kcal<=energy)
		PCA_results$Energy_Unnamed<-PCA_results$Energy_kcal
		kcal_flag=TRUE
	}
	
## creating plot with special attention to Energy as color 
	if(color_user=="Energy_Unnamed"){
		if(kj_flag==TRUE){
			p<-ggplot(PCA_results,aes(x=PC1,y=PC2,color=PCA_results[,"Energy_kj_bin"]))+ scale_color_manual(values=grey_colors) + geom_point(size=0.8)+xlim(x_lim)+ ylim(y_lim)+ labs(color=energy_measure, title=title)+theme_bw()+theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
		}else{
		p<-ggplot(PCA_results,aes(x=PC1,y=PC2,color=PCA_results[,"Energy_kcal_bin"]))+ scale_color_manual(values=grey_colors)+ geom_point(size=0.8)+xlim(x_lim)+ ylim(y_lim)+ labs(color=energy_measure, title=title)+theme_bw()+theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
		}	
	}else{	
		if(color_user=="Energy_Unnamed_c"){color_user="Energy_Unnamed"}
	  if(length(unique(selected_data[,color_user]))==1){
	    p<-ggplot(PCA_results,aes(x=PC1,y=PC2,color=PCA_results[,color_user]))+scale_color_manual(values=grey_colors)+geom_point(size=0.8)+ xlim(x_lim)+ylim(y_lim)+ labs(color=color_user, title=title) +theme_bw()+ theme(aspect.ratio=1,legend.position="bottom", legend.text=element_text(size=8), axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
	  }else{
##handle color advising when comparison to miniMD (always same color)
		PCA_results$process<-factor(PCA_results$process, levels=c("MD","MD400K","MD500K","longMD","revSA","SA", "miniMD", "ConfGen","BEST" ,"miniPrime","miniConf","BEST_new","Prime_new","ConfGen_new"))
	p<-ggplot(PCA_results,aes(x=PC1,y=PC2,color=PCA_results[,color_user]))+scale_color_manual(values=grey_colors)+geom_point(size=0.8)+ xlim(x_lim)+ylim(y_lim)+ labs(color=color_user, title=title) +theme_bw()+ theme(aspect.ratio=1,legend.position="bottom", legend.text=element_text(size=8), axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
	  }
	}
	##New visual adjustments to plot 
	if(Paper_Plotting){
	  #Axis labels deleted
	  #translate cpdLabel
	  if(a=="1"){cpdLabel="7"}
	  if(a=="5"){cpdLabel="1"}
	  if(a=="15"){cpdLabel="2"}
	  if(a=="25"){cpdLabel="5"} # historic reasosn cpd 22 was mistaken with cpd 25 (only naming)
	  if(a=="24"){cpdLabel="6"}
	  if(a=="26"){cpdLabel="3"}
	  if(a=="28"){cpdLabel="4"}
	  #cpd Name and charge status as information to the plot
	  PlotLabelText=paste0("cpd: ",as.character(cpdLabel),", ",as.character(c))
	  PlotLabel <- grobTree(textGrob(PlotLabelText, x=0.95,  y=0.95, just=c(1,1),
	                                 gp=gpar(col="black", fontsize=8, fontface="italic")))
	  xAxisLabel <- grobTree(textGrob("PC1", x=0.95,  y=0.01, just=c(1,0),
	                                  gp=gpar(col="black", fontsize=8)))
	  yAxisLabel <- grobTree(textGrob("PC2", x=0.01,  y=0.80, just=c(0,1),rot=90,
	                                  gp=gpar(col="black", fontsize=8)))
	  
	  if(any(unique(PCA_results[,color_user])%in%c("ConfGen","BEST" ,"miniPrime","miniConf","BEST_new","Prime_new","ConfGen_new"))){
	    #create additional label and put in the middle top
	    print(unique(PCA_results[,color_user]))
	    labelText=unique(PCA_results[,color_user])[!grepl("MD",unique(PCA_results[,color_user]))]
	    if(labelText=="miniPrime"){labelText="PMM"}
	    if(labelText=="miniConf"){labelText="CONF"}
	    PanelLabel <- grobTree(textGrob(labelText, x=0.01,  y=0.01, just=c(0,0),
	                                    gp=gpar(col="black", fontsize=8)))
	    #Give p value of overlap
	    #make distance object
	    nrow(PCA_results)
	    print(paste0("A matrix with the dimensions: ",nrow(PCA_results), " x ",nrow(PCA_results)," is created TAKES TIME"))
	    OverlapDist[[title]]<<-PCA_results
	    
	  }else{
	    PanelLabel <- grobTree(textGrob("", x=0.01,  y=0.90, just=c(0,1),
	                                    gp=gpar(col="black", fontsize=8)))
	  }
  	p=p+ theme(aspect.ratio=1,legend.position="bottom", legend.text=element_text(size=8), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  	    annotation_custom(PlotLabel)+
  	    annotation_custom(xAxisLabel)+
  	    annotation_custom(yAxisLabel)+
  	    annotation_custom(PanelLabel)
	  
	}else{
	  p=p+ theme(aspect.ratio=1,legend.position="bottom", legend.text=element_text(size=8), axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
	}
	if(MarginalHist){
	  p=ggMarginal(p,type="density",groupColour = TRUE,groupFill=T)
	}
##return results
	if(plot_only==TRUE){
		return(p)
	}else{
		results<-list()
		results[["Plot"]]<-p
		results[["Data"]]<-PCA_results
		results[["Title"]]<-paste0(title," colored by",as.character(color))
		results[["color_user"]]<-color
		results[["energy_kj"]]<-energy_kj
		results[["energy_kcal"]]<-energy_kcal
		results[["energy_measure"]]<-energy_measure
		return(results)
	}
}


###creating plot based on button input
results_1<-eventReactive(input$go_1,{results<-button_clicked(plot_only=FALSE);return(results)})
###Plotting plot
output$PCA_plot_1<-renderPlot({	results=results_1()$Plot;filename<-paste0(results_1()$Title,".png")
				if(input$saves_1){
				  sendToSave<<-results
				  set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				test<<-results_1()
				return(results)
				}
})

###creating plot based on button input
results_2<-eventReactive(input$go_2,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_2<-renderPlot({	results=results_2()$Plot;filename<-paste0(results_2()$Title,".png")
				if(input$saves_2){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})


###creating plot based on button input
results_3<-eventReactive(input$go_3,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_3<-renderPlot({	results=results_3()$Plot;filename<-paste0(results_3()$Title,".png")
				if(input$saves_3){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
results_4<-eventReactive(input$go_4,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_4<-renderPlot({	results=results_4()$Plot;filename<-paste0(results_4()$Title,".png")
				if(input$saves_4){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
results_5<-eventReactive(input$go_5,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_5<-renderPlot({	results=results_5()$Plot;filename<-paste0(results_5()$Title,".png")
				if(input$saves_5){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
results_6<-eventReactive(input$go_6,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_6<-renderPlot({	results=results_6()$Plot;filename<-paste0(results_6()$Title,".png")
				if(input$saves_6){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})


###creating plot based on button input
results_7<-eventReactive(input$go_7,{
	results<-button_clicked(plot_only=FALSE)
	return(results)})
###Plotting plot
output$PCA_plot_7<-renderPlot({	results=results_7()$Plot;filename<-paste0(results_7()$Title,".png")
				if(input$saves_7){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})


#############################################TAB2 DENSITY#####################################################################
##slider fuer tab2; reactive
output$slider1<-renderUI({sliderInput("a","Slider_Plot1",min=1,max=100,value=50)}) #round(sqrt(nrow(results_1()$Data)/2)))})
output$slider2<-renderUI({sliderInput("b","Slider_Plot2",min=1,max=100,value=50)}) #round(sqrt(nrow(results_2()$Data)/2)))})
output$slider3<-renderUI({sliderInput("c","Slider_Plot3",min=1,max=100,value=50)}) #round(sqrt(nrow(results_3()$Data)/2)))})
output$slider4<-renderUI({sliderInput("d","Slider_Plot4",min=1,max=100,value=50)}) #round(sqrt(nrow(results_4()$Data)/2)))})
output$slider5<-renderUI({sliderInput("e","Slider_Plot5",min=1,max=100,value=50)}) #round(sqrt(nrow(results_5()$Data)/2)))})
output$slider6<-renderUI({sliderInput("f","Slider_Plot6",min=1,max=100,value=50)}) #round(sqrt(nrow(results_6()$Data)/2)))})
output$slider7<-renderUI({sliderInput("g","Slider_Plot7",min=1,max=100,value=50)}) #round(sqrt(nrow(results_7()$Data)/2)))})

##Density creation of plot
PCA_density<-function(PCA_input,process=input$process_type,slider_input,structure=input$structure,splitPerCondition=input$splitPerCondition,DotsOnTop=input$DotsOnTop,MarginalHist=input$MarginalHist2){
##setting up values from slider and input
	print("Let do density")
  
  results<-PCA_input;color_user<-results$color_user;PCA_results<-results$Data;title<-results$Title;bin_size<-as.numeric(slider_input)
  print(color_user)
  
  if(color_user=="Energy_Unnamed"){
		  
			if(input$energy_measure==c("kj")) {
			  color_user="Energy_kj_bin"
			}else{
			  color_user="Energy_kcal_bin"
			}
		  grey_colors<-viridis::plasma(4)[1:3]#called grey colors due to historic reasons
		  grey_colors[grey_colors=="#ED7953FF"]="#0754fa"#replace the lighter blue with a more distinct blue
		  grey_colors[grey_colors=="#0D0887FF"]="#e6b710" # replace to bright yellow
		  grey_colors=rev(grey_colors)
		}else{
		  print(length(unique(PCA_input$Plot$data[color_user,])))
		  if((length(unique(PCA_input$Plot$data[color_user,])))!=0){
		    print("Number before 0 ? ISSUE")
		    grey_colors<-viridis::viridis(nrow(unique(renamedCol[,"color_user"])))#called grey colors due to historic reasons
		    grey_colors[grey_colors=="#FDE725FF"]="#e6b710" # replace to bright yellow
		  }else{
		    grey_colors<-viridis::viridis(2)#called grey colors due to historic reasons
		    grey_colors[grey_colors=="#FDE725FF"]="#e6b710" # replace to bright yellow
		  }
		  
		}
    print(color_user)
    print(grey_colors)
    print(nrow(unique(PCA_input$Plot$data[color_user])))
		which_process<-unique(PCA_results$process)
		print(which_process)
		renamedCol=PCA_results
		colnames(renamedCol)[grepl(color_user,colnames(renamedCol))]="color_user"
		print(colnames(renamedCol))
		if("miniConf" %in% which_process |"miniPrime"%in% which_process|"BEST"%in% which_process| "all_process" %in% which_process){
  		if("miniConf" %in% which_process){process_game="miniConf"}
  		if("miniPrime"%in% which_process){process_game="miniPrime"}
  		if("BEST"%in% which_process){process_game="BEST"}
  		print("Attention! Works only if one of BEST/miniConf/miniPRIME is selected")
  		print("small Sample is in the game- will be plotted on top")
  ##creating plots dealing with ConfGen data as there are too few
  		print("BEST/miniConf/miniPRIME data wird nicht in Dichte angezeigt! Diese Datapoints draufgemappet")
  		if(color_user=="process"){
  		 print(process_game)
  	   testDensity<<-renamedCol
  	   renamedCol=testDensity
  	   
  	   if(MarginalHist){
  	     #First do both dots, get marginal, plot finally density with marignal
  	     p<-ggplot(renamedCol,aes(x=PC1,y=PC2,color=color_user))+scale_color_manual(values=grey_colors)+geom_point(size=0.8)+ xlim(x_lim)+ylim(y_lim)+ labs(color=color_user, title=title)
  	     #p=ggMarginal(p,type="density",groupColour = TRUE,groupFill = T)
  	     #p+theme_bw()
  	     p<-ggplot(renamedCol,aes(x=PC1,y=PC2,fill=color_user))+
  	       stat_density2d(data=subset(renamedCol, color_user != process_game),mapping=aes(color=as.factor(subset(renamedCol,color_user !=  process_game, select=color_user)[,1])),geom="polygon",bins=bin_size,size=0.2,alpha=0.1) +
  	       geom_point(data=subset(renamedCol,subset= color_user == process_game),aes(color=as.factor(subset(renamedCol,subset= color_user == process_game,select=color_user)[,1])),size=0.5)+
  	       scale_color_manual(values=grey_colors)+scale_fill_manual(values=grey_colors)+xlim(x_lim)+ylim(y_lim)+theme_bw()+
  	       theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
  	     p=ggMarginal(p,renamedCol,x=renamedCol$PC1,y=renamedCol$PC2,type="density",groupColour = TRUE,groupFill = T)
  	   }else{
  	   
  		  p<-ggplot(renamedCol,aes(x=PC1,y=PC2,fill=color_user))+
  		    stat_density2d(data=subset(renamedCol, color_user != process_game),mapping=aes(color=as.factor(subset(renamedCol,color_user !=  process_game, select=color_user)[,1])),geom="polygon",bins=bin_size,size=0.2,alpha=0.1) +
  		    geom_point(data=subset(renamedCol,subset= color_user == process_game),aes(color=as.factor(subset(renamedCol,subset= color_user == process_game,select=color_user)[,1])),size=0.5)+
  		    scale_color_manual(values=grey_colors)+scale_fill_manual(values=grey_colors)+xlim(x_lim)+ylim(y_lim)+theme_bw()+
  		    theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
  	    }
  		 }else{
  		  print("B")
    		p<-ggplot(renamedCol,aes(x=PC1,y=PC2))+
    		  stat_density2d(data=subset(renamedCol, process != process_game),mapping=aes(color=as.factor(subset(renamedCol,process !=  process_game, select=color_user)[,1])),geom="polygon",bins=bin_size,size=0.2,alpha=0.1) +
    		  geom_point(data=subset(renamedCol,subset= process == process_game),aes(color=as.factor(subset(renamedCol,subset= process == process_game,select=color_user)[,1])))+
    		  scale_color_manual(values=grey_colors)+scale_fill_manual(values=grey_colors)+xlim(x_lim)+ylim(y_lim)+theme_bw()+
    		  theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
  		}
		}else{	
		  print("Do we reach here?")
		  testData<<-renamedCol
		p<-ggplot(renamedCol,aes(x=PC1,y=PC2,color=color_user,fill=color_user))+
		  stat_density2d(geom="polygon",bins=bin_size,size=0.2,alpha=0.1)+
		  scale_color_manual(values=grey_colors)+scale_fill_manual(values=grey_colors)+xlim(x_lim)+ylim(y_lim)+theme_bw()+
		  theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
		
		if("lowE_MD" %in% which_process |"lowE_MD" %in%  process){
		  #plot "original 5 starting conformers as grey density contours in the back 
		  print("Do we reach here?")
		  #adjust names of start conformers for plotting reasons
		  renamedCol$process=as.character(renamedCol$process)
		  str(renamedCol)
		  renamedCol[which(renamedCol$process!="lowE_MD"),"color_user"]="old"
		  grey_colors=c(grey_colors[1:3],"grey")
		  p<-ggplot(renamedCol,aes(x=PC1,y=PC2,color=color_user,fill=color_user))+
		    stat_density2d(geom="polygon",bins=bin_size,size=0.2,alpha=0.1)+
		    scale_color_manual(values=grey_colors)+scale_fill_manual(values=grey_colors)+xlim(x_lim)+ylim(y_lim)+theme_bw()+
		    theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
		  
		}
		}
		
  #Axis labels deleted
  #cpd Name and charge status as information to the plot
	#PlotLabelText=paste0("cpd: ",as.character(a),", ",as.character(c))
	
  cpdLabel=unique(PCA_input$Plot$data$structure_derieved)
  print(cpdLabel)
  if(length(cpdLabel)==0){
    cpdLabel=unique(renamedCol$structure_derieved)
  }
  # Tranlsate cpd Label to paper names
  if(cpdLabel=="1"){cpdLabel="7"}
  if(cpdLabel=="5"){cpdLabel="1"}
  if(cpdLabel=="15"){cpdLabel="2"}
  if(cpdLabel=="25"){cpdLabel="5"}
  if(cpdLabel=="24"){cpdLabel="6"}
  if(cpdLabel=="26"){cpdLabel="3"}
  if(cpdLabel=="28"){cpdLabel="4"}
  
  chargeLabel=unique(renamedCol$charge_status)

  print(chargeLabel)
	PlotLabelText=paste0("cpd: ",cpdLabel,", ",chargeLabel)
	PlotLabel <- grobTree(textGrob(PlotLabelText, x=0.95,  y=0.95, just=c(1,1),
		                                 gp=gpar(col="black", fontsize=8, fontface="italic")))
	xAxisLabel <- grobTree(textGrob("PC1", x=0.95,  y=0.01, just=c(1,0),
		                                  gp=gpar(col="black", fontsize=8)))
	yAxisLabel <- grobTree(textGrob("PC2", x=0.01,  y=0.80, just=c(0,1),rot=90,
		                                  gp=gpar(col="black", fontsize=8)))
	if(any(which_process%in%c("ConfGen","BEST" ,"miniPrime","miniConf","BEST_new","Prime_new","ConfGen_new"))){
	  #create additional label and put in the middle top
	  print(unique(PCA_results$Plot$data$process))
	  labelText=unique(unique(which_process))[!grepl("MD",unique(which_process))]
	  PanelLabel <- grobTree(textGrob(labelText, x=0.05,  y=0.1, just=c(0,0),
	                                  gp=gpar(col="black", fontsize=8)))
	}else{
	  PanelLabel <- grobTree(textGrob("", x=0.01,  y=0.01, just=c(0,1),
	                                  gp=gpar(col="black", fontsize=8)))
	}
	p=p+ theme(aspect.ratio=1,legend.position="bottom", legend.text=element_text(size=8), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
		    annotation_custom(PlotLabel)+
		    annotation_custom(xAxisLabel)+
		    annotation_custom(yAxisLabel)+
	      annotation_custom(PanelLabel)
	
	if(DotsOnTop){
	  print("Dots will be plotted on Top")
	  p=p+
	    geom_point(size=0.00001)
	}
	
	
	if(splitPerCondition){
	  print("Contour Plots will be separated")
	  p=p+
	    facet_grid(cols = vars(color_user))
	}

	

		
	return(p)
		
print("DONE")
##return results
	#results<-list()
	#results[["Data"]]<-PCA_results
	#results[["Plot"]]<-p
	#results[["Title"]]<-title
	#return(results)		
}

###Plotting based on button input
###creating plot based on button input
density_1<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_1(),slider_input=input$a)
						return(density)})
###Plotting plot
output$PCA_density_1<-renderPlot({results=density_1();filename<-paste0("Density_",results_1()$Title,".png")
				if(input$density_saves_1){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
density_2<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_2(),slider_input=input$b)
return(density)})
###Plotting plot
output$PCA_density_2<-renderPlot({results=density_2();filename<-paste0("Density_",results_2()$Title,".png")
				if(input$density_saves_2){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
density_3<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_3(),slider_input=input$c)
return(density)})
###Plotting plot
output$PCA_density_3<-renderPlot({results=density_3()$Plot;filename<-paste0("Density_",results_3()$Title,".png")
				if(input$density_saves_3){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
density_4<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_4(),slider_input=input$d)
return(density)})
###Plotting plot
output$PCA_density_4<-renderPlot({results=density_4()$Plot;filename<-paste0("Density_",results_4()$Title,".png")
				if(input$density_saves_4){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
density_5<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_5(),slider_input=input$e)
return(density)})
###Plotting plot
output$PCA_density_5<-renderPlot({results=density_5()$Plot;filename<-paste0("Density_",results_5()$Title,".png")
				if(input$density_saves_5){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
density_6<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_6(),slider_input=input$f)
return(density)})
###Plotting plot
output$PCA_density_6<-renderPlot({results=density_6()$Plot;filename<-paste0("Density_",results_6()$Title,".png")
				if(input$density_saves_6){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
density_7<-eventReactive(input$go_density,{density<-PCA_density(PCA_input=results_7(),slider_input=input$g)
return(density)})
###Plotting plot
output$PCA_density_7<-renderPlot({results=density_7()$Plot;filename<-paste0("Density_",results_7()$Title,".png")
				if(input$density_saves_7){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

####colorblind color palette to avoid misleading colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#5e2645","#8e878b","#398944","#ba7e10")

####################################################Light PCA##################################################################

Light_PCA_go<-function(PCA_input,threshold=input$threshold,jitter_factor=input$jitter_factor,cut_size=input$cut_size){
##setting up variables
	results<-PCA_input; color_user<-results$color_user; PCA_results<-results$Data; title<-results$Title; energy_kj<-results$energy_kj; energy_kcal<-results$energy_kcal; energy_measure<-results$energy_measure;threshold<-threshold;jitter_factor<-jitter_factor;cut_size<-cut_size;
	kj_flag=FALSE
	kcal_flag=FALSE
	if(energy_measure == c("kj")){
		energy<-as.numeric(energy_kj)
		PCA_results<-subset(PCA_results,Energy_kj<=energy)
		PCA_results$Energy_Unnamed<-PCA_results$Energy_kj_bin
		kj_flag=TRUE
	}else{
		energy<-as.numeric(energy_kcal)
		PCA_results<-subset(PCA_results,Energy_kcal<=energy)
		PCA_results$Energy_Unnamed<-PCA_results$Energy_kcal_bin
		kcal_flag=TRUE
	}	
	if(color_user=="Energy_Unnamed_c"){color_user="Energy_Unnamed"}
	
	PCA_to_hm<<-PCA_results
	PCA_to_hm$process<-as.character(PCA_to_hm$process)
	print(str(PCA_to_hm))
##advising each data point two intervals
	cut_size=as.numeric(cut_size) #schieberegler
	PCA_to_hm$PC1_interval<-as.character(cut(PCA_to_hm$PC1,breaks=cut_size))
	PCA_to_hm$PC2_interval<-as.character(cut(PCA_to_hm$PC2,breaks=cut_size))
	PCA_to_hm$merged<-as.character(paste0(PCA_to_hm$PC1_interval,PCA_to_hm$PC2_interval))
	PC1_interval_var<-unique(PCA_to_hm$PC1_interval)
	PC2_interval_var<-unique(PCA_to_hm$PC2_interval)
##make an interval count table
	freq_table<-data.frame(PC1_interval=levels(cut(PCA_to_hm$PC1,breaks=cut_size)), PC2_interval=levels(cut(PCA_to_hm$PC2, breaks=cut_size)))
	freq_table<-expand.grid(freq_table)
	freq_table$Var1<-paste0(freq_table$PC1_interval,freq_table$PC2_interval)
	results_table<-as.data.frame(table(paste0(PCA_to_hm$PC1_interval,PCA_to_hm$PC2_interval)))
	list_of_data<-list()
	data_all<-merge(results_table,freq_table,all=TRUE,sort=FALSE,by="Var1")
	data_all[is.na(data_all$Freq),"Freq"]<-rep(0,length(data_all[is.na(data_all$Freq),"Freq"]))
	data_all$PC1_interval<-factor(data_all$PC1_interval, levels=levels(cut(PCA_to_hm$PC1,breaks=cut_size)))
	data_all$PC2_interval<-factor(data_all$PC2_interval, levels=levels(cut(PCA_to_hm$PC2,breaks=cut_size)))	
	data_all$ID<-c(1:nrow(data_all))
##Threshold for max number in one bin (to be adjustable)
	threshold<-as.numeric(threshold) #schieberegler
##find bins that have more than threshold points
	bins_to_full<-subset(data_all, subset= Freq >= threshold)
##subset PCA_to_hm to adjust its size to be plotted as it is part of overpopulated bin
	x<-paste(as.character(bins_to_full$Var1),collapse="|")
	x<-gsub("\\(","_",x)
	x<-gsub("\\]","_",x)
	PCA_to_hm$merged<-gsub("\\(","_",PCA_to_hm$merged)
	PCA_to_hm$merged<-gsub("\\]","_",PCA_to_hm$merged)
	to_full_data<-PCA_to_hm[grep(x,PCA_to_hm$merged),]
##adapt size based on binnes data points
	PCA_to_hm$size<-paste0("<",threshold)
	if(!("ID" %in% colnames(PCA_to_hm))){PCA_to_hm$ID<-"originaldata"}
	for(bin in unique(to_full_data$merged)){
		
	##sepearta bins from each other
	tmp<-PCA_to_hm[grep(bin,PCA_to_hm$merged),]    
	if(nrow(tmp)==0){
		print(bin)
		}else{
	##evaluate which conditions (wonach wird gefärbt??) are present and which show the same informtion
	categories<-table(tmp[,color_user])
	categories<-as.data.frame(categories)
	##give each cateogrie based on its frequency in the bin a bigger dot 
		categories$size<-paste0("<",threshold)
		categories[categories$Freq<threshold,3]<-paste0("<",threshold)
		categories[categories$Freq<threshold*2 & categories$Freq>threshold,3]<-paste0(">",threshold," <",2*threshold)
		categories[categories$Freq>threshold*2 ,3]<-paste0(">",2*threshold)
	##generate the coordinates for the new points -all the same- ( middle of the bin , later geom_jitter used)
		x_coord<-strsplit(bin, "_")[[1]][2]
		x_coord<-strsplit(x_coord,",")[[1]]
		x_coord<-(as.numeric(x_coord[1])+as.numeric(x_coord[2]))/2
		y_coord<-strsplit(bin, "_")[[1]][4]
		y_coord<-strsplit(y_coord,",")[[1]]
		y_coord<-(as.numeric(y_coord[1])+as.numeric(y_coord[2]))/2
	##Wie viele neue Punkte (jetzt binned große Punte)
		no_of_points<-length(unique(as.character(categories$Var1)))
		new_points<-data.frame(matrix(ncol=ncol(PCA_to_hm), nrow=no_of_points,NA))
		colnames(new_points)<-colnames(PCA_to_hm)
	##set up characterisitcs of these new points
		for(new_row in 1:no_of_points){
			new_points[new_row,color_user]<-as.character(categories$Var1)[new_row]
			new_points$PC1[new_row]<-x_coord
			new_points$PC2[new_row]<-y_coord
			new_points$size[new_row]<-as.character(categories$size)[new_row]
			new_points$ID[new_row]<-c("lightPCA")
		}
	##delete points which are noe represented by binned point and add binned points
		PCA_to_hm<-PCA_to_hm[-(grep(bin,PCA_to_hm$merged)),]
		PCA_to_hm<-rbind(PCA_to_hm,new_points)
	}
	}	
#slightly move newpoints using jitter adding noise
	x<-as.numeric(jitter_factor)
	PCA_to_hm[PCA_to_hm$ID == "lightPCA",]$PC1<-jitter(subset(PCA_to_hm, subset=ID == c("lightPCA"))$PC1,factor=x)
	PCA_to_hm[PCA_to_hm$ID == "lightPCA",]$PC2<-jitter(subset(PCA_to_hm, subset=ID == c("lightPCA"))$PC2,factor=x)
####change plotting order biggest dots first
	PCA_to_hm<-PCA_to_hm[rev(order(PCA_to_hm$size)),]
	print("ordering...")
##customize tick labels based on bin/cut size
	new_x_ticks<-gsub("\\(","",PC1_interval_var)
	new_x_ticks<-gsub("\\]","",new_x_ticks)
	new_x_ticks<-as.numeric(unique(unlist(strsplit(new_x_ticks,","))))
	new_x_ticks<-new_x_ticks[order(new_x_ticks)]
	new_x_ticks<-new_x_ticks[seq(1,length(new_x_ticks),2)]
	new_y_ticks<-gsub("\\(","",PC2_interval_var)
	new_y_ticks<-gsub("\\]","",new_y_ticks)
	new_y_ticks<-as.numeric(unique(unlist(strsplit(new_y_ticks,","))))
	new_y_ticks<-new_y_ticks[order(new_y_ticks)]
	new_y_ticks<-new_y_ticks[seq(1,length(new_y_ticks),2)]
	print("redoing ticks")
##setting up characteristic for plotting
	size1<-as.factor(subset(PCA_to_hm, subset= (ID == c("lightPCA")))$size)
	size1<-factor(size1,levels=c(paste0("<",threshold),paste0(">",threshold," <",2*threshold),paste0(">",2*threshold)))    
	size2<-subset(PCA_to_hm, subset= (ID != c("lightPCA")))$size
	size2<-factor(size2,levels=c(paste0("<",threshold),paste0(">",threshold," <",2*threshold),paste0(">",2*threshold)))    
	color1<-subset(PCA_to_hm, subset= (ID == c("lightPCA")))[,color_user]
	color2<-subset(PCA_to_hm, subset= (ID != c("lightPCA")))[,color_user]
##creating plot considering wheter binned points are there or not
	if(length(color1)==0 ){
		ligthPCA<- ggplot(PCA_to_hm,aes(x=as.numeric(PC1),y=as.numeric(PC2)))+ geom_point(data= subset(PCA_to_hm, subset = (ID != c("lightPCA"))), aes(size=size2,fill=color2),shape=1)+scale_x_continuous(name="PC1",breaks=new_x_ticks,limits=x_lim) +scale_y_continuous(name="PC2",breaks=new_y_ticks,limits=y_lim)+ labs(title=paste0(title," colored by", as.character(color_user) ))+ guides(fill=guide_legend(title=" single"))+theme_bw()+theme(aspect.ratio=1,legend.position="right",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+ scale_fill_manual(values=cbPalette)+ scale_color_manual(values=cbPalette)
	}else if(length(color1)!=0){
		ligthPCA<-ggplot(PCA_to_hm, aes(x=as.numeric(PC1),y=as.numeric(PC2)))+ geom_point(data=subset(PCA_to_hm, subset= (ID == c("lightPCA"))), aes(size=size1,fill=color1),shape=21,color="black",alpha=0.8)+ geom_point(data=subset(PCA_to_hm, subset = (ID != c("lightPCA"))), aes(size=size2,color=color2),shape=1) +scale_x_continuous(name="PC1",breaks=new_x_ticks,limits=x_lim) +scale_y_continuous(name="PC2",breaks=new_y_ticks,limits=y_lim)+ labs(title=paste0(title," colored by",as.character(color_user))) + guides(fill=guide_legend(title=" binned")) + guides(color=guide_legend(title=" single"))+theme_bw()+theme(aspect.ratio=1,legend.position="right",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+ scale_fill_manual(values=cbPalette)+scale_color_manual(values=cbPalette)
	}else{
		print("click GO")
	
	}
##return results
print("results")
	results<-list()
	#results[["Data"]]<-PCA_to_hm
	results[["Plot"]]<-ligthPCA
	results[["Title"]]<-paste0(title," colored by",as.character(color_user))
	return(results)
}

###Plotting based on button input
###creating plot based on button input
PCA_light_1<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_1());return(PCA_light)})
###Plotting plot
output$PCA_light_1<-renderPlot({results=PCA_light_1()$Plot;filename<-paste0("LightPCA_",PCA_light_1()$Title,".png")
				if(input$light_saves_1){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
###creating plot based on button input
PCA_light_2<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_2());return(PCA_light)})
###Plotting plot
output$PCA_light_2<-renderPlot({results=PCA_light_2()$Plot;filename<-paste0("LightPCA_",PCA_light_2()$Title,".png")
				if(input$light_saves_2){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
PCA_light_3<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_3());return(PCA_light)})
###Plotting plot
output$PCA_light_3<-renderPlot({results=PCA_light_3()$Plot;filename<-paste0("LightPCA_",PCA_light_3()$Title,".png")
				if(input$light_saves_3){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
PCA_light_4<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_4());return(PCA_light)})
###Plotting plot
output$PCA_light_4<-renderPlot({results=PCA_light_4()$Plot;filename<-paste0("LightPCA_",PCA_light_4()$Title,".png")
				if(input$light_saves_4){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
PCA_light_5<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_5());return(PCA_light)})
###Plotting plot
output$PCA_light_5<-renderPlot({results=PCA_light_5()$Plot;filename<-paste0("LightPCA_",PCA_light_5()$Title,".png")
				if(input$light_saves_5){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
PCA_light_6<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_6());return(PCA_light)})
###Plotting plot
output$PCA_light_6<-renderPlot({results=PCA_light_6()$Plot;filename<-paste0("LightPCA_",PCA_light_6()$Title,".png")
				if(input$light_saves_6){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

###creating plot based on button input
PCA_light_7<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_7());return(PCA_light)})
###Plotting plot
output$PCA_light_7<-renderPlot({results=PCA_light_7()$Plot;filename<-paste0("LightPCA_",PCA_light_7()$Title,".png")
				if(input$light_saves_7){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

########################################################################################################
##PCA LOADING MAP
###creating plot based on button input
#PCA_light_7<-eventReactive(input$go_update,{PCA_light<-Light_PCA_go(PCA_input=results_7());return(PCA_light)})
#Light_PCA_go<-function(PCA_input,threshold=input$threshold,jitter_factor=input$jitter_factor,cut_size=input$cut_size){
  
output$Loadings<-renderPlot({
	loadings<-as.data.frame(PCA_results_all_data$rotation)
	rownames(loadings)<-gsub("radTorsion_","",rownames(loadings))
	loadings$feature<-rownames(loadings)
	print(summary(PCA_results_all_data))
	x<-unlist(strsplit(loadings$feature,"_"))
	n<-length(x)
	loadings$belonging<-x[seq(1,n,2)]
	#print(input$PC_x)
	x_var=sym(paste0("PC",input$PC_x))
	y_var=sym(paste0("PC",input$PC_y))
	print(y_var)
	p<-ggplot(loadings,aes(x=!!x_var,y=!!y_var, label=feature, color=belonging))
	p<-p+theme_bw()+theme(aspect.ratio=1,legend.position="bottom",legend.text=element_text(size=8),axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+geom_text(size=4.5,fontface="bold")+xlim(-0.6,0.6)+ylim(-0.6,0.6)
	filename<-"Loadings.png"
	if(input$save_loadings){set_panel_size(file=filename,p=p,output=input$radio)
				}else{return(p)
				}

},height=800)

########################################################################################################
###Energy Histogramme
#energy cutoff
	output$slider_energy<-renderUI({sliderInput("energy_cutoff","Slider Energy Filter" ,min=0, max=100, value=20)}) # hier noch flexibles max?
#bin slider
	output$slider_bin_energy<-renderUI({sliderInput("bin_number","Slider Bin Number" ,min=1, max=20, value=10)})

##energy histogram Funktion
	energy_histogram<-function(structure=input$structure_energy,charge=input$ChargeStatus_energy , solvent=input$Solvent_energy,bin=input$bin_number,energy_filter=input$energy_cutoff){
print("start")
#specify varibles if "all" is selected
a<-structure
b<-charge
c<-solvent

##subset settings
	options<-c(structure,charge,solvent,energy_filter)
	if("all_structures" %in% structure){structure<-c("1","5","15","25","24","26","28")}
	if("all_charged" %in% charge){charge<-c("neutral","charged")}
	if("all_solvents" %in% solvent){solvent<-c("Water","CHCL3")}

#select data
print(dim(PCA_results_all))
print(options)
##subset zu miniMD, BEST, Prime, confGernerator
selected_data_energy<-PCA_results_all
selected_data_energy$process<-as.character(PCA_results_all$process)
selected_data_energy$structure_derieved<-as.character(PCA_results_all$structure_derieved)
selected_data_energy<-subset(selected_data_energy,subset=(process=="miniMD"|process=="miniPrime"|process=="miniConf"|process=="BEST"))


	selected_data_energy<-subset(selected_data_energy, subset= structure_derieved==a)
	selected_data_energy<-subset(selected_data_energy, subset= solvent==c)
	selected_data_energy<-subset(selected_data_energy, subset= charge_status==b)
	
print("Data selected based on user input")
print(dim(selected_data_energy))
	
#filter Energy
#energy in Energy_ABS in kj/mol
	selected_data_energy$Energy_ABS<-selected_data_energy$Energy_ABS*0.239006
print("Energy Filter")
	test<<-selected_data_energy
	#selected_data_energy<-selected_data_energy[selected_data_energy$Energy_ABS < as.numeric(energy_filter),]
	
print("new dimensions")
print(energy_filter)
print(dim(selected_data_energy))
#shrink df just to Energy and coresponding process
	energy_df<-selected_data_energy[,c("Energy_ABS","process")]
print(dim(energy_df))
#create Energy bins based on user desired number
	
#	energy_df$Energy_bins<-cut(energy_df$Energy_ABS,breaks=as.numeric(bin))

##relative Energien
energy_df$Energy_rel<-energy_df$Energy_ABS-min(energy_df$Energy_ABS)

energy_df[energy_df$Energy_rel>energy_filter,"Energy_rel"]<-20
energy_df$Energy_bins<-cut(energy_df$Energy_rel,breaks=c(0,6,12,18,24))



#count number of members for each process type in certain enrgy bin
	results_data<-list()
	for(i in unique(energy_df$process)){
		tmp<-energy_df[energy_df$process==i,]
		tmp<-as.data.frame(table(tmp$Energy_bins))
		colnames(tmp)<-c("Energy_bins","Freq")
		tmp$process<-i
		tmp$percentage<-round((tmp$Freq/sum(tmp$Freq))*100,1)
		
		results_data[[i]]<-tmp
	}

	results_data<-do.call(rbind,results_data)
	
	title<-paste0(as.character(options),collapse="_")
	title<-paste0("EnergyHisto_",title)
print(str(results_data))
p<-ggplot(results_data,aes(y=percentage,fill=process,x=Energy_bins))+geom_bar(stat="identity",position="dodge")+labs(title=title)
	p<- p+theme_bw()+theme(aspect.ratio=1, legend.position="bottom",legend.text=element_text(size=8) ,axis.text.x=element_text(angle=45,size=8,hjust=1),axis.text.y=element_text(size=8))
	
	print("test3")
	results<-list()
	results[["Plot"]]<-p
	results[["Title"]]<-title
	return(results)		
}

###creating plot based on button input
energy_1<-eventReactive(input$go_Energy_1,{results<-energy_histogram();return(results)}) 
###Plotting plot
output$energyPlot_1<-renderPlot({results=energy_1()$Plot;filename<-paste0(energy_1()$Title,".png")
				if(input$energy_saves_1){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_2<-eventReactive(input$go_Energy_2,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_2<-renderPlot({results=energy_2()$Plot;filename<-paste0(energy_2()$Title,".png")
				if(input$energy_saves_2){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_3<-eventReactive(input$go_Energy_3,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_3<-renderPlot({results=energy_3()$Plot;filename<-paste0(energy_3()$Title,".png")
				if(input$energy_saves_3){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_4<-eventReactive(input$go_Energy_4,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_4<-renderPlot({results=energy_4()$Plot;filename<-paste0(energy_4()$Title,".png")
				if(input$energy_saves_4){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_5<-eventReactive(input$go_Energy_5,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_5<-renderPlot({results=energy_5()$Plot;filename<-paste0(energy_5()$Title,".png")
				if(input$energy_saves_5){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_6<-eventReactive(input$go_Energy_6,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_6<-renderPlot({results=energy_6()$Plot;filename<-paste0(energy_6()$Title,".png")
				if(input$energy_saves_6){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})
###creating plot based on button input
energy_7<-eventReactive(input$go_Energy_7,{results<-energy_histogram();return(results)})
###Plotting plot
output$energyPlot_7<-renderPlot({results=energy_7()$Plot;filename<-paste0(energy_7()$Title,".png")
				if(input$energy_saves_7){set_panel_size(file=filename,p=results,output=input$radio_energy)
				}else{
				return(results)
				}
})

################################################PSA Histogramme##########################################################
histoPSA<-function(PCA_input){
data<-PCA_input$Data
title<-PCA_input$Title
#count number of members for each PSA bin
p<-ggplot(data, aes(x=PSA))+ geom_histogram(binwidth=5,color="black",alpha=0.5)+labs(title=title)
p<- p+theme_bw()+theme(aspect.ratio=1, legend.position="bottom",legend.text=element_text(size=8) ,axis.text.x=element_text(size=8,),axis.text.y=element_text(size=8))
return(p)
}
####Plotting based on subset selected in Tab 1
output$histoPSA_1<-renderPlot({results=histoPSA(PCA_input=results_1());filename<-paste0("histoPSA",results_1()$Title,".png")
				if(input$histoPSA_saves_1){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})
output$histoPSA_2<-renderPlot({results=histoPSA(PCA_input=results_2());filename<-paste0("histoPSA",results_2()$Title,".png")
				if(input$histoPSA_saves_2){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

output$histoPSA_3<-renderPlot({results=histoPSA(PCA_input=results_3());filename<-paste0("histoPSA",results_3()$Title,".png")
				if(input$histoPSA_saves_3){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

output$histoPSA_4<-renderPlot({results=histoPSA(PCA_input=results_4());filename<-paste0("histoPSA",results_4()$Title,".png")
				if(input$histoPSA_saves_4){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

output$histoPSA_5<-renderPlot({results=histoPSA(PCA_input=results_5());filename<-paste0("histoPSA",results_5()$Title,".png")
				if(input$histoPSA_saves_5){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

output$histoPSA_6<-renderPlot({results=histoPSA(PCA_input=results_6());filename<-paste0("histoPSA",results_6()$Title,".png")
				if(input$histoPSA_saves_6){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})

output$histoPSA_7<-renderPlot({results=histoPSA(PCA_input=results_7());filename<-paste0("histoPSA",results_7()$Title,".png")
				if(input$histoPSA_saves_7){set_panel_size(file=filename,p=results,output=input$radio)
				}else{
				return(results)
				}
})



################################################THE END####################################################################
}
