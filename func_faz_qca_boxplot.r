faz_qca_boxplot<-function(dados = a[a$STA_SUBDIV_SUBDIV==25,], var.y="weight", var.x="sex", var.id="SPMENVAR_SPMEN_ID", range_wiskers=2, same_ylim=TRUE, save_graph=FALSE, graph_dir=getwd(), graphfile_root=paste("apagar_",format(Sys.time(), "%Y%m%d%H%M"),sep=""))
	{
		outliers_out<-c()
		# abnormal length for sex
			par(mfrow=c(4,4), las=2, mar=c(1,2,2,1), oma=c(0,2,3,0))
				ylimite=range(dados[[var.y]], na.rm=T)
				
				for (j in sort(unique(dados[!is.na(dados[[var.x]]),][[var.x]])))
					{	
					cond<- !is.na(dados[[var.y]]) & (!is.na(dados[[var.x]]) & dados[[var.x]]==j)
					if(sum(cond)>0)
						{
						if(same_ylim==TRUE)
							{
							bx.plot<-boxplot(dados[cond,][[var.y]]~dados[cond,][[var.x]], range=range_wiskers, ylim=ylimite, main=paste(var.x,":", j, " (n=",nrow(dados[cond,]),")", sep=""), varwidth=TRUE, pch=21, outbg="red")
							} else { 
									bx.plot<-boxplot(dados[cond,][[var.y]]~dados[cond,][[var.x]], range=range_wiskers, main=paste(var.x,":", j, "(n=",nrow(dados[cond,]), sep=""),")", varwidth=TRUE, pch=21, outbg="red")
									}
						outliers<-dados[cond,var.id][dados[cond,][[var.y]]>bx.plot$stats[5] | dados[cond,][[var.y]]<bx.plot$stats[1]]			
						print(paste("level var.x:",j,"-> n_outliers:",length(outliers)))
						outliers_out<-c(outliers_out, outliers)
						}
					}	
		title(paste(toupper(var.y),"@",toupper(var.x)), outer=TRUE, line=1)
		print(paste("total n_outliers:",length(outliers_out)))
		outliers_out
	}
	
	# example
	data_qca<-data.frame(Y=rweibull(750, 0.5, scale = 1), X=c(rep("GROUP_A", 500),rep("GROUP_B", 250)))
	data_qca$X<-as.character(data_qca$X)
	data_qca$ID<-paste(data_qca$X, c(1:500, 1:250))
	
	faz_qca_boxplot(dados = data_qca, var.y="Y", var.x="X", var.id="ID", range_wiskers=2, same_ylim=TRUE, save_graph=FALSE, graph_dir=getwd(), graphfile_root=paste("apagar_",format(Sys.time(), "%Y%m%d%H%M"),sep=""))