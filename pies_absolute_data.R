setwd("C:/Users//am4613/Desktop/")

##Reference Scott et al. Molecular Systems biology (2014)

ribosomal_fraction <- read.delim("ribosome_biogenesis.txt", header = T, stringsAsFactors = F) #This is phi-R (hopefully)
transport <- read.delim("aminoacid_transporter.txt", header = T, stringsAsFactors = F)
metabolism <- read.delim("aminoacid_metabolism", header = T, stringsAsFactors = F)
nitrogen <- read.delim("nitrogen_metabolism.txt", header = T, stringsAsFactors = F)
tRNA <- read.delim("tRNA_metabolism", header = T, stringsAsFactors = F)
total <- rbind(transport, metabolism, nitrogen, tRNA, stringsAsFactors = F)
wo_duplicates <- unique(total) #This is phi-P (hopefully)
absolute_data <- read.delim("absolute_numbers.txt", skip = 6, header = T, stringsAsFactors = F)

n_ribo <- subset(absolute_data, absolute_data$Systematic.name %in% ribosomal_fraction$ensembl_id)
n_trans <- subset(absolute_data, absolute_data$Systematic.name %in% wo_duplicates$ensembl_id)
constant <- subset(absolute_data, !(absolute_data$Systematic.name %in% wo_duplicates$ensembl_id 
                                    | absolute_data$Systematic.name %in% ribosomal_fraction$ensembl_id))


##Multiply by MW to get mass fraction (some proteins are missing?)
mm <- c(sum(as.numeric(n_ribo$MM.protein.cpc)*n_ribo$Protein.mass..kDa., na.rm = T),
        sum(as.numeric(n_trans$MM.protein.cpc)*n_trans$Protein.mass..kDa., na.rm =T),
        sum(as.numeric(constant$MM.protein.cpc)*constant$Protein.mass..kDa., na.rm = T))
lbls = c("Ribosomal fraction", "Metabolic fraction", "Constant")
pie(mm, labels <- lbls)

mn <- c(sum(as.numeric(n_ribo$MN.protein.cpc)*n_ribo$Protein.mass..kDa., na.rm = T),
        sum(as.numeric(n_trans$MN.protein.cpc)*n_trans$Protein.mass..kDa., na.rm =T),
        sum(as.numeric(constant$MN.protein.cpc)*constant$Protein.mass..kDa., na.rm = T))
lbls = c("Ribosomal fraction", "Metabolic fraction", "Constant")
pie(mn, labels <- lbls)