
### This script is to interact with the Xcell_DB that is on amazonAWS
### April 15th 2017

### hide password:
# see http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
#"~/.Rprofile"
Sys.setenv(user = "adminWSL")
Sys.setenv(pw = "Miglieglia1971!")
# script.R
# id and pw are defined in the script by virtue of .Rprofile
#call_service(used = Sys.getenv("user"), pw = Sys.getenv("pw"))
Sys.getenv("user")
Sys.getenv("pw")


# 1. load libraries and WD -------------------------------------------------------
library(RPostgreSQL)
library(tidyverse)
library(DBI)
library(xlsx)
library(plyr)
library(data.table)
library(readxl)
library(caroline)

type<-"ROXAS"   # select among ROXAS / IPP / ...
TAKE.INFO.FROM.ROXAS<-"yes"     #### here note "yes" if you want that some info are taken from Roxas file and not from the metadatafile


# Define the folder where all the site data are stored
WD<-setwd("~/Desktop/Back up Desktop Famoso/In prep/2016 Audrey LOUY/Audrey Data/S22/")
WD<-setwd("~/Desktop/Back up Desktop Famoso/In prep/2016 Audrey LOUY/Audrey Data/S22/")

# Upload file lists (xlsm.file, output.file, calibration.file) 
# It search within all subfolders for the "output.xls" files and list them in LOAD 
if (type=="ROXAS") {
  LOAD.xlsx<-dir(WD,recursive = TRUE,pattern= "Output.xlsx")  
  LOAD.ring<-dir(WD,recursive = TRUE,pattern= "Output_Rings")  
  LOAD.cell<-dir(WD,recursive = TRUE,pattern= "Output_Cells")  
  LOAD.xlsm<-dir(WD,recursive = TRUE,pattern= ".xlsm")  
  LOAD.cal<-dir(WD,recursive = TRUE,pattern= ".cal")
}
sheets <- excel_sheets(LOAD.xlsm)[grep("MeasuringTable",excel_sheets(LOAD.xlsm))]

# 2. Create a connection to the DB ----------------------------------------
source('pw.R')


# 3. Get a general information about the DB -----------------------------
#--/ List availale tables
db_list_tables(Xcell_db)
#--/ List columns in the table
dbListFields(Xcell_db, "institution_fk")

# 4. upload data to the FK-tables ----------------------------------------------
if (length(TREE)==0) {
  sheetsNameList <- excel_sheets("~/Desktop/XcellDB/Tables_Lists.xlsx")
  
  for (i in sheetsNameList[-7]) {           ## Excluded institution_fk
    print(i)
    sheet<-read_excel("~/Desktop/XcellDB/Tables_Lists.xlsx", sheet = i)
    dbWriteTable(conn = Xcell_db, 
                 name = i, # name of the table that you want to modify
                 value = sheet, # data you want to add
                 overwrite = FALSE, # you don't want to overwrite a table, just append
                 append = TRUE, # since the table already exist you just want to append it
                 row.names=FALSE) # if add the rownames
  }
}


# 5a Upload metada.file
# 5 1 add additional new insitute in Institution_fk -----------------
#dbListFields(Xcell_db, "institution_fk")
Inst<-read_excel(LOAD.xlsm, sheet = "General")
institution_code<-Inst[which(Inst[,1]=="Institution_Code (short) *"),2]
institution_name<-Inst[which(Inst[,1]=="Institution"),2]
countries<-unlist(Inst[which(Inst[,1]=="Country"),2])
country_code<-NULL
for (i in countries) {
  cc<- Ccode$country_code[which( unlist(Ccode[,"Country"]) %in%  i  )]
  country_code<-cbind(country_code,cc)
}
country_code<-data.frame(t(country_code),row.names = NULL)
department<-Inst[which(Inst[,1]=="Department"),2]
street<-Inst[which(Inst[,1]=="street"),2]
postal_code<-Inst[which(Inst[,1]=="postal code"),2]
city<-Inst[which(Inst[,1]=="City"),2]
institution<-cbind(institution_code,institution_name,country_code,department,street,postal_code,city)
colnames(institution)<-c("institution_code","institution_name","country_code","department","street","postal_code","city")
institution<-subset(institution,!is.na(institution_code))
institution<-institution[!duplicated(institution), ]

## Here it would be important to select only the new (and unique) Institutes. 
'%!in%' <- function(x,y)!('%in%'(x,y))
if (length(TREE)==0) {
  new.institution<-as.data.frame(institution)
} else {
  institution.inDB<-dbGetQuery(Xcell_db, "SELECT institution_name from institution_fk")[,"institution_name"]
  new.institution<-as.data.frame(t(institution[which(institution[,"institution_name"] %!in% institution.inDB),]))
}

# Upload institution_fk in DB 
dbListFields(Xcell_db, "institution_fk")
if (nrow(new.institution)!=0) {
  dbWriteTable(conn = Xcell_db, 
               name = "institution_fk", # name of the table that you want to modify
               value = new.institution, # data you want to add
               overwrite = FALSE, # you don't want to overwrite a table, just append
               append = TRUE, # since the table already exist you just want to append it
               row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
#dbGetQuery(Xcell_db, "SELECT * from institution_fk")



# 5 2 add person ------------------
# dbListFields(Xcell_db, "person")
pers<-read_excel(LOAD.xlsm, sheet = "General")
last_name<-pers[which(pers[,1]=="Last Name"),2]
first_name<-pers[which(pers[,1]=="First Name"),2]
email<-pers[which(pers[,1]=="E-mail"),2]
institution_code<-pers[which(pers[,1]=="Institution_Code (short) *"),2]
webpage<-pers[which(pers[,1]=="webpage"),2]
phone_number<-pers[which(pers[,1]=="Phone number"),2]
person<-cbind(last_name,first_name,email,institution_code,webpage,phone_number)
person<-as.data.frame(subset(person,nchar(last_name) > 2))
colnames(person)<-c('last_name',"first_name","email","institution_code","webpage","phone_number")

## Here it would be important to select only the new (and unique) Person. 
checkifnew<-dbGetQuery(Xcell_db, "SELECT * from person")
ifelse(is.null(checkifnew$person_id),new.person<-person, new.person<-person[-which(person$last_name %in% checkifnew$last_name),])
#new.person$person.id<-id.INIT.person:(id.INIT.person+nrow(new.person)-1)


# Upload person in DB 
#dbListFields(Xcell_db, "person")
if (nrow(new.person)!=0) {
  dbWriteTable2(con = Xcell_db, pg.update.seq=TRUE, add.id=TRUE, fill.null=TRUE,
                table.name = "person", # name of the table that you want to modify
                df = new.person[-2,], # data you want to add
                overwrite = FALSE, # you don't want to overwrite a table, just append
                append = TRUE, # since the table already exist you just want to append it
                row.names=FALSE) # if add the rownames
}


#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from person")



# 5 3 site -----------------------------
# dbListFields(Xcell_db, "site")
sit<-read_excel(LOAD.xlsm, sheet = "General")
site_code<-sit[which(sit[,1]=="Site Code (Short)"),2]
site_label<-sit[which(sit[,1]=="Site label"),2]
country_code<-substr((sit[which(sit[,1]=="Full Site Code *"),2]),1,2)
longitude<-as.character(as.numeric(sit[which(sit[,1]=="Longitude"),2]))
latitude<-as.character(as.numeric(sit[which(sit[,1]=="Latitude"),2]))
elevation<-as.character(as.numeric(sit[which(sit[,1]=="Elevation"),2]))
aspect<-as.character(as.numeric(sit[which(sit[,1]=="Aspect"),2]))
slope<-as.character(as.numeric(sit[which(sit[,1]=="Slope"),2]))
soil_depth<-sit[which(sit[,1]=="Soil depth"),2]
soil_water_capacity<-sit[which(sit[,1]=="Soil water holding capacity"),2]
species_composition<-sit[which(sit[,1]=="Species Composition"),2]
management<-sit[which(sit[,1]=="Management Intensity"),2]
data_location<-sit[which(sit[,1]=="Data location"),2]
image_location<-sit[which(sit[,1]=="Image location"),2]
sample_location<-sit[which(sit[,1]=="Sample location"),2]

site<-cbind(site_code,site_label,country_code,longitude,latitude,elevation,aspect,slope,soil_depth,
            soil_water_capacity,species_composition,management,data_location,image_location,sample_location)
#site<-as.data.frame(subset(site,nchar(site_code) > 2))
colnames(site)<-c("site_code","site_label","country_code","longitude","latitude","elevation","aspect","slope","soil_depth",
                  "soil_water_capacity","species_composition","management","data_location","image_location","sample_location")
site <- data.frame(lapply(site, as.character), stringsAsFactors=FALSE)

# Upload site in DB 
# dbListFields(Xcell_db, "site")
if (nrow(site)==1) {
  dbWriteTable2(con = Xcell_db, pg.update.seq=TRUE, add.id=TRUE,
                table.name = "site", # name of the table that you want to modify
                df = site, # data you want to add
                overwrite = FALSE, # you don't want to overwrite a table, just append
                append = TRUE, # since the table already exist you just want to append it
                row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from site")




# 5 4 person_role -----------------------------
# dbListFields(Xcell_db, "person_role")
p.role<-read_excel(LOAD.xlsm, sheet = "General")
last_name<-p.role[which(p.role[,1]=="Last Name"),2]
last_name<-last_name[which(nchar(last_name)>2)]
person<-dbGetQuery(Xcell_db, "SELECT * from person")
person_id<-NULL
for (i in 1:(nrow(last_name))) {
  id<-as.numeric(person$id[which(person$last_name==as.character(last_name[i,]))])
  person_id<-c(person_id,id)
}
site_code<-as.character(p.role[which(p.role[,1]=="Site Code (Short)"),2])
site<-dbGetQuery(Xcell_db, "SELECT * from site")
site.ID<-site$id[which(site$site_code==site_code)]
site_id<-rep(site.ID,nrow(last_name))
role<-rep(1,nrow(last_name))
ifelse(as.character(p.role[which(p.role[,1]=="Is contact person also data owner?"),2])=="Yes",role[1]<-3,role[1]<-2)  
person_role<-as.data.frame(cbind(site_id,person_id,role))

# Upload person_role in DB 
# dbListFields(Xcell_db, "person_role")
if (nrow(person_role)!=0) {
  dbWriteTable2(con = Xcell_db, pg.update.seq=TRUE, add.id=TRUE,
                table.name = "person_role", # name of the table that you want to modify
                df = person_role, # data you want to add
                overwrite = FALSE, # you don't want to overwrite a table, just append
                append = TRUE, # since the table already exist you just want to append it
                row.names=FALSE) # if add the rownames
}

# check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from person_role")



# 5 5 publication -----------------------------
dbListFields(Xcell_db, "publication")
publi<-read_excel(LOAD.xlsm, sheet = "General")
reference<-publi[grep("Publication",unlist(publi[,1])),2]
reference<-reference[which(nchar(reference)>2)]
publication_id<-NULL
site_code<-as.character(p.role[which(p.role[,1]=="Site Code (Short)"),2])
site<-dbGetQuery(Xcell_db, "SELECT * from site")
site.ID<-site$id[which(site$site_code==site_code)]
publication<-as.data.frame(cbind(site.ID,reference))
colnames(publication)<-c("site_id","reference")
publication<-publication[which(!is.na(publication$reference)),]
publication <- data.frame(lapply(publication, as.character), stringsAsFactors=FALSE)

# Upload person_role in DB 
# dbListFields(Xcell_db, "publication")
if (nrow(publication)!=0) {
  dbWriteTable2(con = Xcell_db, pg.update.seq=TRUE, add.id=TRUE,
                table.name = "publication", # name of the table that you want to modify
                df = publication, # data you want to add
                overwrite = FALSE, # you don't want to overwrite a table, just append
                append = TRUE, # since the table already exist you just want to append it
                row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from publication")


# 5 6 tree -----------------------------
dbListFields(Xcell_db, "tree")
tr<-read_excel(LOAD.xlsm, sheet = "TreeTable")
tree_label<-t(tr[which(tr[,1]=="Tree Code *"),-c(1:2)])
tree_label<-tree_label[which(nchar(tree_label)>0),]
species<-t(tr[which(tr[,1]=="Species"),-c(1:2)])
species<-species[which(nchar(species)>0),]
DL<-read_excel(LOAD.xlsm, sheet = "DropLists", skip=1)
species_code<-NULL
for (i in 1:length(species)) {
  sp_code<-as.character(DL[which(DL[,"species"]==species[i]),"species_code"])
  species_code<-c(species_code,sp_code)
}
status_code<-t(tr[which(tr[,1]=="Social status"),c(3:(2+length(tree_label)))])
dbh<-(t(tr[which(tr[,1]=="DBH"),c(3:(2+length(tree_label)))]))
dbh<-as.numeric(dbh)
height<-t(tr[which(tr[,1]=="Height"),c(3:(2+length(tree_label)))])
height<-as.numeric(height)
age<-t(tr[which(tr[,1]=="Age"),c(3:(2+length(tree_label)))])
age<-as.numeric(age)
site_code<-as.character(p.role[which(p.role[,1]=="Site Code (Short)"),2])
site<-dbGetQuery(Xcell_db, "SELECT * from site")
site.ID<-site$id[which(site$site_code==site_code)]
tree<-cbind(site.ID,tree_label,species_code,status_code,dbh,height,age)
tree<-as.data.frame(tree)
colnames(tree)<-c("site_id","tree_label","species_code","status_code","dbh","height","age")
tree <- data.frame(lapply(tree, as.character), stringsAsFactors=FALSE)

# Upload tree in DB 
# dbListFields(Xcell_db, "tree")
if (nrow(tree)!=0) {
  dbWriteTable2(con = Xcell_db, pg.update.seq=TRUE, add.id=TRUE,
                table.name = "tree", # name of the table that you want to modify
                df = tree, # data you want to add
                overwrite = FALSE, # you don't want to overwrite a table, just append
                append = TRUE, # since the table already exist you just want to append it
                row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from tree")




# 5 7 wood_sample -----------------------------
dbListFields(Xcell_db, "wood_sample")
wood<-read_excel(LOAD.xlsm, sheet = "TreeTable")
sample_label<-as.vector(t(wood[which(wood[,1]=="Sample label"),-c(1:2)]))
pos<-which(nchar(sample_label)>0)
sample_label<-sample_label[pos]
radius<-substr(sample_label,nchar(sample_label),nchar(sample_label))
radius<-radius[pos]
sample_type<-as.vector(t(wood[which(wood[,1]=="Sample type *"),-c(1:2)]))
sample_type<-sample_type[pos]
organ<-as.vector(t(wood[which(wood[,1]=="Organ"),-c(1:2)]))
organ<-organ[pos]
cell_type<-as.vector(t(wood[which(wood[,1]=="Cell Type"),-c(1:2)]))
cell_type<-cell_type[pos]
stem_heigth<-as.vector(t(wood[which(wood[,1]=="Stem heigth at sampling location"),-c(1:2)]))
stem_heigth<-as.numeric(stem_heigth[pos])
apex_dist<-as.vector(t(wood[which(wood[,1]=="Distance from tip (only for branches)"),-c(1:2)]))
apex_dist<-as.numeric(apex_dist[pos])

sample_id<-id.INIT.sample:(id.INIT.sample+length(sample_label)-1)
trees<-NULL
for (i in 1:length(sample_label)) {
  #  A<-unlist(strsplit(sample_label[i],"_"))[1]
  A<-substr(sample_label[i],1,nchar(sample_label[i])-2)
  trees<-c(trees, A)
}
#trees<-unlist(trees) 
tree_id<-NULL
for (i in 1:length(sample_label)) {
  B<-which(tree_label == trees[i])+id.INIT.tree-1
  tree_id<-c(tree_id,B)
}
wood_sample<-cbind(tree_id,sample_id,sample_label,sample_type,radius,organ,cell_type,stem_heigth,apex_dist)
wood_sample<-as.data.frame(wood_sample)
colnames(wood_sample)<-c("tree_id","sample_id","sample_label","sample_type","radius","organ","cell_type","stem_heigth","apex_dist")

# Upload wood_sample in DB 
# dbListFields(Xcell_db, "wood_sample")
if (nrow(wood_sample)!=0) {
  dbWriteTable(conn = Xcell_db, 
               name = "wood_sample", # name of the table that you want to modify
               value = wood_sample, # data you want to add
               overwrite = FALSE, # you don't want to overwrite a table, just append
               append = TRUE, # since the table already exist you just want to append it
               row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from wood_sample")


# 5 8 measure_info -----------------------------
# dbListFields(Xcell_db, "measure_info")

measure<-NULL
for (MTable in sheets[]) {
  print(MTable)
  measure0<-read_excel(LOAD.xlsm, sheet = MTable)[1:75,]
  measure<-rbind(measure,measure0)
}
### To simplify work on setting information (only for ROXAS type of file)
ROXAS.SETTINGS<-NULL
for (i in LOAD.xlsx[]) {
  setting<-as.data.frame(t(read_excel(paste(WD,i,sep="/"), sheet="ROXAS settings")[7:9,2]))
  rownames(setting)<-i
  ROXAS.SETTINGS<-rbind(ROXAS.SETTINGS,setting)
}
CAL<-NULL
for (i in LOAD.cal) {
  Cal<-read.table(paste(WD,i,sep="/"), sep="\t", skip=1)
  cal<-as.numeric(strsplit(as.character(Cal[3,1])," ")[[1]][2])
  file<-strsplit(as.character(Cal[1,1]),"=")[[1]][2]
  CAL<-cbind(CAL,c(cal,file))
}
ROXAS.SETTINGS<-cbind(rownames(ROXAS.SETTINGS),ROXAS.SETTINGS,t(CAL))
rownames(ROXAS.SETTINGS)<-1:nrow(ROXAS.SETTINGS)
colnames(ROXAS.SETTINGS)<-c("File name Data","Configuration filename","Date","Software version","Calibration","File")
head(ROXAS.SETTINGS)  

subpiece_label<-as.vector(t(measure[which(measure[,1]=="sub-piece label"),-c(1:2)]))
pos<-which(nchar(subpiece_label)>4)
subpiece_label<-subpiece_label[pos]

sample_label<-as.vector(t(measure[which(measure[,1]=="Sample label"),-c(1:2)]))
pos1<-which(sample_label!="NA")
sample_label<-sample_label[pos1]
sample_label<-substr(sample_label,1,nchar(sample_label)-2)

system<-as.vector(t(measure[which(measure[,1]=="Measuring System"),-c(1:2)]))
system<-system[pos1]
software<-as.vector(t(measure[which(measure[,1]=="Measurement software"),-c(1:2)]))
software<-software[pos1]
software_version<-as.vector(t(measure[which(measure[,1]=="Software version"),-c(1:2)]))
software_version<-software_version[pos1]
mes_geometry<-as.vector(t(measure[which(measure[,1]=="Measurement geometry"),-c(1:2)]))
mes_geometry<-mes_geometry[pos1]
only_ew<-as.vector(t(measure[which(measure[,1]=="Only earlywood measurements?"),-c(1:2)]))
only_ew<-only_ew[pos1]
Unit<-as.vector(t(measure[which(measure[,1]=="Measurement unit"),-c(1:2)]))
Unit<-Unit[pos1]
TangLength<-as.vector(t(measure[which(measure[,1]=="Measured tangential image size (e.g. core size)"),-c(1:2)]))
TangLength<-TangLength[pos1]
TABLE.SAMPLE<-as.data.frame(cbind(sample_label,system,software,software_version,mes_geometry,only_ew,TangLength))
if (length(which(duplicated(TABLE.SAMPLE$sample_label))==TRUE)>0) {
  TABLE.SAMPLE<-TABLE.SAMPLE[-which(duplicated(TABLE.SAMPLE$sample_label)),]
}

image_size<-as.vector(t(measure[which(measure[,1]=="Image size"),-c(1:2)]))
image_size<-as.numeric(image_size[pos])
magnification<-as.vector(t(measure[which(measure[,1]=="Magnification"),-c(1:2)])) 
magnification<-as.numeric(magnification[pos])
calibration<-as.vector(t(measure[which(measure[,1]=="Calibration"),-c(1:2)])) 
calibration<-as.numeric(calibration[pos])
image_filename<-as.vector(t(measure[which(measure[,1]=="File name Image"),-c(1:2)])) 
image_filename<-image_filename[pos]
data_filename<-as.vector(t(measure[which(measure[,1]=="File name Data"),-c(1:2)]))
data_filename<-data_filename[pos]
configuration_filename<-as.vector(t(measure[which(measure[,1]=="Configuration file name"),-c(1:2)]))
configuration_filename<-configuration_filename[pos]
editing_level<-as.vector(t(measure[which(measure[,1]=="Level of editing"),-c(1:2)]))
editing_level<-editing_level[pos]
from<-as.vector(t(measure[which(measure[,1]=="Timeseries From"),-c(1:2)]))
from<-as.numeric(from[pos])
to<-as.vector(t(measure[which(measure[,1]=="Timeseries To"),-c(1:2)]))
to<-as.numeric(to[pos])
SL<-substr(subpiece_label,1,nchar(subpiece_label)-2)
sample_label<-substr(subpiece_label,1,nchar(subpiece_label)-4)
Addcor<-grep("_",substr(sample_label,nchar(sample_label)-1,nchar(sample_label)))  ## Correction for uniform sample label
sample_label[Addcor]<-substr(sample_label[Addcor],1,nchar(sample_label[Addcor])-1)
SL[Addcor]<-substr(SL[Addcor],1,nchar(SL[Addcor])-1)

measure_id<-id.INIT.measure:(id.INIT.measure+length(subpiece_label)-1)
TABLE.PIECE<-as.data.frame(cbind(sample_label,subpiece_label,image_size,magnification,calibration,image_filename,data_filename,
                                 configuration_filename,editing_level,from,to))
#TABLE.CORE <-dbGetQuery(Xcell_db, "SELECT * from wood_sample")
TABLE.CORE <-dbGetQuery(Xcell_db, paste0("SELECT * FROM tree
                                         INNER JOIN wood_sample ON tree.tree_id=wood_sample.tree_id
                                         WHERE site_id = ", escape(id.INIT.site)))
TABLE.CORE$sample_label<-substr(TABLE.CORE$sample_label,1,nchar(TABLE.CORE$sample_label)-2)

# merge Tables
TABLE.PIECE<-merge(TABLE.PIECE,TABLE.CORE[,c("sample_id","sample_label")], by="sample_label")
TABLE<-merge(TABLE.PIECE,TABLE.SAMPLE, by="sample_label")
TABLE[,"measure_id"]<-as.vector(measure_id)


measure_info<-TABLE[,c("subpiece_label","sample_id","measure_id","system","software","software_version",
                       "magnification","image_size","calibration","mes_geometry","only_ew","configuration_filename",
                       "data_filename","image_filename","editing_level","from","to")]        

if (TAKE.INFO.FROM.ROXAS=="yes") {
  head(ROXAS.SETTINGS)  
  MATCH<-NULL
  for (i in 1:nrow(ROXAS.SETTINGS)) {
    match<-grep(ROXAS.SETTINGS$File[i],TABLE$image_filename,fixed=TRUE)
    if(length(match)>1) {
      match<-match[which.min(nchar(as.character(TABLE$image_filename[match])))] 
    }
    MATCH<-c(MATCH,match)
  }
  ROXAS.SETTINGS<-cbind(ROXAS.SETTINGS,TABLE$image_filename[MATCH])
  colnames(ROXAS.SETTINGS)
  measure_info[,"software_version"]<-ROXAS.SETTINGS[,"Software version"]
  measure_info[,"calibration"]<-ROXAS.SETTINGS[,"Calibration"]
  measure_info[,"configuration_filename"]<-ROXAS.SETTINGS[,"Configuration filename"]
  measure_info[,"data_filename"]<-paste0(ROXAS.SETTINGS[,"File"],"_output.xlsx")
  JPG<-substr(TABLE$image_filename[MATCH],nchar(as.character(TABLE$image_filename[MATCH]))-3,nchar(as.character(TABLE$image_filename[MATCH])))
  measure_info[,"image_filename"]<-paste0(ROXAS.SETTINGS[,"File"],JPG)
  #measure_info[,"mes_date"]<-ROXAS.SETTINGS[,"Data"]
}

# Upload wood_sample in DB 
dbListFields(Xcell_db, "measure_info")
if (nrow(measure_info)!=0) {
  dbWriteTable(conn = Xcell_db, 
               name = "measure_info", # name of the table that you want to modify
               value = measure_info, # data you want to add
               overwrite = FALSE, # you don't want to overwrite a table, just append
               append = TRUE, # since the table already exist you just want to append it
               row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
dbGetQuery(Xcell_db, "SELECT * from measure_info")



# 5 9 ring -----------------------


# to proceed tree by tree
TBT<-list(NULL)
for (i in TREE$tree_label) {
  tbt<-list(grep(i,LOAD.ring))
  TBT<-c(TBT,tbt)
}

if(is.null(SAMPLE)) {
  SAMPLE<-dbGetQuery(Xcell_db,"SELECT * FROM wood_sample")
  MEASURE<-dbGetQuery(Xcell_db, "SELECT * FROM measure_info")
}

### LOAD ring
all.ring<-NULL
for (k in LOAD.ring[]) {
  print(k)
  Ring<-as.tbl(read.table(paste(WD,k,sep="/"), sep="\t",skip=1,header=FALSE))[,1:34]
  colnames(Ring)<-c("ID","YEAR","RA","MRW","MINRW","MAXRW","MRADDIST","CNO","CD","CTA","RCTA","MCA","MINCA","MAXCA","KH","KS",
                    "RVGI","RVSF","RGSGV","AOIAR","RAOIAR","CWTPI","CWTBA","CWTLE","CWTRI","CWTTAN","CWTRAD","CWTALL","RTSR",
                    "CTSR","DH","DRAD","DTAN","BEND")
  #  Ring<-Ring[,-union(grep("X",colnames(Ring)),which(is.na(Ring[2,])))]                    # eliminate empty columns
  ifelse(length(which(Ring$CNO==0))==0,Ring<-Ring, Ring<-Ring[-which(Ring$CNO==0),]   )   # exclude years with no cells                                     # eliminate rows with no cells
  head(Ring)
  
  A<-merge(SAMPLE,MEASURE, by="sample_id")
  if(length(grep(as.character(unique(Ring$ID)) ,A$data_filename ))==0) {
    print(paste("skipped", k, sep=" "))
  }
  if(length(grep(as.character(unique(Ring$ID)) ,A$data_filename ))!=0) {  ## here we skip the section that are not in the sample list
    Ring$sample_id<-rep(unique(A[grep(as.character(unique(Ring$ID)) ,A$data_filename ),"sample_id"]),nrow(Ring))
    Ring$ring_id<-rep(NA, nrow(Ring))  ## will be filled later
    Ring$year<-Ring$YEAR
    Ring$ring_width<-Ring$MRW
    Ring$ring_area<-Ring$RA
    Ring$eww<-rep(NA, nrow(Ring))      ## for density profile data
    Ring$lww<-rep(NA, nrow(Ring))
    Ring$ewd<-rep(NA, nrow(Ring))
    Ring$lwd<-rep(NA, nrow(Ring))
    Ring$mxd<-rep(NA, nrow(Ring))
    Ring$mnd<-rep(NA, nrow(Ring))
    Ring$rvgi<-Ring$RVGI
    Ring$rvsf<-Ring$RVSF
    Ring$rgsgv<-Ring$RGSGV
    Ring$CNO<-Ring$CNO ### this is needed to eliminate the double rings
    
    ring<-Ring[,c("ring_id","sample_id","year","ring_width","ring_area","eww","lww","ewd","lwd",
                  "mxd","mnd","rvgi","rvsf","rgsgv","CNO")]
    all.ring<-rbind(all.ring,ring)
  }
} 
head(all.ring)
nrow(all.ring)


# Here is to check and eliminate the double ring
A<-ddply(all.ring, c("sample_id","year"), summarise, count=length(year)) 
double<-A[which(A$count>1),]
if (nrow(double)>0) {
  TODELETE<-NULL
  for(i in 1:nrow(double)) {
    same.ring<-intersect(which(all.ring$sample_id==double$sample_id[i]),which(all.ring$year==double$year[i]))
    todelete<-same.ring[which.min(unlist(c(all.ring[same.ring,"CNO"])))]
    TODELETE<-c(TODELETE,todelete)
  }
  all.ring<-all.ring[-TODELETE,]
}
nrow(all.ring)

# Here we keep only the announced years
all.ring.sub<-NULL
for (i in 1:length(unique(MEASURE$sample_id))) {
  AA<-unique((MEASURE$sample_id))[i]
  FROM<-MEASURE$from[which(MEASURE$sample_id==AA)]
  TO<-MEASURE$to[which(MEASURE$sample_id==AA)]
  KEEP<-NULL
  for(j in 1:length(FROM)) {
    yeartokeep<-c(FROM[j]:TO[j])
    KEEP<-c(KEEP, yeartokeep)
  }
  SUB<-all.ring[which(all.ring$sample_id==AA),]
  SUB<-SUB[which(SUB$year %in% KEEP),]
  all.ring.sub<-rbind(all.ring.sub,SUB)
  #all.ring<-all.ring[which(all.ring$sample_id==AA) & all.ring$year %in% KEEP,]
}

nrow(all.ring.sub)

# Assign ring.id
all.ring.sub$ring_id <- id.INIT.ring:(id.INIT.ring+nrow(all.ring.sub)-1)

ring<-all.ring.sub
ring<-ring[,-which(colnames(ring)=="CNO")]

# plot to check that the correct ring are included
plot(ring$year,ring$sample_id)


# Upload ring in DB 
dbListFields(Xcell_db, "ring")
if (nrow(ring)!=0) {
  dbWriteTable(conn = Xcell_db, 
               name = "ring", # name of the table that you want to modify
               value = ring, # data you want to add
               overwrite = FALSE, # you don't want to overwrite a table, just append
               append = TRUE, # since the table already exist you just want to append it
               row.names=FALSE) # if add the rownames
}


#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from ring")



# 5 10 tracheid_full -----------------------
# to proceed tree by tree
TBT<-list(NULL)
for (i in TREE$tree_label) {
  tbt<-list(grep(i,LOAD.cell))
  TBT<-c(TBT,tbt)
}

if(is.null(RING)) {
  SAMPLE<-dbGetQuery(Xcell_db,"SELECT * FROM wood_sample")
  MEASURE<-dbGetQuery(Xcell_db, "SELECT * FROM measure_info")
  RING<-dbGetQuery(Xcell_db, "SELECT * FROM ring")
}


### LOAD cell
all.cell<-NULL
for (k in LOAD.cell[]) {
  print(k)
  Cell<-as.tbl(read.table(paste(WD,k,sep="/"), sep="\t",header=TRUE))
  head(Cell)
  
  A<-merge(SAMPLE,MEASURE, by="sample_id")  
  # To skip up-loading data that are not mentioned in the LOAD.xlsmfile
  if(length(grep(as.character(unique(Cell$ID)) ,A$data_filename ))==0) {
    print(paste("skipped: check, maybe there are problems with the file names!!!", k, sep=" "))
  }
  if(length(grep(as.character(unique(Cell$ID)) ,A$data_filename ))!=0) {  ## here we skip the section that are not in the sample list
    Cell$sample_id<-rep(unique(A[grep(as.character(unique(Cell$ID)) ,A$data_filename ),"sample_id"]),nrow(Cell))
    
    Cell$sample_id<-rep(unique(A[grep(as.character(unique(Cell$ID)) ,A$data_filename ),"sample_id"]),nrow(Cell))
    Cell$ring_id<-rep(NA, nrow(Cell))  ## will be filled later
    Cell$cell_id<-rep(NA, nrow(Cell))  ## will be filled later
    Cell$year<-Cell$YEAR
    Cell$x_cal<-Cell$XCAL
    Cell$y_cal<-Cell$YCAL
    Cell$asp<-Cell$ASP
    Cell$majax<-Cell$MAJAX
    Cell$dist<-Cell$RADDISTR
    Cell$rel_dist<-Cell$RRADDISTR
    Cell$drad<-Cell$DRAD
    Cell$dtan<-Cell$DTAN
    Cell$ldrad<-Cell$DRAD-Cell$CWTPI-Cell$CWTBA   # To check!!!
    Cell$ldtan<-Cell$DTAN-Cell$CWTLE-Cell$CWTRI
    Cell$cwtpi<-Cell$CWTPI
    Cell$cwtba<-Cell$CWTBA
    Cell$cwtle<-Cell$CWTLE
    Cell$cwtri<-Cell$CWTRI
    Cell$cwttan<-Cell$CWTRAD
    Cell$cwtrad<-Cell$CWTTAN
    Cell$lum<-Cell$CA
    ifelse(length(grep("CWA",colnames(Cell)))==0,Cell$cwa<-rep(NA,nrow(Cell)),Cell$cwa<-Cell$CWA)
    ifelse(length(grep("BEND",colnames(Cell)))==0,Cell$bend<-rep(NA,nrow(Cell)),Cell$bend<-Cell$BEND)
    #Cell$cwa<-Cell$CWA
    #Cell$bend<-Cell$BEND
    
    tracheid_full<-Cell[,c("sample_id","cell_id","ring_id","year","x_cal","y_cal","asp","majax","dist","rel_dist","drad",
                           "dtan","ldrad","ldtan","cwtpi","cwtba","cwtle","cwtri","cwttan","cwtrad",
                           "lum","cwa","bend")]
    # Here we eliminate the double rings
    TF<-unique(paste(tracheid_full$sample_id,tracheid_full$year, sep="_"))
    AC<-unique(paste(all.cell$sample_id,all.cell$year, sep="_"))
    if (length(which(TF %in% AC))>0) {
      double<-TF[which(TF %in% AC)]
      for (i in 1:length(double)) {
        TF.count<-nrow(subset(tracheid_full,sample_id==as.numeric(strsplit(double[i],"_")[[1]][1]) &
                                year==as.numeric(strsplit(double[i],"_")[[1]][2])  ))
        AC.count<-nrow(subset(all.cell,sample_id==as.numeric(strsplit(double[i],"_")[[1]][1]) &
                                year==as.numeric(strsplit(double[i],"_")[[1]][2])  ))
        ifelse(TF.count<=AC.count,tracheid_full<-subset(tracheid_full,sample_id==as.numeric(strsplit(double[i],"_")[[1]][1]) &
                                                          year!=as.numeric(strsplit(double[i],"_")[[1]][2])),
               all.cell<-subset(all.cell,sample_id!=as.numeric(strsplit(double[i],"_")[[1]][1]) |
                                  year!=as.numeric(strsplit(double[i],"_")[[1]][2]))  )
      }
    }
    
    all.cell<-rbind(all.cell,tracheid_full)
  }
} 
head(all.cell)
nrow(all.cell)


# Here we keep only the "announced years"
all.cell.sub<-NULL
for (i in 1:length(unique(MEASURE$sample_id))) {
  AA<-unique(MEASURE$sample_id)[i]
  FROM<-MEASURE$from[which(MEASURE$sample_id==AA)]
  TO<-MEASURE$to[which(MEASURE$sample_id==AA)]
  KEEP<-NULL
  for(j in 1:length(FROM)) {
    yeartokeep<-c(FROM[j]:TO[j])
    KEEP<-c(KEEP, yeartokeep)
  }
  SUB<-all.cell[which(all.cell$sample_id==AA),]
  SUB<-SUB[which(SUB$year %in% KEEP),]
  all.cell.sub<-rbind(all.cell.sub,SUB)
}

nrow(all.cell.sub)

# Assign ring_id and cell_id
all.cell.sub<-merge(all.cell.sub[,-which(colnames(all.cell.sub)=="ring_id")],RING[,c("sample_id","ring_id","year")], by=c("sample_id","year"))
# ifelse(is.null(CELL$cell_id),first.cell_id<-0,first.cell_id<-max(CELL$cell_id))
# all.cell.sub$first.cell_id<-rep(first.cell_id,nrow(all.cell.sub))
# all.cell.sub<-all.cell.sub[order(all.cell.sub$sample_id, all.cell.sub$year,all.cell.sub$dist), ]
# all.cell.sub$cell_id <- all.cell.sub$first.cell_id + 1:nrow(all.cell.sub)
all.cell.sub$cell_id <-id.INIT.cell:(id.INIT.cell+nrow(all.cell.sub)-1)
#Cell$cell_id<-first.cell_id+(0:(nrow(Cell)-1))

tracheid_full<-all.cell.sub
tracheid_full<-tracheid_full[,c("cell_id","ring_id","x_cal","y_cal","asp","majax","dist",
                                "rel_dist","drad","dtan","ldrad","ldtan","cwtpi","cwtba","cwtle","cwtri",
                                "cwttan","cwtrad","lum","cwa","bend")]


# plot to check that the correct ring are included
plot(all.cell.sub$year,all.cell.sub$sample_id)

#tracheid_full[,"bend"]<-round(tracheid_full[,"bend"],2)
#tracheid_full[,"bend"]<-tracheid_full[,"bend"]/10
#tracheid_full[,"cwa"]<-tracheid_full[,"cwa"]/100
#tracheid_full[,"cwa"]<-round(tracheid_full[,"cwa"],2)

# Upload ring in DB 
# dbListFields(Xcell_db, "tracheid_full")
if (nrow(tracheid_full)!=0) {
  dbWriteTable(conn = Xcell_db, 
               name = "tracheid_full", # name of the table that you want to modify
               value = tracheid_full, # data you want to add
               overwrite = FALSE, # you don't want to overwrite a table, just append
               append = TRUE, # since the table already exist you just want to append it
               row.names=FALSE) # if add the rownames
}

#- check that it worked out by reading all data from the table
# dbGetQuery(Xcell_db, "SELECT * from tracheid_full")


#-----------------------------------------------------------

bGetQuery(Xcell_db, "SELECT COUNT(*) FROM tracheid_full")

dbDisconnect(Xcell_db)



# # 6 upload climatic table
# # 2. Create a connection to the DB ----------------------------------------
# Xcell_db <- dbConnect(dbDriver("PostgreSQL"),
#                       dbname = "Xcell",
#                       host = "postgresql-wsl.cmxphl52sxwm.us-west-2.rds.amazonaws.com",
#                       port = 5432,
#                       user = "adminWSL", 
#                       password = "Miglieglia1971!")
# 
# 
# dbGetQuery(Xcell_db, "SELECT * FROM site")
# 


