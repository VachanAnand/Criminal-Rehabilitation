# The Dataset is in more than 35 sheets, we nrrd to load the data 
library(ggplot2)
setwd("/Users/vachananand/Desktop/Monash/Monash Sem 2/Data Visualisation /assignment/Assignment 3")

# parole_df = read.csv("Details_of_inmates_parole_during_the_year.csv")
# released_df = read.csv("Inmates_released.csv")
# ipc_convicted_df = read.csv("IPC_crime_inmates_convicted.csv")
# ipc_trial_df = read.csv("IPC_crime_inmates_under_trial.csv")
# population_df = read.csv("Jail wise population of prison inmates.csv")
# vehicles_df = read.csv("Number_of_vehicles_available.csv")
# women_children_df = read.csv("Number_of_women_prisoners_with_children.csv")
# elec_equip_df = read.csv("Details_of_electronic_equipments_used_in_prison.csv")

age_df = read.csv("Age_group.csv")    ####
budget_df = read.csv("Budget.csv")
caste_df = read.csv("Caste.csv")
death_sentence_df = read.csv("Death_sentence.csv")
domicile_df = read.csv("Domicile.csv")
edu_facility_df = read.csv("Education_facilities.csv")
edu_df = read.csv("Education.csv")
inmate_expenditure_df = read.csv("Expenditure_on_inmates.csv")
expenditure_df = read.csv("Expenditure.csv")
deaths_df = read.csv("Inmates_death.csv")
escapes_df = read.csv("Inmates_escapee.csv")
mentally_ill_df = read.csv("Inmates_suffering_from_mental_ilness.csv")
population_df = read.csv("jail_wise_population_of_prison_inmates.csv")  ####
movement_df = read.csv("Movements_outside_jail_premises.csv")
detention_period_df = read.csv("Period_of_detention_of_undertrials.csv")
population_capacity_df = read.csv("Population_and_capacity_of_jails.csv")
recidivism_df = read.csv("Recidivism.csv")
rehabilitation_df = read.csv("Rehabilitation.csv")
religion_df = read.csv("Religion.csv")
sentence_df = read.csv("Sentence_period.csv")
official_count_df = read.csv("Strength_of_officials.csv")
officer_training_df = read.csv("Training_of_Jail_Officers.csv")
tranquillity_df = read.csv("Tranquillity.csv")
value_goods_df = read.csv("Value_of_goods_produced_by_inmates.csv")
vocation_df = read.csv("Vocational_training.csv")
wages_df = read.csv("Wages.csv")

all_df = c(age_df,budget_df,caste_df,death_sentence_df,domicile_df,edu_facility_df,edu_df,inmate_expenditure_df,expenditure_df,deaths_df,escapes_df,movement_df,detention_period_df,population_capacity_df,recidivism_df,rehabilitation_df,religion_df,sentence_df,official_count_df,officer_training_df,tranquillity_df,value_goods_df,vocation_df,population_df,wages_df)
years = list()
names(all_df)
for (each in c(1:length(all_df))){
  print(names(all_df[each]))
  if (names(all_df[each]) == "year"){
    years = c(years,as.vector(all_df[each]),recursive=TRUE)
  }
}

no_records_plot = ggplot() + aes(years)+ geom_histogram(binwidth=1, colour="black", fill="white") + scale_y_continuous("Number of Records")
no_records_plot



clean_data <- function(df){
  df = df[which(df$year <= 2010),]
  df
}

population_df = clean_data(population_df)
age_df = clean_data(age_df)
budget_df = clean_data(budget_df)
caste_df = clean_data(caste_df)
death_sentence_df = clean_data(death_sentence_df)
domicile_df = clean_data(domicile_df)
edu_facility_df = clean_data(edu_facility_df)
edu_df = clean_data(edu_df)
inmate_expenditure_df = clean_data(inmate_expenditure_df)
expenditure_df = clean_data(expenditure_df)
deaths_df = clean_data(deaths_df)
escapes_df = clean_data(escapes_df)
movement_df = clean_data(movement_df)
detention_period_df = clean_data(detention_period_df)
population_capacity_df = clean_data(population_capacity_df)
recidivism_df = clean_data(recidivism_df)
rehabilitation_df = clean_data(rehabilitation_df)
religion_df = clean_data(religion_df)
sentence_df = clean_data(sentence_df)
official_count_df = clean_data(official_count_df)
officer_training_df = clean_data(officer_training_df)
tranquillity_df = clean_data(tranquillity_df)
value_goods_df = clean_data(value_goods_df)
vocation_df = clean_data(vocation_df)
wages_df = clean_data(wages_df)

a = names(population_df)
a[18]
# first lets start of by checking the crime rate in India from 2001 to 2010
population_yw = aggregate(population_df[, 18], list(population_df$year), sum)
plot_yw = ggplot(population_yw,aes(as.integer(Group.1),x)) + geom_line()
plot_yw
population_syw = aggregate(population_df[, 18], list(population_df$year,population_df$state_ut_name), sum)
plot_syw = ggplot(population_syw,aes(as.integer(Group.1),x)) + geom_line() + facet_wrap(~Group.2) + scale_x_continuous(breaks = seq(2000,2010,2), "Years") + scale_y_continuous("Number of Inmates")+ theme(axis.text.x = element_text(angle = 90),strip.text = element_text(size=7)) 
plot_syw

age_16_18 =sum(age_df$age_16_18)
age_18_30 =sum(age_df$age_18_30)
age_30_50 =sum(age_df$age_30_50)
age_50_above =sum(age_df$age_50_above)


age = data.frame(group = c("16-18","18-30","30-50","50-above"),value = c(age_16_18,age_18_30,age_30_50,age_50_above))
age

age_compare = ggplot(age,aes(group,value)) + geom_bar(stat = "identity",aes(fill = group)) + labs(x = "Age Groups",y = "Number of Inmates") 
age_compare

type = aggregate(age_df[,c(7,8,9,10)],list(age_df$category),sum)
type["Total"] = c(0)
for (i in c(1:dim(type)[1])){
  type[i,6] = sum(type[i,2:5])
}

total = sum(type$Total)

for (i in c(1:dim(type)[1])){
  type[i,6] = (type[i,6]/total)*100
}

plot_type = ggplot(type,aes("",Total)) + geom_bar(stat="identity",aes(fill = Group.1)) + labs(x = "Inmate Type") + theme(axis.text.x=element_blank()) + geom_text(aes(y = Total/2 + c(0, cumsum(Total)[-length(Total)]),label =percent(Total/100)))
plot_type = plot_type + coord_polar("y", start=0)

budget_sw = aggregate(budget_df[,])



recidivism = aggregate(recidivism_df[,3:4],list(recidivism_df$state_name),sum)
recidivism["Percent"] = c(0)
for (i in c(1:dim(recidivism)[1])){
  recidivism[i,4] = (recidivism[i,3]/recidivism[i,2])*100
  
deaths_total = aggregate(deaths_df[,6],list(deaths_df$details),sum)
deaths_total = deaths_total[which(deaths_total$Group.1 == "Natural Deaths" | deaths_total$Group.1 == "Un-natural Deaths"),]
deaths_total["Percent"]=c(0)
deaths_total[1,3] = 11869/(11869+901 ) 
deaths_total[2,3] = 100-deaths_total[1,3]
tot_death_plot = ggplot(deaths_total,aes("",x)) + geom_bar(stat="identity",aes(fill= Group.1)) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+coord_polar("y", start=0)+ geom_text(aes(y = x/2 + c(0, cumsum(x)[-length(x)]),label =round(Percent)))
tot_death_plot
  



deaths = aggregate(deaths_df[,6],list(deaths_df$state_name,deaths_df$details),sum)
deaths = deaths[which(deaths$Group.2 != "Natural Deaths"),]
un = deaths[which(deaths$Group.2 != "Un-natural Deaths" ),]
ggplot(un,aes(Group.2,x)) + geom_bar(stat="identity") + theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 90)) + facet_wrap(~Group.1) + labs(y = "Number of Inmates")

}

