
setwd("/Users/vachananand/Desktop/Data Science/Monash/Monash Sem 2/Data Visualisation /D3/Assignment")


budget_df = read.csv("Budget.csv")
deaths_df = read.csv("inmates_death.csv")
edu_facility_df = read.csv("Education_facilities.csv")
inmate_expenditure_df = read.csv("Expenditure_on_inmates.csv")
expenditure_df = read.csv("Expenditure.csv")
escapes_df = read.csv("Inmates_escapee.csv")
population_df = read.csv("jail_wise_population_of_prison_inmates.csv")
movement_df = read.csv("Movements_outside_jail_premises.csv")
recidivism_df = read.csv("Recidivism.csv")
rehabilitation_df = read.csv("Rehabilitation.csv")
vocation_df = read.csv("Vocational_training.csv")

all_df = c(budget_df,edu_facility_df,deaths_df,inmate_expenditure_df,expenditure_df,escapes_df,movement_df,recidivism_df,rehabilitation_df,vocation_df,population_df)
years = list()
names(all_df)
for (each in c(1:length(all_df))){
  print(names(all_df[each]))
  if (names(all_df[each]) == "year"){
    years = c(years,as.vector(all_df[each]),recursive=TRUE)
  }
}




clean_data <- function(df){
  df = df[which(df$year <= 2010),]
  df
}

population_df = clean_data(population_df)
budget_df = clean_data(budget_df)
edu_facility_df = clean_data(edu_facility_df)
inmate_expenditure_df = clean_data(inmate_expenditure_df)
expenditure_df = clean_data(expenditure_df)
escapes_df = clean_data(escapes_df)
movement_df = clean_data(movement_df)
recidivism_df = clean_data(recidivism_df)
rehabilitation_df = clean_data(rehabilitation_df)
vocation_df = clean_data(vocation_df)
deaths_df = clean_data(deaths_df)


population_ysw = aggregate(population_df[, 18], list(population_df$year,population_df$state_ut_name), sum)
population_sw = aggregate(population_df[, 18], list(population_df$state_ut_name), sum)
budget_ysw = aggregate(budget_df[, 4], list(budget_df$year,budget_df$area_name), sum)
edu_facility_ysw = aggregate(edu_facility_df[, 7], list(edu_facility_df$year,edu_facility_df$state_name), sum)
rehabilitation_ysw = aggregate(rehabilitation_df[, 7], list(rehabilitation_df$year,rehabilitation_df$state_name), sum)
inmate_expenditure_ysw = aggregate(inmate_expenditure_df[, 4], list(inmate_expenditure_df$year,inmate_expenditure_df$area_name), sum)
expenditure_ysw = aggregate(expenditure_df[, 4], list(expenditure_df$year,expenditure_df$area_name), sum)
deaths_ysw = aggregate(deaths_df[, 6], list(deaths_df$year,deaths_df$state_name), sum)
escapes_ysw = aggregate(escapes_df[, 6], list(escapes_df$year,escapes_df$state_name), sum)
movement_ysw = aggregate(movement_df[, 4], list(movement_df$year,movement_df$area_name), sum)
recidivism_ysw = aggregate(recidivism_df[, 4], list(recidivism_df$year,recidivism_df$state_name), sum)
vocation_ysw = aggregate(vocation_df[, 4], list(vocation_df$year,vocation_df$state_name), sum)


all_states = c("A & N Islands","Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chandigarh","Chhattisgarh","D & N Haveli","Daman & Diu","Delhi","Goa","Gujarat","Haryana","Himachal Pradesh","Jammu & Kashmir","Jharkhand","Karnataka","Kerala","Lakshadweep","Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram","Nagaland","Orissa","Pondicherry","Punjab","Rajasthan","Sikkim","Tamilnadu","Tripura","Uttar Pradesh","Uttaranchal","West Bengal")

voc_df = data.frame()
for (i in c(1:length(all_states))){
  df = vocation_ysw[as.character(vocation_ysw$Group.2) == all_states[i],]
  for (j in c(2001:2010)){

    if (j %in% df$Group.1){
      
    }else{
      
      if (all_states[i] == "Arunachal Pradesh" && j <= 2008){
          print("Not Required")
      }else{
        
        df = rbind(c(j,as.character(all_states[i]) ,mean(as.numeric(df$x))),df)
        print(as.character(all_states[i]))
        
        
      }
      
    }
  }
  voc_df = rbind(voc_df,df)
}

b= data.frame(rehabilitation_ysw$x,edu_facility_ysw$x)
b = rbind(c(122.75,102.62),c(122.75,102.62),b)
write.csv(b, file = "manipulated_b.csv")


a = data.frame(population_ysw$Group.1,population_ysw$Group.2,population_ysw$x ,budget_ysw$x,inmate_expenditure_ysw$x, expenditure_ysw$x ,deaths_ysw$x,escapes_ysw$x ,movement_ysw$x ,recidivism_ysw$x,as.numeric(voc_df$x))
a=cbind(a,b)
write.csv(a, file = "manipulated.csv")



n=data.frame()
for (i in c(1:length(all_states))){
  
  t = a[a$population_ysw.Group.2 == all_states[i],]
  
  
  for (j in c(3:length(t))){
    mi = min(t[,j])
    ma = max(t[,j])
    t[,j] = (t[,j] - mi)/(ma-mi)
  }
  
  n = rbind(n,t)
}

n[is.na(n)] = 0

write.csv(n, file = "manipulated_n.csv")


