#DataFest 2023 
#attorneys
attorneys <- read.csv("attorneys.csv") 
dim(attorneys)
attorneys
#exploration1--distribution among using pie chart
#task1: find all unique states in "attorneys" dataset
unique_state <- list()
for(state in attorneys$StateName){
  if(!state %in% unique_state){
    unique_state <- append(unique_state, state)
  }
}

unique_state <- unlist(unique_state)#convert  list to vector
#there are 42 unique states
#task2: count the number of attorneys 
counter_state <-list()
for(state in unique_state){
  count <- 0
  for(s in attorneys$StateName){
    if(state == s){
      count = count + 1
    }
  }
  counter_state <- append(count, counter_state)
}
#counter_state <- unlist(counter_state) 
#task3: eventually present the pie chart
att_pie <- pie(counter_state,labels=unique_state,main="The distribution of attorneys among states in the U.S.", cex=0.5,init.angle=90)
att_pie

#attorneyTimeEntries
attor_time_entry <- read.csv("attorneytimeentries.csv")
dim(attor_time_entry)

#exploration2-- hours spent responding to client questions
#statistics
avg_hours <- mean(attor_time_entry$Hours)
avg_hours  # average: 0.3917
hours_75 <- quantile(attor_time_entry$Hours,0.75)
hours_75    #75 quantile: 0.4
mid_hours <- quantile(attor_time_entry$Hours,0.50)
mid_hours   #median: 0.2
hours_25 <- quantile(attor_time_entry$Hours, 0.25)
hours_25    #25 quantile: 0.1

#boxplot
hour_box <-boxplot(attor_time_entry$Hours, main="The distribution of hours spent responding to client questions",ylab="hours spent responding to client questions")
hour_box

#statesites
statesites <- read.csv("statesites.csv")
dim(statesites)

#allowed assets
avg_asset <- mean(statesites$AllowedAssets)
avg_asset  # average: 92142.86
asset_75 <- quantile(statesites$AllowedAssets,0.75)
asset_75    #75 quantile: 10000
mid_asset <- quantile(statesites$AllowedAssets,0.50)
mid_asset   #median: 10000
asset_25 <- quantile(statesites$AllowedAssets,0.25)
asset_25    #25 quantile: 10000
asset_box <-boxplot(statesites$AllowedAssets, main="The distribution of the total allowed assets across the U.S.",ylab="State's limit for the total allowed amount for all accounts")
asset_box

#base income limit:
#by peeking through the dataset, we know that
#every state has 13590 except Hawaii 15630

#per household member income limit
#by peeking through the dataset, we know that
#every state has 4720 except Hawaii 5430

#income multiplier
avg_mul <- mean(statesites$IncomeMultiplier)
avg_mul  # average: 2.869048
mul_75 <- quantile(statesites$IncomeMultiplier,0.75)
mul_75    #75 quantile: 3
mid_mul <- quantile(statesites$IncomeMultiplier,0.50)
mid_mul   #median: 2.5
mul_25 <- quantile(statesites$IncomeMultiplier,0.25)
mul_25    #25 quantile: 2.5
mul_box <-boxplot(statesites$IncomeMultiplier, main="The distribution of income multiplier across the U.S.",ylab="Income multiplier used for determining client's eligibility based on income")
mul_box


clients -- really important
clients <- read.csv("clients.csv")
factor: ethnic identity
ethics <- list()
for(e in clients$EthnicIdentity){
  if(!e %in% ethics){
    ethics <- append(e, ethics)
  }
}
count_ethic <- list()
for(e in ethics){
  count <- 0
  for(c in clients$EthnicIdentity){
    if(c == e){
      count <- count + 1
    }
  }
  count_ethic <- append(count, count_ethic)
}
ethics <- unlist(ethics)
count_ethic <- unlist(count_ethic)
pie(count_ethic,labels=ethics,main="The distribution of ethnic identity of clients", cex=0.5,init.angle=90)

#factor: gender
genders <- list()
for(g in clients$Gender){
  if(!g %in% genders){
    genders <- append(g, genders)
  }
}
count_gen <- list()
for(g in genders){
  count <- 0
  for(c in clients$Gender){
     if(c == g){
      count <- count + 1
    }
  }
  count_gen <- append(count, count_gen)
}
genders <- unlist(genders)
count_gen <- unlist(count_gen)
pie(count_gen,labels=genders,main="The distribution of genders of clients", cex=0.5,init.angle=90)

#factor: marital status
marry <- list()
for(m in clients$MaritalStatus){
   if(!m %in% marry){
   marry <- append(m, marry)
  }
}
count_ma <- list()
for(m in marry){
  count <- 0
  for(c in clients$MaritalStatus){
    if(c == m){
      count <- count + 1
    }
  }
  count_ma <- append(count, count_ma)
}
marry <- unlist(marry)
count_ma <- unlist(count_ma)
pie(count_ma,labels=marry,main="The distribution of marital status of clients", cex=0.5,init.angle=90)

#factor: veteran
veteran <- list()
for(v in clients$Veteran){
  if(!v %in% veteran){
    veteran <- append(v, veteran)
  }
}
count_v <- list()
for(v in veteran){
  count <- 0
  for(c in clients$Veteran){
     if(c == v){
      count <- count + 1
    }
  }
  count_v <- append(count, count_v)
}
veteran <- unlist(veteran)
count_v <- unlist(count_v)
pie(count_v,labels=veteran,main="The distribution of veteran status of clients", cex=0.5,init.angle=90)

#factor: imprisoned? 
prison <- list()
for(p in clients$Imprisoned){
  if(!p %in% prison){
    prison <- append(p, prison)
  }
}
count_p <- list()
for(p in prison){
  count <- 0
  for(c in clients$Imprisoned){
    if(c == p){
      count <- count + 1
    }
  }
  count_p <- append(count, count_p)
}
prison <- unlist(prison)
count_p <- unlist(count_p)
pie(count_p,labels=prison,main="The distribution of prison status of clients", cex=0.5,init.angle=90)


#factor: age # picking 1000 random non-null ages
ages <- list()
cnt <- 0
for(age in clients$Age){
  if(age != "NULL"){
    age <- strtoi(age)
    age <- as.numeric(age)
    ages <- append(age, ages) 
    cnt = cnt + 1
  }
  if(cnt > 1000){
    break
  }
}
ages
num_age <- list()
for(age in ages){
  num_age <- append(age,num_age)
}
num_age <- unlist(num_age)
avg_age <- mean(num_age)
avg_age #average: 41.16
age_75 <- quantile(num_age,0.75)
age_75    #75 quantile: 51
mid_age <- quantile(num_age,0.50)
mid_age   #median: 39
age_25 <- quantile(num_age,0.25)
age_25    #25 quantile: 30
age_box <-boxplot(num_age, main="The distribution of the age of clients",ylab="the age of clients")
age_box

#factor: #person in household
house <- list()
cnt <- 0
for(per in clients$NumberInHousehold){
  if(per != "NULL"){
    per <- strtoi(per)
    per <- as.numeric(per)
    house <- append(per, house) 
    cnt = cnt + 1
  }
  if(cnt > 1000){
    break
  }
}

num_hou <- list()
for(hou in house){
  num_hou <- append(hou,num_hou)
}
num_hou <- unlist(num_hou)
avg_hou <- mean(num_hou)
avg_hou #average: 2,656
hou_75 <- quantile(num_hou,0.75)
hou_75    #75 quantile: 4 
mid_hou <- quantile(num_hou,0.50)
mid_hou   #median: 2
hou_25 <- quantile(num_hou,0.25)
hou_25    #25 quantile: 1 
hou_box <-boxplot(num_hou, main="The distribution of the number of people residing in the client's household",ylab="the number of people residing in the client's household")
hou_box

factor: income <seemingly quite important>
annual income
income <- list()
cnt <- 0
for(per in clients$AnnualIncome){
  if(per != "NULL" && per != "0"){
    per <- as.numeric(per)
    income <- append(per, income) 
    cnt = cnt + 1
  }
  if(cnt > 1000){
    break
  }
}
num_income <- list()
for(hou in income){
  num_income <- append(hou,num_income)
}
num_income <- unlist(num_income)
avg_income <- mean(num_income)
avg_income #average: 56217.74

in_75 <- quantile(num_income,0.75)
in_75    #75 quantile: 36000 
mid_in <- quantile(num_income,0.50)
mid_in   #median: 22000
in_25 <- quantile(num_income,0.25)
in_25    #25 quantile: 12000 
in_box <-boxplot(num_income, main="The distribution of the annual income of clients",ylab="the annual income of client")
in_box

#questions
questions <- read.csv("questions.csv")
all categories are collected
types <- list()
for(c in questions$Category){
  if(!c %in% types){
    types <- append(c,types)
  }
}
for(c in questions$Subcategory){
  if(!c %in% types){
    types <- append(c,types)
  }
}
count_type <- list()
for(t in types){
  count <- 0
  for(c in questions$Category){
    if(c == t){
      count <- count + 1
    }
  }
  for(c in questions$Subcategory){
    if(c == t){
       count <- count + 1
    }
  }
  count_type <- append(count, count_type)
}
types <- unlist(types)
count_type <- unlist(count_type)
cat_pie <- pie(count_type,labels=types,main="The distribution of categories of questions", cex=0.5,init.angle=90)
cat_pie

#question post 
post <- read.csv("questionposts_corrected.csv") 