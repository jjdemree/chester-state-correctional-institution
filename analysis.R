BJS_Data<-read.delim("C:/Users/Jacob/Desktop/BJS Stats/ICPSR_24642/DS0001/BJS_Data.tsv")
View(BJS_Data)
attach(BJS_Data)

#Size
mean(V78[which(V23==3)])
V78[which(V1==390000000074100000000)]
t.test(V78[which(V23==3)],mu=V78[which(V1==390000000074100000000)])

#Demographics
n<-1821
for(i in 1:n){
  if(V84[i]==9999){
    V84[i]<-NA
  }
}
for(i in 1:n){
  if(V85[i]==9999){
    V85[i]<-NA
  }
}
for(i in 1:n){
  if(V86[i]==9999){
    V86[i]<-NA
  }
}
for(i in 1:n){
  if(V87[i]==999){
    V87[i]<-NA
  }
}
for(i in 1:n){
  if(V88[i]==999){
    V88[i]<-NA
  }
}
for(i in 1:n){
  if(V89[i]==999){
    V89[i]<-NA
  }
}
for(i in 1:n){
  if(V90[i]==999){
    V90[i]<-NA
  }
}
for(i in 1:n){
  if(V91[i]==999){
    V91[i]<-NA
  }
}
for(i in 1:n){
  if(V93[i]==9999){
    V93[i]<-NA
  }
}

inmates_white<-rep(0,n)
for(i in 1:n){
  inmates_white[i]<-V84[i]/V93[i]
}
inmates_black<-rep(0,n)
for(i in 1:n){
  inmates_black[i]<-V85[i]/V93[i]
}
inmates_latino<-rep(0,n)
for(i in 1:n){
  inmates_latino[i]<-V86[i]/V93[i]
}
inmates_amerind<-rep(0,n)
for(i in 1:n){
  inmates_amerind[i]<-V87[i]/V93[i]
}
inmates_asian<-rep(0,n)
for(i in 1:n){
  inmates_asian[i]<-V88[i]/V93[i]
}
inmates_pacisl<-rep(0,n)
for(i in 1:n){
  inmates_pacisl[i]<-V89[i]/V93[i]
}
inmates_multi<-rep(0,n)
for(i in 1:n){
  inmates_multi[i]<-V90[i]/V93[i]
}
inmates_other<-rep(0,n)
for(i in 1:n){
  inmates_other[i]<-V91[i]/V93[i]
}

t.test(inmates_white[which(V23==3)],mu=inmates_white[which(V1==390000000074100000000)])
t.test(inmates_black[which(V23==3)],mu=inmates_black[which(V1==390000000074100000000)])
t.test(inmates_latino[which(V23==3)],mu=inmates_latino[which(V1==390000000074100000000)])
t.test(inmates_amerind[which(V23==3)],mu=inmates_amerind[which(V1==390000000074100000000)])
t.test(inmates_asian[which(V23==3)],mu=inmates_asian[which(V1==390000000074100000000)])
t.test(inmates_pacisl[which(V23==3)],mu=inmates_pacisl[which(V1==390000000074100000000)])
t.test(inmates_multi[which(V23==3)],mu=inmates_multi[which(V1==390000000074100000000)])
t.test(inmates_other[which(V23==3)],mu=inmates_other[which(V1==390000000074100000000)])

inmates_white_pa<-inmates_white[which(V11=="PA")]
inmates_black_pa<-inmates_black[which(V11=="PA")]
inmates_latino_pa<-inmates_latino[which(V11=="PA")]
inmates_amerind_pa<-inmates_amerind[which(V11=="PA")]
inmates_asian_pa<-inmates_asian[which(V11=="PA")]
inmates_pacisl_pa<-inmates_pacisl[which(V11=="PA")]
inmates_multi_pa<-inmates_multi[which(V11=="PA")]
inmates_other_pa<-inmates_other[which(V11=="PA")]

t.test(inmates_white_pa[which(V23==3)],mu=inmates_white[which(V1==390000000074100000000)])
t.test(inmates_black_pa[which(V23==3)],mu=inmates_black[which(V1==390000000074100000000)])
t.test(inmates_latino_pa[which(V23==3)],mu=inmates_latino[which(V1==390000000074100000000)])
t.test(inmates_amerind_pa[which(V23==3)],mu=inmates_amerind[which(V1==390000000074100000000)])
t.test(inmates_asian_pa[which(V23==3)],mu=inmates_asian[which(V1==390000000074100000000)])
t.test(inmates_pacisl_pa[which(V23==3)],mu=inmates_pacisl[which(V1==390000000074100000000)])
t.test(inmates_multi_pa[which(V23==3)],mu=inmates_multi[which(V1==390000000074100000000)])
t.test(inmates_other_pa[which(V23==3)],mu=inmates_other[which(V1==390000000074100000000)])

inmates_gender_pa<-as.factor(V22[which(V11=="PA")])
summary(na.omit(inmates_gender_pa[which(V23==3)]))

for(i in 1:n){
  if(V99[i]==9999){
    V99[i]<-NA
  }
}
for(i in 1:n){
  if(V100[i]==9999){
    V100[i]<-NA
  }
}
for(i in 1:n){
  if(V101[i]==999){
    V101[i]<-NA
  }
}
for(i in 1:n){
  if(V102[i]==9999){
    V102[i]<-NA
  }
}

inmates_oneplus<-rep(0,n)
for(i in 1:n){
  inmates_oneplus[i]<-V99[i]/V102[i]
}
inmates_oneless<-rep(0,n)
for(i in 1:n){
  inmates_oneless[i]<-V100[i]/V102[i]
}
inmates_unsentenced<-rep(0,n)
for(i in 1:n){
  inmates_unsentenced[i]<-V101[i]/V102[i]
}

t.test(inmates_oneplus[which(V23==3)],mu=inmates_oneplus[which(V1==390000000074100000000)])
t.test(inmates_oneless[which(V23==3)],mu=inmates_oneless[which(V1==390000000074100000000)])
t.test(inmates_unsentenced[which(V23==3)],mu=inmates_unsentenced[which(V1==390000000074100000000)])

inmates_oneplus_pa<-inmates_oneplus[which(V11=="PA")]
inmates_oneless_pa<-inmates_oneless[which(V11=="PA")]
inmates_unsentenced_pa<-inmates_unsentenced[which(V11=="PA")]

t.test(inmates_oneplus_pa[which(V23==3)],mu=inmates_oneplus[which(V1==390000000074100000000)])
t.test(inmates_oneless_pa[which(V23==3)],mu=inmates_oneless[which(V1==390000000074100000000)])
t.test(inmates_unsentenced_pa[which(V23==3)],mu=inmates_unsentenced[which(V1==390000000074100000000)])

for(i in 1:n){
  if(V94[i]==9999){
    V94[i]<-NA
  }
}
for(i in 1:n){
  if(V95[i]==9999){
    V95[i]<-NA
  }
}
for(i in 1:n){
  if(V96[i]==9999){
    V96[i]<-NA
  }
}
for(i in 1:n){
  if(V97[i]==9999){
    V97[i]<-NA
  }
}
for(i in 1:n){
  if(V98[i]==9999){
    V98[i]<-NA
  }
}


#Custody
custody_max<-rep(0,n)
for(i in 1:n){
  custody_max[i]<-V94[i]/V98[i]
}
custody_mid<-rep(0,n)
for(i in 1:n){
  custody_mid[i]<-V95[i]/V98[i]
}
custody_min<-rep(0,n)
for(i in 1:n){
  custody_min[i]<-V96[i]/V98[i]
}

t.test(custody_max[which(V23==3)],mu=custody_max[which(V1==390000000074100000000)])
t.test(custody_mid[which(V23==3)],mu=custody_mid[which(V1==390000000074100000000)])
t.test(custody_min[which(V23==3)],mu=custody_min[which(V1==390000000074100000000)])

custody_min_pa<-custody_min[which(V11=="PA")]
custody_mid_pa<-custody_mid[which(V11=="PA")]
custody_max_pa<-custody_max[which(V11=="PA")]

t.test(custody_max_pa[which(V23==3)],mu=custody_max[which(V1==390000000074100000000)])
t.test(custody_mid_pa[which(V23==3)],mu=custody_mid[which(V1==390000000074100000000)])
t.test(custody_min_pa[which(V23==3)],mu=custody_min[which(V1==390000000074100000000)])


#Staff Gender

for(i in 1:n){
  if(V143[i]==99){
    V143[i]<-NA
  }
}
for(i in 1:n){
  if(V144[i]==99){
    V144[i]<-NA
  }
}
for(i in 1:n){
  if(V145[i]==999){
    V145[i]<-NA
  }
}
for(i in 1:n){
  if(V146[i]==999){
    V146[i]<-NA
  }
}
for(i in 1:n){
  if(V147[i]==999){
    V147[i]<-NA
  }
}
for(i in 1:n){
  if(V148[i]==999){
    V148[i]<-NA
  }
}
for(i in 1:n){
  if(V149[i]==99){
    V149[i]<-NA
  }
}
for(i in 1:n){
  if(V150[i]==99){
    V150[i]<-NA
  }
}
for(i in 1:n){
  if(V151[i]==999){
    V151[i]<-NA
  }
}
for(i in 1:n){
  if(V152[i]==999){
    V152[i]<-NA
  }
}
for(i in 1:n){
  if(V154[i]==999){
    V154[i]<-NA
  }
}
for(i in 1:n){
  if(V155[i]==999){
    V155[i]<-NA
  }
}
for(i in 1:n){
  if(V156[i]==9999){
    V156[i]<-NA
  }
}
for(i in 1:n){
  if(V157[i]==999){
    V157[i]<-NA
  }
}

staff_admin_male<-rep(0,n)
for(i in 1:n){
  staff_admin_male[i]<-V143[i]/(V143[i]+V144[i])
}
staff_security_male<-rep(0,n)
for(i in 1:n){
  staff_security_male[i]<-V145[i]/(V145[i]+V146[i])
}
staff_clerical_male<-rep(0,n)
for(i in 1:n){
  staff_clerical_male[i]<-V147[i]/(V147[i]+V148[i])
}
staff_educ_male<-rep(0,n)
for(i in 1:n){
  staff_educ_male[i]<-V149[i]/(V149[i]+V150[i])
}
staff_proftech_male<-rep(0,n)
for(i in 1:n){
  staff_proftech_male[i]<-V151[i]/(V151[i]+V152[i])
}
staff_other_male<-rep(0,n)
for(i in 1:n){
  staff_other_male[i]<-V154[i]/(V154[i]+V155[i])
}
staff_total_male<-rep(0,n)
for(i in 1:n){
  staff_total_male[i]<-V156[i]/(V156[i]+V157[i])
}

t.test(staff_admin_male[which(V23==3)],mu=staff_admin_male[which(V1==390000000074100000000)])
t.test(staff_security_male[which(V23==3)],mu=staff_security_male[which(V1==390000000074100000000)])
t.test(staff_clerical_male[which(V23==3)],mu=staff_clerical_male[which(V1==390000000074100000000)])
t.test(staff_educ_male[which(V23==3)],mu=staff_educ_male[which(V1==390000000074100000000)])
t.test(staff_proftech_male[which(V23==3)],mu=staff_proftech_male[which(V1==390000000074100000000)])
t.test(staff_other_male[which(V23==3)],mu=staff_other_male[which(V1==390000000074100000000)])
t.test(staff_total_male[which(V23==3)],mu=staff_total_male[which(V1==390000000074100000000)])

staff_admin_male_pa<-staff_admin_male[which(V11=="PA")]
staff_security_male_pa<-staff_security_male[which(V11=="PA")]
staff_clerical_male_pa<-staff_clerical_male[which(V11=="PA")]
staff_educ_male_pa<-staff_educ_male[which(V11=="PA")]
staff_proftech_male_pa<-staff_proftech_male[which(V11=="PA")]
staff_other_male_pa<-staff_other_male[which(V11=="PA")]
staff_total_male_pa<-staff_total_male[which(V11=="PA")]

t.test(staff_admin_male_pa[which(V23==3)],mu=staff_admin_male[which(V1==390000000074100000000)])
t.test(staff_security_male_pa[which(V23==3)],mu=staff_security_male[which(V1==390000000074100000000)])
t.test(staff_clerical_male_pa[which(V23==3)],mu=staff_clerical_male[which(V1==390000000074100000000)])
t.test(staff_educ_male_pa[which(V23==3)],mu=staff_educ_male[which(V1==390000000074100000000)])
t.test(staff_proftech_male_pa[which(V23==3)],mu=staff_proftech_male[which(V1==390000000074100000000)])
t.test(staff_other_male_pa[which(V23==3)],mu=staff_other_male[which(V1==390000000074100000000)])
t.test(staff_total_male_pa[which(V23==3)],mu=staff_total_male[which(V1==390000000074100000000)])

#Services
services_sum<-rep(0,n)
for(i in 1:n){
  services_sum[i]<-V215[i]+V216[i]+V217[i]+V218[i]+V219[i]+V220[i]+V221[i]+V222[i]+V223[i]+V225[i]+V226[i]+V227[i]+V228[i]+V229[i]+V230[i]+V231[i]+V232[i]+V233[i]
}

t.test(services_sum[which(V23==3)],mu=services_sum[which(V1==390000000074100000000)])