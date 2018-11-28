#Model a set of marks of students (in the range [0, 10]) with a hierarchical model:

if(!require(data.table)){
    install.packages("data.table", dependencies = TRUE)
    library(data.table)
}
library(rstan)
library(ggplot2)

#Make this example reproducible:
set.seed(19)

#BUILD THE DATA SET OF MARKS:
#Set the general population parameters:
mean_marks<-7
sd_marks<-2

#Generate the mean and sd for each school from the general population distribution:
num_schools<-5
marks<-rnorm(num_schools, mean_marks, sd_marks)

#Generate the marks of the students within each school:
num_students<-100
schools_marks<-sapply(1:num_schools, function(i) rnorm(num_students, marks[i], 0.5))

#Build the final data set:
school_data<-data.table(school_id=rep(1:num_schools, each=num_students), 
                        marks=c(schools_marks[,1],schools_marks[,2],schools_marks[,3],schools_marks[,4],schools_marks[,5]))

#Run the model in Stan with the schools variable to split the marks:
stan_data<-list(
    n_schools=num_schools,
    N=nrow(school_data), 
    school_id=school_data$school_id,
    marks=school_data$marks
)

fit_model<-stan("Example3_marks_HierarchicalModel/schools.stan",
                data=stan_data,
                iter = 4000, chains = 2)
#Rhat is 1, so the model converged
#marks_all and disper_all are close to 7 and 2 (the real values)
#The dispersion of each school offset is also close to 0.5 (the real value)
#Mean mark values for schools are also close to the real ones
print(fit_model) 

#Have a look at the parameters returned from Stan:
fit_df<-as.data.frame(fit_model)
colnames(fit_df) 

# Let's inspect the histogram of the visits to all the values for marks_all
#IMPORTANT: This is NOT the normal distribution for the global population, but it's the bunch of probabilities of all the possible means for the normal holding the normal distribution of the global population
ggplot(data.frame(m_all=fit_df$marks_all), aes(x=m_all))+
    geom_histogram(aes(y=..density..),binwidth=.5, colour="black", fill="white")+
    geom_density(alpha=0.2, fill="#FF6666")

#Get mean point estimates (we want the means because we want to compare them with 
#   the means we fed the normal distributions with to build the fake data set):
mean_genPop<-mean(fit_df$marks_all)
marks_est<-apply(fit_df[, paste0("marks_school[", 1:stan_data$n_schools, "]")], 2, mean)

#Have a look at the errors since we know the true parameter values:
abs(marks-marks_est)


#Plot the results: the resulting normal distributions from the mean values for our parameters
colors_schools<-c("gray40", "seagreen3", "orange3", "dodgerblue3", "mediumpurple3")

#Plot the distribution of the global population:
sd_all<-mean(fit_df$disper_all)
marks_grid<-seq(from=0, to=10, length.out=100)
sams_all<-dnorm(marks_grid, mean_genPop, sd_all)/10
plot3<-ggplot(data.frame(x=marks_grid, y=sams_all), aes(x=x, y=y))+geom_path(color="coral2", lwd=2)+
    xlab("Marks")+ylab("Probability of every mark")+
    ggtitle("Densities of the parameters in our hierarchical model")+
    coord_cartesian(xlim=c(0,10), ylim=c(0,0.1))+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), title = element_text(size = 14))
show(plot3)

#Add vertical lines indicating the real values for the mean mark of each school:
plot3<-plot3+geom_vline(xintercept=marks, col=colors_schools, lty=2, size=1)
show(plot3)

#And plot the distribution of marks per school:
sd_schools<-apply(fit_df[, paste0("disper_school[", 1:stan_data$n_schools, "]")], 2, mean)
data_plot_schools<-mapply(function(x,y){
    dnorm(marks_grid, x, y)/10
    }, marks_est, sd_schools)
invisible(mapply(function(i,c){
    plot3<<-plot3+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools[, i]), mapping = aes(x=x, y=y), col=c, size=1.1)
    }, 1:5, colors_schools))
show(plot3)



#ADD A VARIABLE ON TOP OF SCHOOLS THAT OVERCOMPLICATES THE MODEL AND CAPTURES NO EXTRA VARIABILITY OF OUR MARKS DATA SET:
#REGION cut on top of schools:
#Let's add another level that still preserves the natural division of schools, and adds no extra variation to the model, only extra complexity:
school_data[, region:=ifelse(school_id %in% 1:2, 1, 2)]

stan_data<-list(
    n_schools=num_schools,
    n_regions=school_data[, uniqueN(region)],
    N=nrow(school_data),
    regions_map=c(1, 1, 2, 2, 2), #First two schools in region 1, the other three in region 2
    school_id=school_data$school_id,
    marks=school_data$marks
)

fit_model_region<-stan("Example3_marks_HierarchicalModel/region_school.stan",
                       data=stan_data,
                       iter = 4000, chains = 2)
print(fit_model_region)
fit_df<-as.data.frame(fit_model_region)

#Get point estimates for the global mean and the offset per school:
marks_school_withRegion<-apply(fit_df[, paste0("marks_school[", 1:stan_data$n_schools, "]")], 2, mean)

#Check the errors:
abs(marks-marks_school_withRegion)


#Add the new distributions to the previous plot:
sd_schools_region<-apply(fit_df[, paste0("disper_school[", 1:stan_data$n_schools, "]")], 2, mean)
data_plot_schools<-mapply(function(x,y){ #Instead of getting the normal, find the distribution in the sample in fit_df
    dnorm(marks_grid, x, y)/10
}, marks_school_withRegion, sd_schools_region)

invisible(mapply(function(i,c){
    plot3<<-plot3+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools[, i]), mapping = aes(x=x, y=y), col=c, size=1.1, lty=4)
}, 1:5, colors_schools))
show(plot3)
#These new distributions cannot be seen clearly since they're coincident with the results we already got with the simpler model (only schools division)
#This extra complexity is not adding anything to the model



#ADDING AN EXTRA VARIABLE THAT RUINS THE OUTCOME:
#Socioeconomic class (High, Low) on top of School (SEclass=1 (high) with highest marks, SEclass=2 (low) with lowest marks). 
#   This variable has been shown to be correlated with marks, so it may make sense to add it to our model to try to describe the variability 
#   of the marks better. However, we built this fake data set, and we know this variable has no influence over 
#   marks (maybe a spurious correlation if we're unlucky). Then, this extra variable will lead to a misleading outcome:
median_marks<-school_data[, .(median_school=median(marks)), school_id]
setnames(median_marks, "school_id", "s_id")
school_data[, idObs:=1:nrow(school_data)]
school_data[, SEclass:=ifelse(.SD$marks>=median_marks[s_id==.SD$school_id, median_school], 1, 2), idObs]
school_data[, school_id_doubled:=ifelse(SEclass==1, school_id, school_id+num_schools)]

#Make sure we split the schools into two groups of equal size:
school_data[, .N, school_id_doubled]

stan_data<-list(
    n_schools=school_data[, uniqueN(school_id_doubled)],
    n_SEclasses=school_data[, uniqueN(SEclass)],
    N=nrow(school_data),
    SEclass_map=school_data$SEclass,
    school_id=school_data$school_id_doubled,
    marks=school_data$marks
)

fit_model_SEclass<-stan("Example3_marks_HierarchicalModel/SEclass_school.stan",
                        data=stan_data,
                        iter = 4000, chains = 2)
print(fit_model_SEclass)

#Plot again the distribution of school marks from the initial division:
plot4<-ggplot(data.frame(x=marks_grid, y=sams_all), aes(x=x, y=y))+geom_path(color="coral2", lwd=2)+
    xlab("Marks")+ylab("Probability of every mark")+
    ggtitle("Densities of the parameters in our hierarchical model")+
    coord_cartesian(xlim=c(0,10), ylim=c(0,0.15))+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), title = element_text(size = 14))
show(plot4)

marks_grid<-seq(from=0, to=10, length.out=100)
data_plot_schools<-mapply(function(x,y){
    dnorm(marks_grid, x, y)/10
    }, marks_est, sd_schools)
invisible(mapply(function(i,c){
    plot4<<-plot4+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools[, i]), mapping = aes(x=x, y=y), col=c, size=1, lty=1)
    }, 1:5, colors_schools))
show(plot4)

#Now add the new distributions we found:
fit_df<-as.data.frame(fit_model_SEclass)

#Build back the estimates for each SE class:
means_SEclass<-apply(fit_df[, paste0("marks_SEclass[", 1:stan_data$n_SEclasses,"]")], 2, mean)
means_schoolsOffsets<-apply(fit_df[, paste0("offset_school[", 1:stan_data$n_schools, "]")], 2, mean)
marks_est_SESchool<-rep(means_SEclass, each=num_schools)+means_schoolsOffsets

sd_schools2<-apply(fit_df[, paste0("disper_school[", 1:stan_data$n_schools, "]")], 2, mean)
data_plot_schools_SE<-mapply(function(x,y){
    dnorm(marks_grid, x, y)/10
}, marks_est_SESchool, sd_schools2)

invisible(mapply(function(i,c){
    plot4<<-plot4+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools_SE[, i]), mapping = aes(x=x, y=y), col=c, size=1, lty=4)
    plot4<<-plot4+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools_SE[, i+num_schools]), mapping = aes(x=x, y=y), col=c, size=1, lty=4)
}, 1:5, colors_schools))
show(plot4)


#Let's inspect the problem by viewing just one school:
plot5<-ggplot(data.frame(x=marks_grid, y=data_plot_schools[, 1]), aes(x=x, y=y))+geom_path(color=colors_schools[1], size=1.2)+
    xlab("Marks")+ylab("Probability of every mark")+
    ggtitle("Densities of the parameters in our hierarchical model (School 1)")+
    coord_cartesian(xlim=c(2,7), ylim=c(0,0.13))+
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), title = element_text(size = 14))
plot5<-plot5+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools_SE[, 1]), mapping = aes(x=x, y=y), col=colors_schools[1], size=1, lty=4)
plot5<-plot5+geom_path(data = data.frame(x=marks_grid, y=data_plot_schools_SE[, 1+num_schools]), mapping = aes(x=x, y=y), col=colors_schools[1], size=1, lty=4)
plot5<-plot5+geom_vline(xintercept = marks[1], col="coral2", size=1.1)
show(plot5)
#We can see that Stan made its best to fit 2 normals to split the marks within each school, when the real data is not split (see orange line)
