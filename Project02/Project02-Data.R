# -------------------------------------------------#
#                   Project 02                     #
#           STA404 - Ethan Gutknecht               #   
#--------------------------------------------------#

# Ready Excel Spreadsheets
setwd("C:/Users/Gutkn/Documents/STA404/Project02")
highSchoolCompletion <- read.csv('HighSchoolCompletion.csv')

# Data Clean For Education Data
highSchoolCompletion$Black <- as.double(highSchoolCompletion$Black)
highSchoolCompletion$White <- as.double(highSchoolCompletion$White)
highSchoolCompletion$Hispanic <- as.double(highSchoolCompletion$Hispanic)
highSchoolCompletion$Asian <- as.double(highSchoolCompletion$Asian)
highSchoolCompletion$Two.or.more.races <- as.double(highSchoolCompletion$Two.or.more.races)
highSchoolCompletion <- highSchoolCompletion %>%
  pivot_longer(cols=c('Total', 'White', 'Black', 
                      'Hispanic', 'Asian','Two.or.more.races'),
               names_to='Race',
               values_to='Percentage')

highSchoolCompletion$Race <- as.factor(highSchoolCompletion$Race)
levels(highSchoolCompletion$Race) <- list("Total Population" = "Total",
                                          "White" = "White",
                                          "Black" = "Black",
                                          "Hispanic" = "Hispanic",
                                          "Asian" = "Asian",
                                          "Two Or More Races" = "Two.or.more.races")


# Data Clean for Salary Data
medianAnualEarnings <- read.csv('MedianAnnualEarnings.csv')
medianAnualEarnings <- medianAnualEarnings %>%
  pivot_longer(cols=c('Less.than.9th.grade',
                      'Some.high.school..no.completion',
                      'High.school.completion..includes.equivalency.', 
                      'Some.college..no.degree',
                      'Associate.s.degree',
                      'Bachelor.s.degree.5.',
                      'Master.s.degree',
                      'Professional.degree',
                      'Doctor.s.degree'),
               names_to='EducationLevel',
               values_to='Salary')

medianAnualEarnings$Salary <- as.numeric(gsub("\\,", "", medianAnualEarnings$Salary))

medianAnualEarnings$EducationLevel <- as.factor(medianAnualEarnings$EducationLevel)
levels(medianAnualEarnings$EducationLevel) <- list("9th Grade" = "Less.than.9th.grade",
                                                     "Some Highschool" = "Some.high.school..no.completion",
                                                     "High School" = "High.school.completion..includes.equivalency.",
                                                     "Some College" = "Some.college..no.degree", 
                                                     "Associate's Degree" = "Associate.s.degree", 
                                                     "Bachelor's Degree" = "Bachelor.s.degree.5.", 
                                                     "Master's Degree" = "Master.s.degree", 
                                                     "Doctor's Degree" = "Doctor.s.degree",
                                                     "Progressional Degree" = "Professional.degree")

# Save Data To File
save(medianAnualEarnings, file = "MedianAnnualEarnings.RData")
save(highSchoolCompletion, file = "highSchoolCompletion.RData")


