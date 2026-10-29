# project2_shinyapp-
Project2 for ST558 

This is a project based on O*Net data to describe the relationship between occupations characteristics and their required skills scores.
O*net is dataset about the occupations and their characteristics in the U.S. Based on my project about the relationship between occupations and skills required by occupations, I chose this dataset including the major types of occupations (occupation group, preparation level), occupation codes and titles, 35 occupation skills, education level required by the occupations, and the skill dimension I extarcted before. Specifically, for each skill, we have a score for their importance in this occupation from 0-5 (continuous variables), and other variables are categorical variables. I chose 3 skills in each factor dimension, so we have 12 skills in total. 
Specifically, we have:
  1. occupation unique codes and titles 
  2. job zone (the preparation level needed before people enter these occupations)
  3. occupation groups (the major type of occupations)
  4. scores on skill dimensions
  5. scores on 12 specific skills
  
Specifically, this project created
  1. a static description/summary/visualization of occupation types and skill scores. 
  2. a shinyApp for audience to interact with the visualization. 
  
This ShinyApp lets users explore the skills that U.S. occupations rely on, using data from the O*NET 31.0 Database (2026). 
How to use: Users can subset the data by occupation group, preparation level, skill type, and skill scores, then view and download the data, or make tables and plots.
Sidebar: choose subsets of the data, then click 'Apply Filters'.,
Data Download: view the data you chose(you subsetted), and save it as a csv. file.
Data Exploration: one-way and two ways contingency tables, numerical summaries, and different types of plots, depending on which variables you chose. Attention: The heatmap shows the average of the selected numeric variable by occupation group and preparation level, which are fixed.
