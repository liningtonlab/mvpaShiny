# Define global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
# 50MB upload limit
options(shiny.maxRequestSize=50*1024^2)
