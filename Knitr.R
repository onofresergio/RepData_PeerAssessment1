require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('PA1_template.rmd', 'PA1_template.md') # creates md file
markdownToHTML('PA1_template.md', 'PA1_template.html') # creates html file
