---
title: "Pre-requisites for Bayesian Hierarchical Modelling course"
author: "Andrew Parnell"
output: html_document
---

In preparation for the course please install the following, preferably in the below suggested order. Make sure you run these as soon as possible to avoid falling behind. JAGS and Stan are not required until day 2 so if you are having problems we can install together on the afternoon of the first day.

Remember you will need your own personal computer with administrator access for the duration of the course and a good internet connection.

As this module will be delivered online please install [Zoom](https://www.zoom.us) and [Slack](https://slack.com) to access the videos and interactive components of the course. All the Zoom links to the meeting will be posted to the Slack `#General` channel.

### Step 1

Install the following using the corresponding links

-	R: [http://www.r-project.org](http://www.r-project.org)

-	Rstudio (optional but recommended): [https://www.rstudio.com](https://www.rstudio.com)

-	JAGS: http://sourceforge.net/projects/mcmc-jags/files/

- If you're using Windows you also need to install the correct version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to match your R version

### Step 2

Now install the following packages by going into Rstudio (or R) by typing:
```{r,eval=FALSE}
install.packages(c('rjags','R2jags','rmutil','boot'))
install.packages('rstan', dependencies = TRUE)
```

Note: this might take a while as these are pretty big and complicated packages.

Once you have installed these I would recommend checking the [rstan installation guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) to check everything is working

### Troubleshooting

If you run into any problems please drop me a line at <andrew.parnell@mu.ie>.

