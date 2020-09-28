---
title: "2860 Group Project 1"
author: "Connor Briggs, Allison Basili, Jonathan Candelaria"
date: "9/27/2020"
output: html_document
---

```{r setup, include=FALSE}
library(tidyverse)
library(readxl)
wpg <- read_excel("wpg.xlsx")
head(wpg)

```

##A
```{r}
wpg <- mutate(wpg, `Occupied SF` = `Occupancy Pct` * `Total Center SF`)
wpg[, c(1, 8, 9, 18)]

```


##B
```{r}
head(arrange(wpg, desc(`Occupied SF`)))[1:5, c(1, 18)]

```

##C
```{r}
unique(wpg$Type)

EnclosedSF <- wpg[wpg$Type == "Enclosed", 9]
sum(EnclosedSF)/nrow(EnclosedSF)

OpenAirSF <- wpg[wpg$Type == "Open Air", 9]
sum(OpenAirSF)/nrow(OpenAirSF)

```
On average, enclosed malls are larger.


##D
```{r}

n <- nrow(wpg)
LargeMall <- c()

for (i in 1:n)
{
  if (wpg$Type[i] == "Open Air" && wpg$`Total Center SF`[i] >= 500000)
  {
    LargeMall[i] <- "Large Mall"
   
  } else {
    if (wpg$Type[i] == "Enclosed" && wpg$`Total Center SF`[i] >= 1000000)
    {
      LargeMall[i] <- "Large Mall"
     
    } else {
      LargeMall[i] <- "Small Mall"
     
    }
   
  }
 
}

wpg <- mutate(wpg, "LargeMall" = LargeMall)

wpg[c(1, 9, 11, 19)]

   
```

##E
```{r}

EnclosedMoneyOwed <- sum(na.omit(wpg[wpg$Type == "Enclosed", 14]))
EnclosedMoneyOwed

OpenAirMoneyOwed <- sum(na.omit(wpg[wpg$Type == "Open Air", 14]))
OpenAirMoneyOwed

```
Compared to enclosed properties, open air porperties owe $1,668,125 less.


##F
```{r}
States <- unique(wpg$State)
k <- length(States)
n <- nrow(wpg)

States <- as.tibble(States)
States$Median <- rep(0, k)

for (i in 1:k)
{
  size <- 0
  count <- 0
  for (j in 1:n)
  {
    if (wpg$State[j] == States[i, 1])
    {
      size = size + wpg[j, 9]
      count = count +1
     
    }
   
  }
  States[i, 2] = size / count
 
}

States

```
