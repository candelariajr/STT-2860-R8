Project 1
STT 2860

```{r}
library(tidyverse)
wpg <- read_excel("wpg.xlsx")
head(wpg)

```

a
```{r}
wpg <- mutate(wpg, `Occupied SF` = `Occupancy Pct` * `Total Center SF`)
wpg[, c(1, 8, 9, 18)]

```

b
```{r}
head(arrange(wpg, desc(`Occupied SF`)))[1:5, c(1, 18)]

```

c
```{r}
unique(wpg$Type)

EnclosedSF <- wpg[wpg$Type == "Enclosed", 9]
head(EnclosedSF)
sum(EnclosedSF)/nrow(EnclosedSF)

OpenAirSF <- wpg[wpg$Type == "Open Air", 9]
head(OpenAirSF)
sum(OpenAirSF)/nrow(OpenAirSF)

```

d
```{r}

## DOESNT WORK

n <- nrow(wpg)
LargeMall <- c()

for (i in 1:n)
{
  if (wpg$Type[i] == "Enclosed" && wpg$`Total Center SF`[i] >= 1000000)
  {
    LargeMall[i] <- "Large Mall"
    
  } else {
    if (wpg$`Total Center SF`[i] >= 500000)
    {
      LargeMall[i] <- "Large Mall"
      
    } else {
      LargeMall[i] <- "Small Mall"
      
    }
    
  }
  
}

wpg <- mutate(wpg, LargeMall)
wpg[, c(1, 9, 11, 19)]

## THIS IS WRONG
   
```

e
```{r}
EnclosedMoneyOwed <- sum(na.omit(wpg[wpg$Type == "Enclosed", 14]))
EnclosedMoneyOwed

OpenAirMoneyOwed <- sum(na.omit(wpg[wpg$Type == "Open Air", 14]))
OpenAirMoneyOwed

```

f
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

g
```{r}
Mortgage <- select(wpg, Name, State, `Principal Balance`)
Mortgage <- Mortgage[is.na(Mortgage$`Principal Balance`) == FALSE, ]
Mortgage

```

h
```{r}
States$OutstandingMortgage <- rep(0, 30)
k <- nrow(States)
m <- nrow(Mortgage)

for (i in 1:k)
{
  for (j in 1:m)
  {
    if (Mortgage$State[j] == States[i, 1])
    {
      States[i, 3] = States[i, 3] + Mortgage$`Principal Balance`[j]
      
    }
    
  }
  
}

States

```
