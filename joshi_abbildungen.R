```{r Häufigkeit Wochentage, echo=FALSE, include=FALSE}
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library

Fibo1_2 <- rbind(Fibo1,Fibo2)
Fibo1_2 <- Fibo1_2[(which(duplicated(Fibo1_2))),]
nrow(Fibo1_2)
plot1 <- gf_bar(~ Wochentag, data = Fibo1_2, title = "Fibonacci 1 & 2")

Fibo21_34 <- rbind(Fibo21,Fibo34)
Fibo21_34 <- Fibo21_34[(which(duplicated(Fibo21_34))),]
nrow(Fibo21_34)
plot2 <- gf_bar(~ Wochentag, data = Fibo21_34, title = "Fibonacci 21 & 34")
```


# Abb. 1


```{r fig.align="left", fig.cap="Die Fibonacci Zahlen 1, 2, 21 und 34 werden samstags am häufigsten mit ihren Goldenen-Schnitt Zahlen gezogen.", default= NULL,fig.height=3, fig.width=8}
grid.arrange(plot1,plot2,ncol=2)
```

#Abb. 2

Die Bootstrapverteilung der Fibonacci-Zahlenpaare 21 und 34 kann anhand Abbildung XY visualisiert werden.
```{r Bootstrap_21_34, fig.cap =  "In 25% der Stichprobenverteilungen des Bootstrap Resamplings wurden die Fibonacci-Zahlen 21&34 37 mal gezogen und in 75% der Stichprobenverteilungen 64 mal (schwarze Linien). Der p-Wert der Originalstichprobe weist eine Ziehungsanzahl von 63 auf (rote Linie) Dieser Wert liegt noch innerhalb der Quantile und stellt somit einen plausiblen Wert dar. Die Hypothese, dass die Fibonacci-Zahlen 21&34 am häufgsten mit ihren goldenen Schnitt Zahlen gezogen werden kann hiermit verworfen werden.", echo=FALSE}
plot(Bootsvertlg_21_34,title = "Bootsverteilung der Fibonacci Zahlenpaare 21&34")
```
