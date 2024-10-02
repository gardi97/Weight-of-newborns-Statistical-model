---
title: "Modello Statistico per Prevedere il Peso dei Neonati"
author: "Francesco Gardini"
date: "2024-01-12"
output:
  pdf_document: default
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Descrizione del Dataset e obiettivo dello studio

Il dataset oggetto di studio è composto da 2500 osservazioni ciascuna delle quali 
è descritta da 10 variabili: 4 riferite alla madre, 3 al neonato, 1 al tipo di parto e 
1 all'ospedale in cui è avvenuta la nascita.

Questo progetto mira ad indagare le relazioni tra le variabili, sia quelle riferite alla madre sia quelle riferite al neonato, e il peso del nascituro. 
.
```{r}
dati = read.csv('neonati.csv')
attach(dati)
n=nrow(dati)
dati$Tipo.parto = factor(dati$Tipo.parto)
dati$Fumatrici = factor(dati$Fumatrici)
dati$Ospedale = factor(dati$Ospedale)
dati$Sesso = factor(dati$Sesso)
```


### Variabili quantitative
Di seguito sono riportati massimo e minimo, media, mediana, 25° e 75° percentile
per le variabiali quantititative del dataset.
```{r}
var_quant = c('Anni.madre',
              'N.gravidanze',
              'Gestazione',
              'Peso',
              'Lunghezza',
              'Cranio')

summary(dati[,var_quant])
```
```{r}

which(Anni.madre < 10)

dati[which(Anni.madre < 10),]



```
```{r}

dati[c(1152,1380),'Anni.madre'] = mean(Anni.madre)

summary(dati[,var_quant])

```


### Variabili qualitative
Per le variabili qualitative sono state riportate le frequenze assolute e relative delle 
rispettive modalità.

```{r}
ni=table(Fumatrici)
fi=round(ni/n,2)
distr_freq_fum = as.data.frame(cbind(ni,fi))

ni=table(Tipo.parto)
fi=round(ni/n,2)
distr_freq_parto = as.data.frame(cbind(ni,fi))


ni=table(Ospedale)
fi=round(ni/n,2)
distr_freq_osp = as.data.frame(cbind(ni,fi))


ni=table(Sesso)
fi=round(ni/n,2)
distr_freq_sesso = as.data.frame(cbind(ni,fi))

distr_freq_fum
distr_freq_parto
distr_freq_osp
distr_freq_sesso


#library(kableExtra)

#kable(distr_freq_fum, caption = "Fumatrici") %>%
#kable_styling(full_width = FALSE)
  
#kable(distr_freq_parto, caption = "Tipo di Parto") %>%
#kable_styling(full_width = FALSE)
  
#kable(distr_freq_osp, caption = "Ospedale") %>%
#kable_styling(full_width = FALSE)
  
#kable(distr_freq_sesso, caption = "Sesso") %>%
#kable_styling(full_width = FALSE)
```

Si nota come la stragrande maggioranza delle madri (96%) non fumi e che il tipo di parto prevalente è quello naturale (71%). Le altre due variabili che descrivono rispettivemente l'ospedale di provenienza e il sesso si distribuiscono in modo omogeno tra le loro modalità. 

### Variabiabile Peso
Di seguito sono riportati il valore degli indici di forma della variabile peso e il risultato del test di Shapiro, utilizzato per determinare se la variabile si distribuisce in modo normale oppure no.

```{r}
skewness=moments::skewness(dati$Peso)
kurtosi=moments::kurtosis(dati$Peso)-3

skewness
kurtosi

shapiro.test(dati$Peso)
```

Il valore negativo di skewnees indica che la distribuzione presenta un'assimetria negativa con una frquenza maggiore per valori maggiori della media, mentre la kurtosi maggiore di zero è indice di una distribi+uzione leptocurtica. Il p-value del test di Shapiro molto minore di 0.05 porta a rifiutare l'ipotesi nulla di normalità. Tutto ciò è ben visibile nel grafico della denistà di probabilià della variabile Peso riportato di seguito.

Tali considerazione dovranno essere tenute presente durante la costruzione e l'analisi del modello statistico.

```{r}
plot(density(dati$Peso), 
     main = 'Peso neonati [g]',
     xlab = 'Peso [g]')

```

La densità di probabilità della variabile Peso conferma quanto descritto dagli indici, infatti risulta essere asimmetrica e più "appuntita" rispetto alla distribuzione gaussiana.

### Test di verifica di ipotesi

I seguenti test verificano l'ipotesi di uguaglianza tra la media del campione oggetto
di questo studio e quella della popolazione la popalazione per le variabili Peso e lunughezza. Inoltre, si vuole indagare l'ipotesi che tali variabili e quella che descrive la circonferenza del cranio abbiano medie diverse tra maschi e femmine.

I boxplot seguenti mostrano le distribuzioni delle variabili Peso, Lunghezza e Cranio per maschi e femmine.

```{r}
par(mfrow= c(1,3))
boxplot(Peso~Sesso, col=c('pink','blue'))
boxplot(Lunghezza~Sesso, col=c('pink','blue'))
boxplot(Cranio~Sesso, col=c('pink','blue'))
```
Si possono notare differenze tra i due sessi per tutte e tre le variabili, in particolare i maschi mostrano tendenzialmente valori maggiori.

Sono stati svolti dei test di Shapiro per valutare la normalità delle tre variabili così da poter scegliere che tipo di test svolgere.
```{r}
shapiro.test(dati[dati$Sesso=='M',"Peso"])
shapiro.test(dati[dati$Sesso=='F',"Peso"])
shapiro.test(Peso)
```
```{r}
shapiro.test(dati[dati$Sesso=='M',"Lunghezza"])
shapiro.test(dati[dati$Sesso=='F',"Lunghezza"])
shapiro.test(Lunghezza)
```
```{r}
shapiro.test(dati[dati$Sesso=='M',"Cranio"])
shapiro.test(dati[dati$Sesso=='F',"Cranio"])
shapiro.test(Cranio)
```
Dall'esito dei test svolti si può affermare che tutte e tre le variabili non si distribuiscono normalmente sia quando vengono considerate in base al sesso sia quando si considera la distribuzione totale. Quest'osservazione, unita alla presenza di diversi outliers in tutte per tutte e tre le variabili, ha portato alla scelta di un test non parametrico, in particolare quello di Wilcoxon.


Il valore medio del peso del campione è stato confrontato con il valore medio del peso della popolazione. Come valore per la media della popolazione è stato considerato 3,3 Kg, tale valore è in linea con idati relativi alle nascite pubblicati dal MInistero della salute e con quelli consultabili online.

Il valore del p-value maggiore di 0.05 consente di affermare che non c'è una differenza statisticamente significativa tra la media del campione e quella della popolazione.
```{r}
wilcox.test(x = Peso,
       alternative = 'two.sided',
       mu=3300,
       conf.level=0.95)

```

Lo stesso tipo di test è stato condotto sulla variabile lunghezza, in questo caso il valore di riferiemtno della popolazione è stato fissato a 50 cm, trovato online. 
Il p-value snesibilmente minore di 0.05 porta a rifitare l'ipotesi nulla di uguaglianza tra la media del campione in esame e quella della popolazione.

```{r}
wilcox.test(x=Lunghezza,
       alternative = 'two.sided',
       mu=500,
       conf.level=0.95)

```

In seguito, si è valutato se ci fossero differenze significative tra i neonati di sesso maschile e quelli di sesso femminile per quanto rigurda peso, lunghezza e circonferenza del cranio.

I valori di p-value molto minori del livello di significatività fissato a 0.05 permettono di affermare che ci sono differenze significative tra maschi e femmine per tutte e tre le variabili
```{r}
wilcox.test(Peso~Sesso, data = dati)
```

```{r}
wilcox.test(Lunghezza~Sesso, data = dati)
```

```{r}
wilcox.test(Cranio~Sesso, data = dati)
```

Si è indagata la possibile relazione tra il fumo e alcune variabili quali Gestazione, Peso, Lunghezza e Cranio per capire se l'essere fumatrice influisse sulla gravidanza.
Per fare ciò sono stati condotti dei Wilcoxon per capire se ci fossero differenze significative tra i due gruppi.

```{r}
shapiro.test(dati[dati$Fumatrici==1,"Gestazione"])
shapiro.test(dati[dati$Fumatrici==0,"Gestazione"])

shapiro.test(dati[dati$Fumatrici==1,"Peso"])
shapiro.test(dati[dati$Fumatrici==0,"Peso"])

shapiro.test(dati[dati$Fumatrici==1,"Lunghezza"])
shapiro.test(dati[dati$Fumatrici==0,"Lunghezza"])

shapiro.test(dati[dati$Fumatrici==1,"Cranio"])
shapiro.test(dati[dati$Fumatrici==0,"Cranio"])

```
Considerato che la maggior parte delle distribuzioni non sono normali, anche in questo caso, è stato scelto il test di Wilcoxon.

```{r}
wilcox.test(Gestazione~Fumatrici, data = dati)
```

```{r}
wilcox.test(Peso~Fumatrici)
```

```{r}
wilcox.test(Lunghezza~Fumatrici)
```

```{r}
wilcox.test(Cranio~Fumatrici)
```
I boxplot di seguito mostrano le distribuzioni delle variabili Gestazione, Peso, Lunghezza e Cranio per fumatrici e non fumatrici.
```{r}
par(mfrow = c(1,2))
boxplot(Gestazione~Fumatrici, col=c('green','red'), names=c('NO','SI'))
boxplot(Peso~Fumatrici,col=c('green','red'), names=c('NO','SI'))
```

```{r}
par(mfrow = c(1,2))
boxplot(Lunghezza~Fumatrici, col=c('green','red'), names=c('NO','SI'))
boxplot(Cranio~Fumatrici, col=c('green','red'), names=c('NO','SI'))
```
I risultati dei test mostrano che solo per la variabile lunghezza si evidenziano differenze significative tra i due gruppi.


Infine, si è valutata l'ipotesi che la frequenza dei cesari fosse significativamente diversa tra i diversi ospedali. Per fare ciò è stata costruita la tabella di contingenza delle variabili Tipo.Parto e Ospedale e si è valuatata la loro dipendenza mediante il Test Chi-quadrato di Pearson. Il valore del p-value nettamente maggiore di 0.05 porta a non rifiutare l'ipotesi nulla di indipendenza delle due variabili.
```{r}
tab_cont_osp_parto = table(Ospedale, Tipo.parto)
chisq.test(tab_cont_osp_parto)
```
Il boxplot di seguito, a conferma del test, mostra come le frequenze relative delle due modalità della variabile Tipo.Parto siano molto simili tra i tre ospedali.
```{r}
library(ggplot2)
ggplot(data=dati)+
 geom_bar(aes(x=Ospedale, fill=Tipo.parto),
          position = 'fill')+
  theme_classic()+
  labs(fill='Tipo di parto',
       x='Ospedale',
       y='Nascite',
       title = 'Nascite per tipo di parto nei diversi ospedali')
```


# Modello

### Correlazione lineare 
```{r}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}

pairs(dati, upper.panel = panel.smooth, lower.panel = panel.cor)

```

Da una prima analisi della correlazione tra le variabili del dataset e la variabile risposta Peso si può natare che le correlazioni più significative sono con le variabili Lunghezza, Cranio e Gestazione. 
Con queste variabili, dagli scatter plot, è possibile vedere anche una possibile componente non lineare, soprattutto per la variabile Gestazione.


### Modello di regressione con tutte le variabili


```{r}
mod1 = lm(Peso ~., data = dati)
summary(mod1)
```

Si nota come alcune variabili abbiano un valore di p-value maggiore di 0.05 e quindi non contribuiscano in modo sigificativo a spiegare la varianza della variabile risposta Peso. 
Nei modelli successivi tali variabili verranno eliminate così da creare un modello più semplice a 
parità di livello di varianza spiegata.

Dal valore dei coefficienti si evince come quasi tutte le variabili mostrino una correlazione positiva con la variabile esplicativa Peso, ad eccezione della modalità '1' della variabile Fumatrici e della modalità 'osp2' della variabile ospedale. In particolare, la variabile Gestazione è quella che provoca l'incremento maggiore della variabile peso in seguito ad un suo aumento. Da notare il significativo aumento del Peso con la modalità 'M' della variabile Sesso, in linea con il fatto che siano sate evidenziate differenze significative di peso tra maschi e femmine.

```{r}
mod2 = update(mod1, ~.-Anni.madre)
summary(mod2)
```

```{r}
mod3 = update(mod1, ~.-Fumatrici)
summary(mod3)
```
Nel modello 2 è stata eleminata la variabile Anni.madre mentre nel modello 3 Fumatrici, nel modello 4 sono state eliminate entrambe. L'indice R-squared Adhiusted, che corrisponde alla percentuale di varianza spiegata dal modello, non varia tra i 4 modelli.
```{r}
mod4 = update(mod1, ~.-Fumatrici-Anni.madre)
summary(mod4)
```
Il test ANOVA può esssere utilizzato per confrontare due modelli e valutare se ci sono differenze significative in termini di varianza spiegata tra i due. Il p-value nettamente maggiore di 0.05 consente di affermare non ci sono differenze significative tra il modello iniziale (mod1) e il modello in cui non sono state considarate le variabili Anni.madre e Fumatrici (mod4)
```{r}
anova(mod1,mod4)
```
La funzione stepAIC è stata usata per creare un modello utilizzando il metodo di ottimizzazione stepwise in modo automatico. 
```{r}
library(MASS)

mod_stepwise = stepAIC(mod1, 
                       direction = 'both',
                       k = log(n))
```
```{r}
summary(mod_stepwise)
```
Il modello così creato non utilizza le variabili Tipo.parto e ospedale che invece sono presenti nel modello 4.

Gli indici BIC e AIC possono essere utilizzati per scegliere un modello rispetto ad un altro, si sceglie il modello con l'indice minore 
```{r}
df=BIC(mod4, mod_stepwise)
aic = AIC(mod4, mod_stepwise)
df$AIC = aic$AIC
df
anova(mod4, mod_stepwise)
```
L'indice BIC è minore per il modello creato con la funzione automatica mentre il l'AIC è inferiore per il modello selezionato manualmente e contenente più variabili. Considerando che la variabile Tipo.parto risulta significativa così come una delle modalità di quella Ospedale, che il test Anova mostra differenze significative in merito alla varianza spiegata tra i due modelli e che R-squared adjusted è maggiore per il modello scelto manualmente, quest'ultimo è stato preferito.

### Effetti di non linearità

Come accennato in precedenza dagli scatter plot che mettono in relazione le variabili N.gravidanze, Gestazione, Lunghezza, Cranio. Per tenerne conto e provare ad aumentare la 
percentuale di varianza spiegata dal modello, gli effetti quadratici di tali variabili sono stati aggiunti al modello 4.

```{r}

mod5 = update(mod_stepwise, ~.+I(N.gravidanze^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2))
summary(mod5)
```
Come si può notare, la componente quadratica della variabile Cranio non è significativa, inoltre, perde di significatività anche la variabile Cranio, che nel modello di partenza, invece, mostrava un p-value molto minore di 0.05.

Nel modello seguente è stata, quindi, eliminata la componente quadratica della variabile cranio.

```{r}

mod6= update(mod5, ~.-I(Cranio^2))
summary(mod6)
```
In seguito all'eliminazione della sua componente non lineare la variabile cranio è nuovamente significativa, come nel modello di partenza.

Questo modello è stato confrontato con il modello di partenza utilizzando gli stessi criteri descritti in precedenza.

```{r}
df=BIC(mod_stepwise, mod6)
aic = AIC(mod4, mod6)
df$AIC = aic$AIC
df
anova(mod_stepwise, mod6)
```
Anche in questo caso l'indice BIC è inferiore per il modello con meno variabile mentre l'AIC mostra un comportamento opposto. Per quanto riguarda il test ANOVA questo indica un aumento significativo della varianza spiegata del modello che tiene conto degli effetti non lineari rispetto a quello di partenza. Quest'ultimo modello è stato quindi preferito rispetto al modello di partenza.

### Interazioni tra le variabili

All'ultimo modello contente anche gli effetti non lineari di alcune variabili sono stati aggiunti dei termini per tenere conto dei possibili effetti di interazione tra le variabili. 
In particolare si è valutata l'interazione tra le variabili Gestazione, Lunghezza e Cranio.

```{r}
mod7 = update(mod6, ~.+Gestazione*Lunghezza)
summary(mod7)

```
```{r}
mod8 = update(mod6, ~.+Gestazione*Cranio)
summary(mod8)
```
```{r}

mod9 = update(mod6, ~.+Lunghezza*Cranio)
summary(mod9)

```
```{r}
mod10 = update(mod6, ~.+Gestazione*Cranio+Gestazione*Lunghezza+Lunghezza*Cranio)
summary(mod10)
```
L'aggiunta delle interazioni tra queste variabili sia singolarmente che simultaneamente risulta non essere significativa oppure provoca la perdita di significatività di variabili che nel modello di partenza lo erano. Per questa ragione è stato deciso di non introdurre tali interazione nel modello definitivo che corrisponde al modello 8, quello contente anche gli effetti di non linearità.

## Analisi del modello 

### Non collinearità dei residui
Affinchè il modello sia affidabile è importante valutare la non correlazione tra i regressori. L'indice VIF è utile per valutare l'eventuale collinearità dei regressori. Essendo presenti nel modello termini non lineari al posto dell'indice VIF è stato calcolato il GVIF, una forma generalizzata del precedente indice. Si nota come per tutti i regressori il GVIF sia minore di 5 il che indica una scarsa collinearità tra i regressori.



```{r}
library(car)
vif(mod6, type = 'predictor')
```
### Analisi dei residui

Le ipotesi alla base dei modelli di correlazione che riguardano i residui sono:
*Indipendenza: non ci deve essere autocorrelazione tra i residui
*Omoschedasticità: la varianza è costante tra i residui
*Normalità: i residui si devono distribuire secondo la distribuzione normale

Per valutare tale ipotesi si utilizzano i seguenti grafici e appositi test.
```{r}
par(mfrow=c(2,2))
plot(mod6)

```
```{r}
library(lmtest)
bptest(mod6)
dwtest(mod6)
shapiro.test(residuals(mod6))

```


Dal grafico 1 si nota come la maggior parte dei valori previsti sia tra i 2500 e i 4000 grammi, in linea con la distribuzione della variabile peso; sono presenti punti con valori di residuo molto alto e si nota come i valori di residuo maggiore siano per valori alti di peso il che potrebbe essere indice di etoroschedasticità. L'eteorschedasticità è confermata dal Test di Breusch-Pagan il cui valore di p-value molto minore di 0.05 fa rifiutare l'ipotesi nulla di omoschedasticità.

Il grafico 2 consente di valutare l'indipendenza dei residui: non si notano distribuzioni particolari o tendenze il che unito al valore del p-value resistuito dal test di Durbin-Watson è indice dell'assenza di autocorrelazione tra i residui.

Il grafico 3 è utile per studiare la deviazione rispetto alla distribuzione normale dei residui. Si vede come nella parte superiore diversi punti devino rispetto alla retta diagonale il che è sintomo di non normalità dei residui, la non normalità è confermata dal test di Shapiro-Wilk.

Attraverso il grafico si può vedere come ci siano dei punti con valore di leverage significativo e un punto con distanza di Cook vicino a 1.

### Outliers e levareges
Gli outliers sono punti che non sono ben interpolati dal modello, il cui residuo è molto grande. I punti con leverage elevato sono, invece, punti che influenzano fortemente la stima dei coefficienti del modello.
Attraverso la distnza di Cook è possibile capire quanto la rimozione di un determinato punto impatterebbe sulla ridefinizione del modello.

Nel seguente grafico è mostrato il valore del leverage di ciascuna osservazione.

```{r}
lev = hatvalues(mod6)
p=sum(lev)
soglia = 2*p/n
plot(lev, main = 'Leverage')
abline(h=soglia, col=2)
lev[lev>soglia]

n_lev = length(lev[lev>soglia])
n_lev
max(lev)

```
Si nota come ci sia un punto (1551) con leverage particolarmente alto rispetto agli altri, in totale 90 punti hanno un valore maggiore della soglia

Il seguente grafico consente, invece, di identificare gli outliers.

```{r}

plot(rstudent(mod6), main = 'Outliers')
abline(h=c(-2,2),col=2)
out = outlierTest(mod6)
out

```
In particolare sono stati idenficati 5 punti come outlier.

Analizzando il grafico della distanza di Cook si nota come un punto abbia una distanza prossima a 1, mentre tutti gli altri sono al di sotto del valore di 0.5.
```{r}
cook = cooks.distance(mod6)
plot(cook, main = 'Distanza di Cook')
cook[cook>0.5]
max(cook)

```

```{r}
lev_oss = which(lev>soglia)
out_oss = as.numeric(attributes(out$rstudent)$names)

intersect(lev_oss,out_oss)
```

Si nota, in particolare, come le osservazioni 155 e 1551 siano sia outliers che leverages; l'osservazione 1551 inoltre è anche quella con il valore della distanza di Cook maggiore e prossimo a 1.

```{r}
dati[c(1551,155),]
```
```{r}
library(ggplot2)
ggplot()+
  geom_point(aes(dati[dati$Sesso=='F','Lunghezza'], dati[dati$Sesso=='F','Peso']),
             position = 'jitter')+
  theme_classic()+
  scale_x_continuous(breaks = seq(0,1000,50))+
  scale_y_continuous(breaks = seq(0,6000,250))+
  labs(
    x = 'Lunghezza [mm]',
    y = 'Peso [g]',
    title = 'Scatter Plot Lunghezza-Peso, Sesso: F'
  )
```
Osservando i valori della variabile per l'osservazione si nota come il valore del peso di 4370 g sia molto elevato rispetto alla lunghezza di 315 mm. Tale anomalia è facilmente identificabile anche sullo scatter plot mostato, in cui si vede che valori di lunnghezza nell'ordine di quella dell'osservazione 1551 corrispondono a un peso intorno ai 1000 g, in linea con il valore predetto dal modello. 


### Un nuovo modello senza l'osservazione 1551

Fatte queste considerazioni e ricordando che l'osservazione 1551 ha un valore di leverage molto elevato così come quello della distanza di Cook si è provato a eliminarla dal dataset e creare un nuovo modello con le stesse variabili del modello 6, ritenuto finora il modello migliore.



```{r}
new_data = dati[-c(1551),]
row.names(new_data) = NULL

mod_no_1551 = lm(Peso ~.-Anni.madre-Fumatrici-Ospedale-Tipo.parto+I(Gestazione^2)+I(N.gravidanze^2)+I(Lunghezza^2),
                 data = new_data)

summary(mod_no_1551)
```
```{r}
vif(mod_no_1551, type = 'predictor')
```
Anche per questo modello l'indice GVIF è inferiore a 5 per tutti i regressori quindi non è presente una collinearità tra regeressori preoccupante.

```{r}
library(lmtest)
bptest(mod_no_1551)
dwtest(mod_no_1551)
shapiro.test(residuals(mod_no_1551))
```
```{r}
par(mfrow=c(2,2))
plot(mod_no_1551)
```
Come si può vedere dai grafici e dai valori dei test è stata eliminata l'eteroschedasticità presente nel modello precedente e continua a non essere presente autocorrelazione tra i residui, tuttavia i residui continuano a non distriurisi secondo la distribuzione gaussiana, come mostrato dal Q-Q plot e dal test di Shapiro.

```{r}

lev = hatvalues(mod_no_1551)
p=sum(lev)
soglia = 2*p/n
plot(lev, main = 'Leverages')
abline(h=soglia, col=2)
lev[lev>soglia]

n_lev = length(lev[lev>soglia])
n_lev
max(lev)

plot(rstudent(mod_no_1551), main = 'Outliers')
abline(h=c(-2,2),col=2)
out = outlierTest(mod_no_1551)
out

cook = cooks.distance(mod_no_1551)
plot(cook, main = 'Distanza di Cook')
cook[cook>0.5]
max(cook)

lev_oss = which(lev>soglia)
out_oss = as.numeric(attributes(out$rstudent)$names)

intersect(lev_oss,out_oss)

```

Per quanto riguarda il numero di punti di leverage e outliers con valore sopra la soglia, questo è molto simile al modello contente l'osservazione 1551, tuttavia non sono presenti osservazioni con distanza di Cook maggiore di 0.5. L'osservazione 155 risulta essere sia un punto con leverage sopra la soglia sia un outlier, tuttavia considerando che la sua distanza di Cook è nettamente inferiore a 0.05 si è deciso di continuare a considerarlo nel modello.

```{r}
df=BIC(mod6, mod_no_1551)
aic = AIC(mod6, mod_no_1551)
df$AIC = aic$AIC
df

```
Infine, confrontando i due modelli utilizzando gli indici BIC e AIC, come fatto con i precedenti modelli, si nota come questi risultino inferiori nel modello senza l'osservazione 1551 che quindi risulta migliore del modello che la contiene (mod6)


### Conclusioni e considerazioni finali sul modello prodotto
In conclusione, la combinazioni di variabili del modello 6 a cui si è giunti dopo aver realizzato e confrontanto tra loro diveris modelli lineari si è dimostrata in grado di spiegare circa il 74% della varianza della variabile Peso. Tuttavia, in questo modello è stata evidenziata eteroschedasticità e non normalità dei residui. Analizzando gli outliers e i valori di leverage è stato possibile individuare l'osservazione 1551 come outlier molto influente sul modello. La sua eliminazione, a parità di variabili utilizzate, ha permesso di eliminare l'eteroschedasticità dei residui e di aumentare leggermente la percentuale di varianza del peso spiegata dal modello. Tuttavia, permane la non normalità dei residui, questo potrebbe anche essere spiegato dalla non normalità della variabile esplicativa. Per queste ragioni, sebbene la percentuale di varianza spiegata dal modello sia più che accettabile, sarebbe opportuno utillarlo con estrema cautela per fare previsioni. Si potrebbe, inoltre, provare ad utilizzare altri tipi di modelli più adatti per variabili che non si distribuiscono normalmente


Rappresentare questo modello in un unico grafico è pressochè impossibile condierato il numero di regressori da cui è formato. Per dare un'idea delle relazioni tra i singoli regressori utilizzati e la variabile esplicativa Peso sono stati realizzati 4 scatter plot, uno per ogni variabile quantitativa utilizzata nel modello.
 
```{r}
tit = paste('Cor: ',as.character(round(cor(new_data$Peso, new_data$Lunghezza),2)))
g_lun= ggplot(data = new_data)+
  geom_point(aes(Lunghezza,Peso, col=Sesso), pch = 20, position = 'jitter')+
  geom_smooth(aes(Lunghezza,Peso, col=Sesso), method = 'lm')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,6000,500))+
  scale_x_continuous(breaks = seq(0,600,50))+
  labs(x = 'Lunghezza [mm]',
       y = 'Peso [g]',
       title = tit)

tit = paste('Cor: ',as.character(round(cor(new_data$Peso, new_data$Cranio),2)))
g_cra=ggplot(data = new_data)+
  geom_point(aes(Cranio,Peso, col=Sesso), pch = 20, position = 'jitter')+
  geom_smooth(aes(Cranio,Peso, col=Sesso), method = 'lm')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,6000,500))+
  scale_x_continuous(breaks = seq(0,600,50))+
  labs(x = 'Cranio [mm]',
       y = 'Peso [g]',
       title = tit)

tit = paste('Cor: ',as.character(round(cor(new_data$Peso, new_data$N.gravidanze),2)))
g_grav=ggplot(data = new_data)+
  geom_point(aes(N.gravidanze,Peso, col=Sesso), pch = 20, position = 'jitter')+
  geom_smooth(aes(N.gravidanze,Peso, col=Sesso), method = 'lm')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,6000,500))+
  scale_x_continuous(breaks = seq(0,15,1))+
  labs(x = 'Numero di gravidanze',
       y = 'Peso [g]',
       title = tit)

tit = paste('Cor: ',as.character(round(cor(new_data$Peso, new_data$Gestazione),2)))
g_gest=ggplot(data = new_data)+
  geom_point(aes(Gestazione,Peso, col=Sesso), pch = 20, position = 'jitter')+
  geom_smooth(aes(Gestazione,Peso, col=Sesso), method = 'lm')+
  theme_classic()+
  scale_y_continuous(breaks = seq(0,6000,500))+
  scale_x_continuous(breaks = seq(0,50,2))+
  labs(x = 'Settimane Gestazione',
       y = 'Peso [g]',
       title = tit)

library(gridExtra)
grid.arrange(g_lun,g_cra,g_gest,g_grav, nrow=2,ncol=2)
```

In ogni scatter plot è rappresentata la divisione per Sesso, visto che sia dall'analisi dei coefficienti del modello sia dai test di ipotesi svolti, si è notato come le variabili mostrino differenze signifative tra maschi e femmine. Per ciascuna variabile, inoltre, è stato riportato il valore del coefficiente di correlazione lineare con il peso, si nota come la lunghezza sia la variabile con la correlazione maggiore, pari a 0.81. Il numero di gravidanze non mostra correlazione con il peso, tuttavia, dall'analisi dei coefficienti questa variabile è risultata significativa per cui è stata considerata nel modello.

## Modello di previsione Peso

Si vuole fare una previsione del peso di una neonata conoscendo la settimana in cui partirirà la madre e il numero di gravidanze. 
E' stato quindi creato un nuovo modello che mettesse in relazione la variabile Peso con le variabili N.gravidanze, Gestazione e Sesso.

Si è partiti considerando esclusivamente gli effetti lineari per poi aggiungere termini quadratici e termini di interazione tra le variabili.
```{r}
mod_anni_sett = lm(Peso ~ N.gravidanze + Gestazione + Sesso)
summary(mod_anni_sett)

mod_anni_sett_1 = update(mod_anni_sett, ~.+ I(Gestazione^2)+I(N.gravidanze^2))
summary(mod_anni_sett_1)

mod_anni_sett_2 = update(mod_anni_sett_1, ~.+ N.gravidanze*Gestazione + 
                           N.gravidanze*Sesso + Gestazione*Sesso)
summary(mod_anni_sett_2)

mod_anni_sett_3 = update(mod_anni_sett_1, ~.+ N.gravidanze*Gestazione)
summary(mod_anni_sett_3)

mod_anni_sett_4 = update(mod_anni_sett_1, ~.+ N.gravidanze*Sesso)
summary(mod_anni_sett_4)

mod_anni_sett_5 = update(mod_anni_sett_1, ~.+ Sesso*Gestazione)
summary(mod_anni_sett_5)
```
```{r}
df=BIC(mod_anni_sett, mod_anni_sett_1)
aic = AIC(mod_anni_sett, mod_anni_sett_1)
df$AIC = aic$AIC
df
anova(mod_anni_sett, mod_anni_sett_1)
```
Confrantando il primo modello con quello in cui sono state aggiunte i termini quadratici si nota come quest'ultimo abbia un R-squared adjusted maggiore e indici BIC e AIC minori. Queste considerazioni unite al p-value restituito dal test ANOVA hanno fatto prefirire il secondo modello al primo.
L'aggiunta dei termini di interazione non ha portato buoni risultati in quanto questi sono spesso risultati non significativi oppure hanno fatto perdere la significatività a variabili che nel modello di partenza ce l'avevano. Per questa ragione è stato scelto di non includere termini di interazione nel modello utilizzato per la previsione.

```{r}
pred = data.frame( N.gravidanze = 3,
                   Gestazione = 39,
                   Sesso = 'F')
round(predict(mod_anni_sett_1,newdata = pred),0)
```
Il peso predetto per una neonata la cui madre è alla terza gravidanza e partorirà alla 39° settimana è di 3297g. Il modello, tuttavia, riesce a spigare solo circa il 40% della varianza della variabile peso per cui non è molto adatto per fornire una previsione accurata.