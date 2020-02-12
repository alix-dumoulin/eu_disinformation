# Disinfo Horizon: Responding to Future Threats

Hackathon on disinformation, Brussels 29-30 January.

## Background

This github repo is contains research and output of the EU vs Disinformation hackathon. Alongside the "Disinfo Horizon: Responding to Future Threats" conference, the East StratCom Task Force (ESTF) hosted a hackathon to explore ways to leverage the EUvsDisinfo Disinfo Database.

## The data

The database contains over 7000 disinformation cases and debunks. The data was collected over the last 5 years, analysed, manually labelled and categorised. It is now available via API at http://api.veedoo.io.

## Ideas

* A network analysis of the meta data to gain a broad idea of what the data is. This might include network analysis of organisations and the keywords they share.

* Based on word embedding and other quantitative text analysis methods, the user can explore word similarity and frequency in the data. This might include a search tool for a word which would output the other similar words and plot this.

* An explorer to better understand the data and get the big picture. Includes searching organisations to better understand what their share, when to whom etc.

## Implementation

The Shiny dashboard is available here: https://alixdumoulin.shinyapps.io/disinformation_hackathon/

The code is in the `shiny-dashboard` repo and can be downloaded. 

## How to run the shiny locally?

1) Clone the `shiny-dashboard` repo

Make sure all the datafiles and the app.R remain in the same folder. 

2) Download all packages specified in the `requirement.txt`file

```{r}
install.packages("package")
```

3) Run the code

Either press the `Run App` button on the RStudio interface or run the whole file. The last command runs the shiny app in your browser locally:

```{r}
shinyApp(ui, server)
```
