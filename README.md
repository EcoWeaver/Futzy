# Futzy
 
## Overview
This is a Shiny app written for R that enables the creation of fuzzy cognitive maps. A fuzzy cognitive map is a decision-making tool that models causal influences between nodes over time.

## Install
Required packages: 
- Shiny
- bslib
- shinyFeedback
- DiagrammeR
- iGraph
- tidyverse
- visNetwork
- reshape2
- ggplot2

Download both R files and run them using [RStudio](https://posit.co/download/rstudio-desktop/).

## A Short Reading List
- [Fuzzy cognitive mapping as a tool to assess the relative cumulative effects of environmental stressors on an Arctic seabird population to identify conservation action and research priorities](https://besjournals.onlinelibrary.wiley.com/doi/10.1002/2688-8319.12241) - Rooney et al. 2023
- [Preventing a series of unfortunate events: Using qualitative models to improve conservation](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.14231) - Clark-Wolf et al. 2022
- [Ecological models based on people’s knowledge: a multi-step fuzzy cognitive mapping approach](https://linkinghub.elsevier.com/retrieve/pii/S030438000300543X) - Özesmi & Özesmi 2004

## Usage

## Current tasks
### Basic dev stuff
- DEBUG and refine code!
- Add ability to delete & modify node or edge
- Incorporate graph statistics into model analysis (e.g. degree, centrality)
- Include more information about connections when a node or edge is selected.

### More theoretical things
- Investigate connecting a node to an edge such that changes in the node influence the degree on the edge.
- Expand functionality to allow for the combining and analysis of fuzzy cognitive maps such as those drawn by participants.
- Add extensions from [Giabbaneli and Napoles 2024](https://link.springer.com/10.1007/978-3-031-48963-1)
- Allow for importing graphs from spreadsheets arranged like this:  variable 1/connection/variable 2/polarity/weight/comment variable 1/comment variable 2/comment variable3/other indicator of dependance as time or space
- Filter variables based on space or time differences
- Investigate adding "[transitive closure](https://www.cs.sfu.ca/~ggbaker/zju/math/closures.html)" function