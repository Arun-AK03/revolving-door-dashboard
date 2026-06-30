# The Revolving Door — Recurring Homelessness in NSW

An interactive **R Shiny dashboard** exploring why people in Australia return to homelessness even after being housed. It turns fragmented government data into a clear narrative: who is most at risk of re-entry, how long it takes, and which triggers — like domestic violence or financial stress — raise the risk.
`r`, `shiny`, `data-visualization`, `dashboard`, `homelessness`, `ozmaps`, `open-data`.

### 🔗 Live dashboard
**[gokul-venugopal.shinyapps.io/Revolving_Door_App](https://gokul-venugopal.shinyapps.io/Revolving_Door_App/)**

> University team project — UNSW, DATA5002. Built with public, open-government data.

## The problem
In NSW, roughly **11% of people housed by homelessness services return to homelessness within a year.** Homelessness is often a cycle, not a one-off event — but that pattern is hidden across separate spreadsheets, reports, and PDFs. This dashboard makes the cycle visible so policymakers, analysts, and journalists can act on it.

## What the dashboard does
- **Overview KPIs** — 12-month return rate, median return time, average clients, housed cohort.
- **Pathways & demographics** — a Sankey flow (Housed → Still Housed → Returned) and monthly returns broken down by reason.
- **Return timeline** — how returns trend over time.
- **Risk & response** — an alluvial view (Reason → Assistance → Outcome) and a homelessness-rate map of Australia by demographic, built with `ozmaps`.
- **Intervention simulator + risk calculator** — enter client characteristics to see predicted re-entry risk and the top drivers, alongside model coefficients.
- **Policy section** — turning the findings into recommendations.

## Data sources
- **AIHW** — Specialist Homelessness Services: monthly data (Jul 2017 – Jun 2025) covering clients, client groups, and reasons; plus the indicators tables on returns to homelessness.
- **Data.NSW (Dept. of Communities & Justice)** — clients who are housed and then return to homelessness (2018–2024).
- **ABS** — Estimating Homelessness, Census 2021 (rates and demographics by state).

All sources are public open data. See the proposal and report in `/docs` for full descriptions and citations.

## Tech stack
R · Shiny · shinydashboard · plotly · leaflet · ozmaps · dplyr · tidyr · ggplot2 · lubridate

## Repository structure
```
revolving-door-dashboard/
├── README.md
├── app.R                 # the Shiny app (rename from Latestcode.R)
├── Data/                 # CSV inputs, in the folder layout the app expects
```

## Run it locally
```r
# from the project root, in R/RStudio:
install.packages(c("shiny","shinydashboard","plotly","dplyr","readr",
                   "ggplot2","lubridate","scales","tidyr","leaflet","ozmaps"))
shiny::runApp()
```


## Challenges & learnings
The original plan used a heat-correlation map, but the data didn't support it, so the team pivoted to a geographic view of Australia using `ozmaps` — which meant matching state names across datasets and linking each state to its sub-indices. Early single-page layouts felt cluttered, so the design moved to a scrolling, sectioned dashboard for clarity. (More detail in `/docs`.)

## My role
Part of the project team. I built the interactive ozmaps choropleth — clicking any Australian state surfaces its demographic breakdown — and co-developed the intervention simulator. I also led data sourcing and preprocessing, cleaning and aligning the AIHW, ABS, and Data.NSW datasets into the structure the dashboard consumes.

## Author
**Arun Kumar Selvaraj** — MSc Business Data Science & Decisions, UNSW
📧 arunsp2003@gmail.com
