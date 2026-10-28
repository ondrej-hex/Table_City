# Table_City

A project about simulating a virtual city in R!\

<br><br>

# Current simulation models roadmap

## Spatial
### City matrix
Each cell is a space devoted to either Residential / Commercial / Industrial / Public spaces.

### Expansion
**TBD** (so far perfectly meets demand and import needs; idea: Score maps (over city matrix) for commercial value, residential value, industrial value, etc… Affected by proximity to the city center, other specific areas / success values, etc…)



## Agentic
### Population
Birth rates (5% birth chance per person)\
Aging (75+ years: increasing chance of death)\
Homelessness and joblessness\
Moving in/out of city

### Buildings lifecycle
**TBD** (building value, position, maintenance -> efficiency -> income, …)\
(Ideas: When stagnating -> improper maintenance -> demolition)


## Economic
### Supply/demand
Citizens increase demand\
Commercial spaces produce supply (while importing goods)

### Import/export
Commercial spaces import goods\
Industrial spaces export goods (imbalance solved through city trade)

### Capital
**TBD** (idea: every person/building and the city has capital, spending, income, maintenance => efficiency, etc…)\
(Ideas: municipal capital (taxes(based on land value??), costs, etc…); capital loops (industry export -> commercial selling goods <-> citizen wages, money spending), etc…)\
(Idea: residential value <- sum(resident wages); every building has its economic value rather than direct capital; this shows on score maps and thus commercial zones near rich buildings have more value, no need for costly capital simulation)


## QoL
### Happiness
**TBD** (idea: increases income/efficiency or something; improved by education(TBD), work, city services, goods, etc…)

### City services / utilities / public spaces / …
**TBD** (public services paid by the city either necessary or for specific benefits)\
(Ideas: Electricity, water, waste, town hall, parks, landmarks?, schools,  …)



