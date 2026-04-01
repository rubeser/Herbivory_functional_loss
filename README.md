# Herbivore Functional Loss - Pleistocene to Future

This project analyzes the functional trait space of large herbivores in the Basque Country and Navarra (Spain) across three temporal and management scenarios:
1. **Last Interglacial (LIG)**: Pleistocene megafauna baseline.
2. **Present**: Current community including wild and domestic species.
3. **Abandonment**: Projected community following the hypothetical withdrawal of livestock and active management.

## Project Structure

- `data/`: Contains the cleaned functional trait database (`species_updated.xlsx`).
- `scripts/`: R scripts for analysis:
  - `01_famd_core.R`: Factor Analysis of Mixed Data (FAMD) and trait significance.
  - `02_clustering.R`: Functional group clustering.
  - `03_scenario_analysis.R`: Comparison of functional space across scenarios.
  - `04_conservation_exclusivity.R`: Analysis of functional exclusivity and conservation proxies.
  - `export_famd_table.R`: Utility to export results.
- `outputs/`: Directory where analysis results and figures are saved.

## Getting Started

1. Open `Herbivory_functional_loss.Rproj` in RStudio.
2. Run the scripts in the `scripts/` folder sequentially (01 to 04).

## Data

The input data `species_updated.xlsx` includes the following traits for each species:
- Body Mass (`Mass.g`)
- Fermentation Type (`Fermentation.Type`)
- Feeding Guild (`Guild.w.Omnivory`)
- Water Dependence (`water_dependence`)
- Group Behaviour (`group_behaviour`)

And presence/absence for each scenario:
- `last_interglacial`
- `present`
- `abandonment`
