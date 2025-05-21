# Aneuploidy Rates Simulation

This is a repository for the code used to produce data in the paper...

## File Explanation

The `data/` folder contains generated data organized by date. Below is an overview of the contents:

### Folders

The main data files supporting the paper are:

-   **04-04c, 04-04d, 04-04e**\
    Contains 3000 ABC_seq Lenormand data for [**Capalbo**](https://doi.org/10.1016/j.ajhg.2021.11.002).

-   **04-08c, 04-08d, 04-08e**\
    Contains 3000 ABC_seq Lenormand data for [**Munne 2017**](https://doi.org/10.1016/j.fertnstert.2017.05.002).

-   **04-16c, 04-16d, 04-16e**\
    Contains 3000 ABC_seq Lenormand data for [**Walters-Sen**](https://doi.org/10.1016/j.rbmo.2021.06.022).

-   **04-18c, 04-18d, 04-18e**\
    Contains 3000 ABC_seq Lenormand data for [**Rodrigo**](https://doi.org/10.3390/genes11101151).

-   **04-19c, 04-19d, 04-19e**\
    Contains 3000 ABC_seq Lenormand data for [**Clarke**](https://doi.org/10.1007/s10815-023-02941-6).

-   **04-21**\
    Includes misdiagnosed rates applied to expected values with dispersal levels of 0, 0.5, and 1 for **Capalbo**.

-   **04-22**\
    Contains generated embryos based on distributions from `04-21`.

## Usage

### `sequential_abc.R`

Implements the sequential ABC algorithm, estimating posterior distributions of meiotic and mitotic error rates by comparing simulated and observed data.

### `seq_abc.sh`

A shell script that runs the sequential ABC pipeline in `sequential_abc.R`.

### `misdiagnosed_rates.R`

Simulates misclassification of biopsies and generates datasets with the newly adjusted
biopsy proportions, providing insights into the reliability of ABC inference under various conditions.


### `misdiagnosed_generate.sh`

A shell script that runs the misdiagnosis scenarios in `misdiagnosed_rates.R`.

### `embryo_generate.R`

Simulates embryo aneuploidy profiles based on specified meiotic and mitotic error rates.  Generates datasets for posterior distribution analysis.

### `embryo_generate.sh`

A shell script that runs `embryo_generate.R`.

### `plot_data.R`

Generates visualizations from the simulation results for the paper.



