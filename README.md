# MCC_Synth
Code repository for MCC project on synthetic controls. We used health data from the [Multi-Country Multi-City Collaborative Research Network](https://mccstudy.lshtm.ac.uk) (MCC) database and global wildfire-derived air pollution data from [Xu et al. (2023)](https://www.nature.com/articles/s41586-023-06398-6)

The file `_targets.R` contains the full analysis pipeline. Individual functions called during the pipeline are stored in the `R` folder.

Follow these steps to run the pipeline on your local machine:

1. Clone the repository to your local device.
2. Move the relevant datasets (main MCC data and wildfire air pollution data) to a folder called `data` within your R project folder.
3. Open the R project and install the package `renv` using the terminal.
4. Call `renv::restore()` to load the necessary packages and versions.
5. Call `targets::tar_make()` in the terminal.

The analysis pipeline should now run. Note that some of the simulations are time intensive, so it may be more efficient to adapt the code to run on an HPC. An example jobscript is given in the `run.sh' file.

Please report any issues with running the analysis to calum.kennedy.25@ucl.ac.uk.
