# Getting Started with Git and GitHub

## About The Coffee, Cookie and Coding $\left(C^3\right)$ Workshops

The DSDE team created this workshop to assist the Yale Public Health and Biostatistics masters-level students effectively leverage computational tools and analytical methods in their educational and future professional endeavors.


## About Workshop

**Presented by:** Yale's Public Health Data Science and Data Equity (DSDE) Team

**Workshop Title:** Getting Started with Git and GitHub

**Platform:** Hybrid (link to Zoom)

**Event Page:** (link to website calendar event)

**Date:** Monday November $4^{\text{th}}$, 2024

We will be launching our Coffee, Cookie and Coding $\left(C^3\right)$ workshop series with a starter guide on Git and GitHub. Upon completing the workshop, you will be able to:
- Identify the benefits of using Git and GitHub.
- Configure your local Git and personal GitHub accounts and set up their security keys.
- Share and contribute to projects with an example created in RStudio.

You can find videos covering the content of this workshop on the DSDE YouTube (link) and website (link). If you are affiliated with Yale, you can set up an office hour appointment with one of the data scientists. Please book an appointment [here](https://outlook.office365.com/owa/calendar/DataScienceDataEquityOfficeHours@yale.edu/bookings/).

## About Repository

There are two GitHub repsitories associated with this workshop. The link to the second repository can be found here: [JHU-CRC-Cases-and-Recoveries](https://github.com/ysph-dsde/JHU-CRC-Cases-and-Recoveries).

### Overview Of Contents

- **For the analysis:** "Analysis Script_Vaccinations Time-Series Plot.R"
- **For cleaning the raw data:** "Cleaning Script_Vaccinations.R"
- **For harmonizing census data:** "Population Estimates and Projections/Population Data Cleaning Script.R"
- **R version:** 4.4.1
- ``renv`` is included to reproduce the environment.

**NOTE:** The cleaning and census harmonization scripts have already been run to generate the necessary files called in the analysis workflow. Users of this repository will only need to open the "Analysis Script_Vaccinations Time-Series Plot.R". Re-running the cleaning script _might_ result in variations as a product of the isotonic regression.

### First Steps



1. [Create a new](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-new-repository) GitHub repository.
   
   **NOTE:** Do not use a template or include a description, README file, .gitignore, or license. Only adjust the GitHub account owner as needed and create the name for the new repository. We recommend you intially set the repository to Private.
   
2. Open Terminal.
3. Navigate to the file location you want to copy the repo to.
   ```
   cd "/file_location/"
   ```

4. Clone a bare copy of the repo.
   ```
   # using SSH
   git clone --bare git@github.com:ysph-dsde/JHU-CRC-Vaccinations.git
   
   # or using HTTPS
   git clone --bare https://github.com/ysph-dsde/JHU-CRC-Vaccinations.git
   ```
   
5. Open the project file.
   ```
   cd "JHU-CRC-Vaccinations.git"
   ```
   
6. Push a mirror of the cloned Git file to your newly created GitHub repository.
   ```
   # using SSH
   git push --mirror git@github.com:EXAMPLE-USER/NEW-REPOSITORY.git

   # or using HTTPS
   git push --mirror https://github.com/EXAMPLE-USER/NEW-REPOSITORY.git
   ```

7. Copy the following into the terminal to remove the project file used to create a new remote repository.
   ```
   cd ..
   rm -rf JHU-CRC-Vaccinations.git
   ```





  
1. Clone a the newly created GitHub repository.
   ```
   # using SSH
   git clone --bare git@github.com:ysph-dsde/JHU-CRC-Vaccinations.git
   
   # or using HTTPS
   git clone --bare https://github.com/ysph-dsde/JHU-CRC-Vaccinations.git
   ```

12. asd
   ```
   git checkout --orphan tempBranch         # Create a temporary branch
   git add -A                               # Add all files and commit them
   git commit -m "Reset the repo"
   git branch -D main                       # Deletes the master branch
   git branch -m main                       # Rename the current branch to master
   git push -f origin main                  # Force push master branch to github
   git gc --aggressive --prune=all          # Remove the old files
   ```
11. Open the newly cloned file and launch the "JHU-CRC-Vaccinations.Rproj" project file.
12. In the R console, activate the enviroment by runing
   ```
   renv::restore()
   ```

   **NOTE:** You may need to run ``renv::restore()`` twice to initialize and install all the packages indicated for by the lockfile. You know you are all set to go when running ``renv::restore()`` gives you ``- The library is already synchronized with the lockfile.``
   
11. 

After initially establishing your local copy (mirror) from this remote repository, activate the enviroment by runing ``renv::restore()`` in the R Console and follow the prompts.

This project uses a renv() lock so that the enviroment is reporducible for all users. Users not using this version of R may experience problems running the script, even with the renv() activated. After initially establishing your local copy (mirror) from this remote repository, activate the enviroment by runing ``renv::restore()`` in the R Console and follow the prompts.

NOTE: You may need to run ``renv::restore()`` twice to initialize and install all the packages indicated for by the lockfile. You know you are all set to go when running ``renv::restore()`` gives you ``- The library is already synchronized with the lockfile.``


Users not using this version of R may experience problems running the script, even with the renv() activated.

## About Original Data Source

The [Johns Hopkins Coronavirus Resource Center](https://coronavirus.jhu.edu/) (JHU CRC) tracked and made publically available COVID-19 pandemic data from January 22, 2020 and March 10, 2023. We imported cumulative vaccination counts for the U.S. from their [GovEX/COVID-19](https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data) GitHub repository. The raw data used in the analysis script can be found in the data_tables/vaccine_data/us_data/time_series subdirectory ([original source](https://github.com/govex/COVID-19/blob/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv)).

The data dictionary provided by JHU CRC has been copied to this repository ([original source](https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data/us_data)). Additional details and methods for harmonizing the U.S. Census Bureau's 2010 to 2019 population projections with 2020 to 2023 vintages can be found in the "Population Estimates and Projections" subdirectory.







