# version 1.0.3.9005 (excel branch)

* Created a markdown document that is the template for auto-generating a word document output.
* Launching the model from excel now also auto-generates the word document.
* fixed bug with grade orders and saving
* excel sheet now allows saving interim states


# version 1.0.3.9004 (excel branch)

* loading package now sets the data path in the excel sheet to point to the simulated data.
* moved creation of excel sheets into function instead of script.
* running from excel text logs when problems happen

# version 1.0.3.9003 (excel branch)

* tidied up data creation from simulated or other data so duplicated columns don't cause trouble
* added simulated_data.rds to excel files for quick testing of excel sheet
* excel input and writing now uses xlsx package exclusively so readxl package is no longer needed
* moved some things around in the excel sheet so have changed the references in .onLoad appropriately
* moved excel functions into R folder.

# version 1.0.3.9002 (excel branch)

## MAJOR CHANGES

* Excel_Blockbuster does not save yearly outputs, but instead saves an excel workbook called "output[DATE].xlsx" in the same folder as the input. This workbook contains total backlog (C,D,E) by year, backlog per grade by year, and breakdowns by buildingid and elementid.

## Minor changes
* Excel_Blockbuster no longer sets the working directory. It is not good practise to set the working directory.
* Added brief descriptions of what the excel files scripts are used for and do.
* removed unnecessary "update excel file with folders.R" script that has been incorporated into .onLoad in zzz.R


# version 1.0.3.9001 (excel branch)

* updated vignette so it describes new outputs.
* amended installation instructions so dialog window no longer displays password in plaintext.

# version 1.0.3.9000 (excel branch)

## MAJOR CHANGES
* revamped blockbuster output.  It now outputs backlog and area summaries by elementid and buildingid along with the last element-level and block-level states
* SAVE is now FALSE by default in Blockbuster().  Setting it to TRUE saves the
interim element-level and block-level states. The output file collating the two is no longer saved.

## Other changes
* added input checks that the element-level and block-level objects are well-formed
* added blockbuster2::: to the summarise_element examples as the function is not exported. This was causing the build to fail.
* added tests for integration of summaries into main blockbuster outputs
* changed startup Message to packageStartupMessage as good practise.
* fixed the Blockbuster unit tests that were broken by the new output structure
* added unit tests for the new input checks and summarisers

# version 1.0.2.9001 (excel branch)

* fixed typo bug in summarise_element_area which was causing the build to fail.
* made the excel sheet update on load dependent on finding the sheet in the right place.  If it is not there a message is displayed.

# version 1.0.2.9000 (excel branch)

* added element_summarise_area and element_summarise_backlog functions that will
be used within the Blockbuster function to create yearly summaries.
* Added unit testing of element_summarise_area and element_summarise_backlog.
* updated badges in readme

# version 1.0.2

* updated package authors
* Fixed tests that were broken by carriage returns in warnings that were added in 1.0.1 to keep within 80 character lines)
* added travis and codecov badges to the README
* all functions now have examples
* readr added to dependencies (used to load outputs)

# version 1.0.1

* Changed package and repository name

General fixes to pass CHECK.

* removed spaces from filenames to make them all portable
* removed links to block and element classes as they have been removed from 
the package
* added tidyr to imports as gather is used in pull_Element_Data and the like to 
manipulate outputs into tidy format.
* added documentation for the toy datasets used in documentation examples.
* fixed various warnings and errors associated wwith documentation