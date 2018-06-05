
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
