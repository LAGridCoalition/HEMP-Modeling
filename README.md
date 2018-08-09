# HEMP-Modeling - Nuclear EMP FORTRAN code

Computational modeling of the effects of High altitude Electromagnetic Pulse

## About the code

All existing site data is based on US AF Thesis AD 777841 on EMP by Terry C. Chapman that was written in 1973 for AFIT, which is included in this repo for your perusal.

The goal of new data is to eventually reflect an EMP voltage impressed on any item acting as an antennae will be greater than 49,999 Volts per Meter and be accomplished via unclassified/open source material/documents.

It is noted that the USAF AFIT Thesis by Louis W. Seiler Jr contains no FORTRAN code and offers no rigid proof that the voltages implied can be proved. See ADA 009208 in 1975.

## Contributing to the project

We welcome your contributions to this project.

In particular, we're looking for improvements that match the following categories:

* Improvements based on "GAS Boosting" theory found in many open documents, for example [""]() or [""]()
* Improvements on calculating impact of third- and fourth-generation weapons
* Performance improvements with minimal impact on the numerical accuracy of the code (with a demonstration of the change in quantified error)
* Alternative calculation methods that improve accuracy of the code

In order to make it as easy as possible for us to use your contributions, please address the following items in your Pull Request description and submitted code:

* A written description of the changes/improvements made to the code, including premises, facts, methodological approaches, and references
* A synopsis of how to navigate the changes in the code, in order to make review easier
* Data and replication process for demonstrating claims made in the description of the code improvements
* Limitations or concerns to be aware of regarding the approach and the code improvements
