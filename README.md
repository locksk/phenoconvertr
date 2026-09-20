# phenoconverter

Pharmacogenomics is being used to infer enzymatic activity on the basis of genetic variation, as opposed to deriving estimates based on drug clearance methods. Enzymatic activity across the diplotype may either be described using metabolism phenotypes (ranging from Poor Metabolism, up to Ultra-rapid metabolism) or through activity scores (ranging from 0 to >3, in which increased scores represent faster metabolism). 

A caveat of this is phenoconversion, which represents the difference between actual enzyme activity, and that which is suggested via genetics. Phenoconversion may result in conversion to a higher-than-predicted phenotype in instances in which enzyme inducers are present. In the presence of enzyme inhibitors, phenoconversion can result in a lower-than-predicted phenotype. 

Many commonly- and not-so-commonly-used drugs and substances can cause phenoconversion. As a few examples, anti-depressants, cigarette smoking, oral contraceptives, and even grapefruit juice, can all cause discrepencies between inferred and actual metabolism statuses. Therefore, it is important to take these into account during analyses. 

Development of this package was loosely based upon the `ChlorpromazineR` package (Brown, 2021)

Further details are found in the [companion vingette](https://locksk.github.io/phenoconverter-companion/) (also located in docs/companion)

## ⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️ Disclaimer ⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️
Use at your own risk.

* This package was created for use with the CardiffCOGS sample. While there has been an effort to make it flexible to different data sets, it just may not work. 
* Sertraline is identified as a moderate CYP2D6 inhibtor in Lesche et al., (2020) but a weak CYP2D6 inhibitor in the FDA tables and therefore no phenoconversions are currently performed on individuals taking those drugs.

## Description

`phenoconverter` aims to provide a transparent and reproducible method for converting activity scores to phenoconversion-corrected activity scores for CYP1A2, CYP2C19, CYP2C9, CYP2D6, CYP3A4, and CYP3A5. Reference data was obtained from the [FDA](https://www.fda.gov/drugs/drug-interactions-labeling/healthcare-professionals-fdas-examples-drugs-interact-cyp-enzymes-and-transporter-systems), which has information on inhibitors and inducers for a range of CYP-family enzymes. 

## Installation
Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.

## Example use
        test <- phenoconverter::test_data()

        phenoconverter::phenoconvert(test, "ID", "CYP1A2", "CYP1A2_AS", "other_meds")

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Roadmap
If you have ideas for releases in the future, it is a good idea to list them in the README.

## Contributing
State if you are open to contributions and what your requirements are for accepting them.

For people who want to make changes to your project, it's helpful to have some documentation on how to get started. Perhaps there is a script that they should run or some environment variables that they need to set. Make these steps explicit. These instructions could also be useful to your future self.

You can also document commands to lint the code or run tests. These steps help to ensure high code quality and reduce the likelihood that the changes inadvertently break something. Having instructions for running tests is especially helpful if it requires external setup, such as starting a Selenium server for testing in a browser.

## Authors and acknowledgment
Show your appreciation to those who have contributed to the project.

## License
For open source projects, say how it is licensed.

## Project status
If you have run out of energy or time for your project, put a note at the top of the README saying that development has slowed down or stopped completely. Someone may choose to fork your project or volunteer to step in as a maintainer or owner, allowing your project to keep going. You can also make an explicit request for maintainers.
