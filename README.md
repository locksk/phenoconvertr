# phenoconverter

Pharmacogenomics is being used to infer enzymatic activity on the basis of genetic variation, as opposed to deriving estimates based on drug clearance methods. Enzymatic activity across the diplotype may either be described using metabolism phenotypes (ranging from Poor Metabolism, up to Ultra-rapid metabolism) or through activity scores (ranging from 0 to >3, in which increased scores represent faster metabolism). 

A caveat of this is phenoconversion, which represents the difference between actual enzyme activity, and that which is suggested via genetics. Phenoconversion may result in conversion to a higher-than-predicted phenotype in instances in which enzyme inducers are present. In the presence of enzyme inhibitors, phenoconversion can result in a lower-than-predicted phenotype. 

Many commonly- and not-so-commonly-used drugs and substances can cause phenoconversion. As a few examples, anti-depressants, cigarette smoking, oral contraceptives, and even grapefruit juice, can all cause discrepencies between inferred and actual metabolism statuses. Therefore, it is important to take these into account during analyses. 

Development of this package was loosely based upon the `ChlorpromazineR` package (Brown, 2021)

Further details are found in the [companion vingette](https://locksk.github.io/phenoconverter-companion/) (also located in docs/companion)

## ⚠️⚠️⚠️⚠️ Disclaimer ⚠️⚠️⚠️⚠️
Use at your own risk.

* This package was created for use with the CardiffCOGS sample. While there has been an effort to make it flexible to different data sets, it just may not work. 
* Sertraline is identified as a moderate CYP2D6 inhibtor in Lesche et al., (2020) but a weak CYP2D6 inhibitor in the FDA tables and therefore no phenoconversions are currently performed on individuals taking those drugs.

## Description

`phenoconverter` aims to provide a transparent and reproducible method for converting activity scores to phenoconversion-corrected activity scores for CYP1A2, CYP2C19, CYP2C9, CYP2D6, CYP3A4, and CYP3A5. Reference data was obtained from the [FDA](https://www.fda.gov/drugs/drug-interactions-labeling/healthcare-professionals-fdas-examples-drugs-interact-cyp-enzymes-and-transporter-systems), which has information on inhibitors and inducers for a range of CYP-family enzymes. 

## Installation
        # please note differences between repository name and package name
        devtools::install_github("locksk/phenoconvertr")
        library(phenoconverter)

## Example use
        test <- phenoconverter::test_data()

        phenoconverter::phenoconvert(test, "ID", "CYP1A2", "CYP1A2_AS", "other_meds")


## Authors and acknowledgment
Development of this package was loosely based upon the `ChlorpromazineR` package (Brown, 2021)

## License
For open source projects, say how it is licensed.

