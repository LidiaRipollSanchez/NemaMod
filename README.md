# [NemaMod](https://www.nemamod.org)
NemaMod is an application for intuitive and rapid visualisation of neuropeptide connections in C.elegans.

<img width="1721" alt="Screenshot 2023-11-21 at 18 57 17" src="https://github.com/LidiaRipollSanchez/NemaMod/assets/86192587/16108dae-76ad-42ee-8509-e551810c7807">

Papers about RadaR available: https://doi.org/10.1101/347534

See a running example here.

Prerequisites for using RadaR

RadaR was built in R , an open source programming language using the Shiny package, a web application framework for R. Users will need to download R in order to use RadaR and we suggest the use of RStudio. R is completely free to use. All required code can be found in this github repositroy.

Input type for RadaR's calculation

RadaR works with standard csv-files (.csv). The variables needed for RadaR are as follows:

Input variables for RadaR

Admission data

Variable	Detail
id	Patient ID or study ID
adm_id	Admission ID
gender	Gender
adm_start_date	Admission date (YYYY-MM-DD)
adm_end_date	Discharge date (YYYY-MM-DD)
death_during_adm	Death during admission (TRUE/FALSE)
adm_route	Origin at admission
sub_specialty	Sub-specialty
specialty	General specialty (internal medicine, surgery, other)
birth_date	Birth date (YYYY-MM-DD)
Antimicrobial consumption data

Variable	Detail
id	Patient ID or study ID
ab_start_date	Start of antimicrobial treatment (YYYY-MM-DD)
ab_stop_date	Stop of antimicrobial treatment (YYYY-MM-DD)
ab_route	Administration route (e.g. IV, oral, ...)
atc_code	ATC code according WHO ATC classification system
ddd_per_day	Defined daily dose of antimicrobial according to WHO ATC classification system per day
Microbiological data

Variable	Detail
id	Patient ID or study ID
specialty	Ordering specialty
antimicrobial susceptibility testing	Several columns of tested antimicrobial agents (e.g. amox, cipr etc.) with resistance results (R / I / S)
mo	Microbial ID (if test is positive) following the nomenclature of the Integrated Taxonomic Information System
material	Test material (currently supported: blood and urine)
test_date	Test date (YYYY-MM-DD)
test_number	Test number
These data will be loaded, merged, and transformed for analysis upon start of RadaR

Privacy and storage

RadaR works with sensitive hospital data and is based on single observations on the patient level. All data for the running example is simulated and don't represent any real patients.
RadaR can be run locally on protected servers within institutions (for example: Shiny server) or on personal machines (mac, windows, linus).

Author

logo_unilogo_umcg

RadaR was created at the Faculty of Medical Sciences of the University of Groningen and the Medical Microbiology & Infection Prevention department of the University Medical Center Groningen (UMCG) by Christian Luz, PhD Student.

Copyright

License RadaR is licensed under the GNU General Public License (GPL) v2.0. In a nutshell, this means that this package:

May be used for commercial purposes

May be used for private purposes

May be modified, although:

Modifications must be released under the same license when distributing the package
Changes made to the code must be documented
May be distributed, although:

Source code must be made available when the package is distributed
A copy of the license and copyright notice must be included.
Comes with a LIMITATION of liability

Comes with NO warranty
