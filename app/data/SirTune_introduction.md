## SirTune introduction

The primary function of SirTune is to provide information to User Language (SOUL) programmers to make it easy for them to improve the performance of their programs. This is accomplished in two stages:

1. Data is collected in a running Online region. This data is obtained by "polling," which is at regular intervals SirTune examines the state of the Online region and records data pertinent to this state.  The data collection portion was integrated with the Sirius Mods prior to version 7.5 of Model 204.

2. After this data is collected, it is summarized and presented in a set of reports that indicate to a SOUL programmer the potential performance problem areas. These problem areas can generally be refined to an arbitrary level of detail (for example, individual lines of code in a procedure). The reporting portion is the SIRTUNEREPORT SOUL program, distributed in the RKTools SIRIUS file (prior to RKTools 7.7) or the M204PROC file (as of RKTools 7.7).


### Contents

1. System requirements
2. Installation
    * 2.1 SirTune object file
    * 2.2 SirTune SOUL files
3. SirTune topics


### System requirements

SirTune requires the following components to run:

* Mainframe operating systems – one of the following:
    * z/OS
    * CMS (releases currently supported by IBM) under z/VM
* Model 204 V6R1 or later, running in its Online or IFAM4 configuration.


### Installation

SirTune consists of an object file (for data collecting) as well as SOUL files (for reporting). As of SirTune 7.1, the object code was integrated into Sirius Mods, and as of Model 204 7.5, Sirius Mods was integrated into the Model 204 nucleus. Thus, if you are running Model 204 7.5 or later and you are authorized to use SirTune, the necessary object code is already installed into Model 204.  In addition to the object code, you need to download and install the SirTune SOUL files to run SirTune reports.


#### SirTune object file

If you are running a pre-7.5 version of Model 204 and want to refresh the SirTune object file, you can download the "Sirius Mods" object file from the Rocket Software website "Rocket M204 Customer Care" page (http://m204.rocketsoftware.com). The download process requires a Model 204 user ID and password.

1. On the "Rocket M204 Customer Care" page, go to the "Downloads and Uploads" section and click the "Download object files" link.
2. After providing Model 204 login credentials, you access the "Download object files" page, which contains a dynamically-generated list of the various Model 204 object files that you are authorized to download.
3. Select the Sirius Mods row for the version you require, and click the "Download object file" link.


#### SirTune SOUL files

SirTune report generation is performed by a SOUL program that is distributed as part of the RKTools product family in the SIRIUS file (for versions of RKTools prior to 7.7) or the M204PROC file (as of RKTools 7.7).

* If your site uses RKTools, verify that file SIRIUS or M204PROC contains the program SIRTUNEREPORT. If the RKTools version is 7.7 or higher, you can use the RKWeb interface to view SirTune report output on your browser. RKTools installation has information about RKTools and RKWeb installation.
* If your site is not licensed for RKTools, you are still authorized (as a user of SirTune) to download the SIRIUS or M204PROC file. You can download the file from the Rocket website:
    1. On the "SOUL files" page, click the "SOUL files" link in the "Downloads and Uploads" section.
    2. On the Rocket M204 Customer Care page, find the appropriate row for your version of RKTools, and click the "Download Procfile" link.
You don't need to do the entire RKTools installation: just download and restore the SIRIUS or M204PROC file, and make it available to your SirTune report jobs.


### SirTune topics

The SirTune documentation consists of the pages listed below. This list is also available as a "See also" link from each of the pages.

For information about product changes and Model 204 feature support per SirTune version, see the release notes.
For information about product error messages, see SirTune data collector messages.

* SirTune introduction
* SirTune data collection under MVS
* SirTune data collection under CMS
* SirTune data collection statements
* SirTune MODIFY and SMSG commands
* SirTune report generation
* SirTune reports
* SirTune user states
* SirTune and Model 204 quad types
* SirTune statement wildcards
* SirTune date processing

Category: SirTune
