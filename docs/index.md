---
layout: default
---

## Welcome to YADER

YADER as a simple program for extracting dose from CT studies and reporting dose summary.

### Pros
* works like a bot, gathering information periodically using standard C-FIND and C-GET protocols after setup
* minimal server and network overhead
* combined web server
* supports both the local filesystem database (SQLite) and a dedicated database server (PostgreSQL)
* distributed as a single jar file

### Cons
* not designed to work in real-time 
* cannot extract information where all images are needed to calculate

## Before Installation

YADER is distributed as a single fat jar file. And needs Java and [Tesseract](https://tesseract-ocr.github.io) to run.

### Install JAVA
* Run following commands from terminal.  
  ```
  java --version
  ```
* If java is not found, install java from [jdk.java.net](https://jdk.java.net)  
    * Oracle JRE requires login and license agreement, recommends JDK despite its size.  
    * Recommends LTS version of JDK (currently 21).  
    * Download file for your operating system, extract to a folder and set PATH, JAVA_HOME environment variables.  
      further information : (Baeldung's site)[https://www.baeldung.com/java-home-vs-path-env-var]

### Install Tesseract 
* Tesseract for Windows users  
  Download installer from [UB Mannheim](https://digi.bib.uni-mannheim.de/tesseract/)
* Tesseract for Mac users  
  Recommends installing using [Homebrew](https://brew.sh).  
  ```
  brew install tesseract
  ```

## Download and Run YADER Conf
* yaderConf is a configuration utility for testing DICOM connection, OCR and regular expression
* before running find out your PACS server's connection information, including IP address, port, application entity (AE) title
* [download yaderConf](https://github.com/maryknollrad/yader/releases/download/{{ site.yader_version }}/yaderConf-{{ site.yader_version }}.jar)
* run and answer program's questions
  ```
  java -jar yaderConf-{{ site.yader_version }}.jar
  ```
* if it works to the end, yader will run without problem.
* yaderConf will save yader.conf file in current directory, please read carefully contents and make adjustments
    * process time : `calendar-event`
    * start date : `process-begin`
    * default drl category 

## Download and Run YADER
* [download yader](https://github.com/maryknollrad/yader/releases/download/{{ site.yader_version }}/yader-{{ site.yader_version }}.jar)
* download ct-info.conf from [project site](https://github.com/maryknollrad/ct_infos) and add information of your CT's dose report series
* run YADER at the same directory as yader.conf and ct-info.conf files
  ```
  java -Dconf.file=ct_info.conf -jar yader-{{ site.yader_version }}.jar
  ```
