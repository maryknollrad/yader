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

  openjdk 21.0.2 2024-01-16
  OpenJDK Runtime Environment GraalVM CE 21.0.2+13.1 (build 21.0.2+13-jvmci-23.1-b30)
  OpenJDK 64-Bit Server VM GraalVM CE 21.0.2+13.1 (build 21.0.2+13-jvmci-23.1-b30, mixed mode, sharing)
  ```
* If java is not found, install java from [jdk.java.net](https://jdk.java.net)

### Install Tesseract 
* Tesseract for Windows users  
  Download installer from [UB Mannheim](https://digi.bib.uni-mannheim.de/tesseract/)
* Tesseract for Mac users  
  Recommends installing using [Homebrew](https://brew.sh).  
  ```
  brew install tesseract
  ```

## Run YADER Conf
* yaderConf is a configuration utility for testing DICOM connection and regular expression
* before running find out your PACS server's connection information, including IP address, port, application entity (AE) title
* download yaderConf from [github release page](https://github.com/maryknollrad/yader/releases)
* run and answer program's questions
  ```
  java -jar yaderConf-(version).jar
  ```
* it will save yader.conf file in current directory

## Run YADER
* download yader from [github release page](https://github.com/maryknollrad/yader/releases)
* run 
  ```
  java -jar yader-(version).jar
  ```
