### Info
 
this folder contains a working code based on [StackOverflow answer](https://stackoverflow.com/questions/24873725/how-to-get-pptx-slide-notes-text-using-apache-poi)

### Background

An `.pptx` is a glorified vanilla ZIP archive containing __XML__ files authored according to the Office [Open XML standard](https://learn.microsoft.com/en-us/office/open-xml/about-the-open-xml-sdk). Each such slide is stored in a separate __XML__ file, *embedded* images and fonts live in the `media` folder, and *themes* and *layouts* are stored separately. All this Opens in [PowerPoint for the Web](https://powerpoint.cloud.microsoft/en-us/), [LibreOffice Impress](https://en.wikipedia.org/wiki/LibreOffice_Impress) and the [Google Slides](https://en.wikipedia.org/wiki/Google_Slides)

The standalone PPT Viewer [is discontinued](https://support.microsoft.com/en-us/powerpoint/view-a-presentation-without-powerpoint) long time ago and it not easily avaiable for download

In the past the packaging of PowerPoing components was done via [OLE Storage](https://en.wikipedia.org/wiki/COM_Structured_Storage) which is equivalent but labor intensive to program against

### Usage

```cmd
mvn -DskipTests package install
```

```sh
curl -skLO https://samplelib.com/ppt/sample-presentation.pptx
```
```text
java -cp target\extractor-0.1.0SNAPSHOT.jar;target\lib\* example.Example sample-presentation.pptx 2>nul
```

```text
===== SLIDE 1 =====
Sample Presentation
A demo PPTX file from samplelib.com

===== SLIDE 2 =====
Agenda
Introduction
Project goals
Quarterly results
Roadmap
Questions & answers

===== SLIDE 3 =====
Project Goals
Ship the new dashboard
Faster page loads
Mobile-first layout
Grow active users
Reduce churn
Improve onboarding

===== SLIDE 4 =====
Quarterly Revenue

===== SLIDE 5 =====
Sales by Quarter

===== SLIDE 6 =====
Embedded Image

===== SLIDE 7 =====
Pros and Cons
Advantages
? Open standard
? Wide tool support
? Easy to script
Trade-offs
? Verbose XML
? Large file sizes
? Layout drift

===== SLIDE 8 =====
Thank you!
```
--

### See Also
  
  * https://samplelib.com/sample-ppt.html

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
