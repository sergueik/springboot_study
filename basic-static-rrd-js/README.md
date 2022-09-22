### Info

Basic Springboot application hosting Flot backed RRD processing  page also rendered by thymeleaf spring framework 
with a few key resources loaded from static resource directory


### Running
```sh
mvn clean -Dmaven.test.skip=true spring-boot:run
```
followed by

```sh
curl http://localhost:8080/
```

the default value in the input field `data.rrd` corresponds to the file present in `src\main\resources\static\data` (relative path `/data/`)
so clicking on the `Update` button renders it and the graph is interactive.

![Sample Page](https://github.com/sergueik/springboot_study/blob/master/basic-static-rrd-js/screenshots/capture-flot.png)

the chart is interactie: date range can be selected with mouse:

![Sample Page](https://github.com/sergueik/springboot_study/blob/master/basic-static-rrd-js/screenshots/capture-select.png)

![Sample Page](https://github.com/sergueik/springboot_study/blob/master/basic-static-rrd-js/screenshots/capture-new-range.png)

To render arbitrary MRTG file, e.g. example file in the project root directory, copy it in the same directory as above
```sh
cp example.rrd src/main/resources/static/data
```

and update the input. No application restart is necessary

### See Also
  
  * __JavascriptRRD__ [project page](https://sourceforge.net/projects/javascriptrrd/)
  * __Flot__ JavaScript plotting for __jQuery__ [home page](https://www.flotcharts.org) and [repository](JavaScript plotting for jQuery) 
  * __Flot__ [tutorial](https://www.jqueryflottutorial.com/what-is-jquery-flot.html)
  * https://jgefroh.medium.com/a-guide-to-using-nginx-for-static-websites-d96a9d034940
  * [linking the thymeleaft page resources stored in local directory](https://stackoverflow.com/questions/29460618/inserting-an-image-from-local-directory-in-thymeleaf-spring-framework-with-mave)
  * [thymeleaf in a Spring MVC application](https://www.baeldung.com/thymeleaf-in-spring-mvc)
  * [RRDtool - tutorial and graph examples](https://calomel.org/rrdtool.html)
  * [properties](https://www.baeldung.com/properties-with-spring) with Spring and Spring Boot
  * [static resources](https://www.baeldung.com/spring-mvc-static-resources) - customizing path to support legacy non-standard layouts
  * [DAO patterns](https://www.baeldung.com/java-dao-pattern)
  * https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
  * [original basic springboot /thymeleaf project](https://github.com/kolorobot/spring-boot-thymeleaf)
  * https://howtodoinjava.com/junit5/junit-5-assumptions-examples/
  * wikipedia [comparison of javascript charting libraries](wikipedia comparison of javascript charting libraries)
   
### Youtube Links
  * [Javascript Flot renderer of RRDTool Network Traffic Monitoring data](https://www.youtube.com/watch?v=yY9rbOHxwyg) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
