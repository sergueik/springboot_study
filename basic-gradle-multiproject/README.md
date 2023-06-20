### Info

This directory contains a trimmed composite gradle project aggregation [example](https://docs.gradle.org/current/samples/zips/sample_structuring_software_projects-groovy-dsl.zip)
from 

 found in the official __Migrating Maven multi-module builds to Gradle multi-project builds__  [gradle documentation](https://docs.gradle.org/current/userguide/migrating_from_maven.html#migmvn:multimodule_builds)

### See Also

   * [establishing cross-project dependencies](https://docs.gradle.org/current/userguide/declaring_dependencies.html#sub:project_dependencies) via `dependencies { implementation project(':projectame') implementation projects.projectname }` keyword that built targets are exercised in the correct order
   * https://ncorti.com/blog/gradle-plugins-and-composite-builds
