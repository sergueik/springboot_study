package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional

class PrintToolVersionBuildSrcTask extends DefaultTask {
    @Input
    String tool = null;
    @Input
    String fileName = null;
    @Input
    @Optional
    String commandline = null;
    
    @TaskAction
    void printToolVersion() {
     def propertyUpdaterBootstrap = new PropertyUpdaterBootstrap( tool, fileName, commandline)
		propertyUpdaterBootstrap.process()
		println "Done."

	}
}
