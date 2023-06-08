package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

class PrintToolVersionBuildSrcTask extends DefaultTask {
    String tool
    String fileName
    String commandline
    @TaskAction
    void printToolVersion() {
     def propertyUpdaterBootstrap = new PropertyUpdaterBootstrap( tool, fileName, commandline)
		propertyUpdaterBootstrap.process()
		println "Done."

	}
}
