package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional

class PrintToolVersionBuildSrcTask extends DefaultTask {
    @Input
    String syntax = null;
    @Input
    String fileName = null;
    @Input
    @Optional
    String commandline = null;
    
    @TaskAction
    void printToolVersion() {
    
      switch (syntax) {
            case 'java':
			    def propertyUpdaterBootstrap = new ApplicationPropertyUpdaterBootstrap( fileName, commandline)
					propertyUpdaterBootstrap.process()
					println "Done."
                break
                break
            case 'udeploy':
			    def propertyUpdaterBootstrap = new UdeployPropertyUpdaterBootstrap( fileName, commandline)
					propertyUpdaterBootstrap.process()
					println "Done."
                break
            default:
                throw new IllegalArgumentException("Unknown syntax")
        }

	}
}
