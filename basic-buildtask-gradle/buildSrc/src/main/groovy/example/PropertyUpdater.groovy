package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional

import java.nio.file.Paths
import example.ApplicationPropertyUpdater
import example.UdeployPropertyUpdater
import example.Utils
import java.util.Properties

class PropertyUpdater extends DefaultTask {
    @Input
    String syntax = null;
    @Input
    String fileName = null;
    @Input
    @Optional
    String filePath = null;
    @Input
    @Optional
    String commandline = null;
    
    @TaskAction
    void updateProperties() {
      def utils = Utils.getInstance()
      switch (syntax) {
            case 'java':
			    if (!fileName) 			    
			    	fileName = "gradle.properties"
			    if (!filePath) 			    
			    	filePath = "buildSrc/src/main/resources"
			    if (!commandline)
			       commandline = utils.getApplicationProperties().getProperty("commandline") 

			    String configuration = null
			    String configurationFilePath = null
			    if (filePath == null) {
					println "reading template configuration from resources."
					configuration = utils.getResourceContent(fileName)
				} else {
					configurationFilePath = Paths.get(String.format("%s/%s/%s", System.getProperty("user.dir"), filePath, fileName)).normalize() .toAbsolutePath().toString()
					println "reading template configuration from file: " + configurationFilePath
					configuration = utils.getFileContent(configurationFilePath)
				}
				println 'template configuration: ' + configuration 
				
				// NOTE: observed some challenge with lambda inline in groovy code, give up temporarily
				def properties = utils.getPropertiesFromCommandline(commandline)
				def propertyUpdater = new ApplicationPropertyUpdater(configuration, properties)
				propertyUpdater.setTrim(false)
				propertyUpdater.updateConfiguration()
				configuration = propertyUpdater.getConfiguration()
				println "new configuration: " + configuration
				if (configurationFilePath != null)
					utils.writeToFile(configuration, configurationFilePath, true)
			    
				println 'Done.'
                break
            case 'udeploy':
            /*
			    def propertyUpdaterBootstrap = new UdeployPropertyUpdaterBootstrap( fileName, commandline)
			    if (filePath != null) 
			    	propertyUpdaterBootstrap.setFilePath(filePath)
				propertyUpdaterBootstrap.process()
				println 'Done.'
                break
                */
			    if (!fileName) 			    
			    	fileName = "application.yaml"
			    if (!filePath) 			    
			    	filePath = "buildSrc/src/main/resources"
			    if (!commandline)
			       commandline = utils.getApplicationProperties().getProperty("commandline") 

			    String configuration = null
			    String configurationFilePath = null
			    if (filePath == null) {
					println "reading template configuration from resources."
					configuration = utils.getResourceContent(fileName)
				} else {
					configurationFilePath = Paths.get(String.format("%s/%s/%s", System.getProperty("user.dir"), filePath, fileName)).normalize() .toAbsolutePath().toString()
					println "reading template configuration from file: " + configurationFilePath
					configuration = utils.getFileContent(configurationFilePath)
				}
				println 'template configuration: ' + configuration 
				
				// NOTE: observed some challenge with lambda inline in groovy code, give up temporarily
				def properties = utils.getPropertiesFromCommandline(commandline)
				def propertyUpdater = new UdeployPropertyUpdater(configuration, properties)
				
				propertyUpdater.updateConfiguration()
				configuration = propertyUpdater.getConfiguration()
				println "new configuration: " + configuration
				if (configurationFilePath != null)
					utils.writeToFile(configuration, configurationFilePath, true)
			    
				println 'Done.'
                break
            default:
                throw new IllegalArgumentException("Unknown syntax")
        }

	}
}