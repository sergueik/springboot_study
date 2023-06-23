package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional

import java.nio.file.Paths
import example.ApplicationPropertyUpdater
import example.UdeployPropertyUpdater
import example.PropertyUpdater
import example.Utils
import java.util.Properties
// NOTE: if the groovy and java class with same name preseent
// the error will be observed in runtime
// as
// java.lang.IncompatibleClassChangeError: Implementing class
// at example.PropertyUpdater.class$(PropertyUpdater.groovy)
// at example.PropertyUpdater.$get$$class$example$UdeployPropertyUpdaterBootstrap(PropertyUpdater.groovy)
// with variations
class PropertyUpdaterTask extends DefaultTask {
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
      PropertyUpdater propertyUpdater = null
      // def propertyUpdater = null 
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
				// NOTE: java.lang.IncompatibleClassChangeError: Implementing class
				propertyUpdater = new ApplicationPropertyUpdater(configuration, properties)
				propertyUpdater.setTrim(false)
				propertyUpdater.updateConfiguration()
				configuration = propertyUpdater.getConfiguration()
				println "new configuration: " + configuration
				if (configurationFilePath != null)
					utils.writeToFile(configuration, configurationFilePath, true)
			    
				println 'Done.'
                break
            case 'udeploy':                
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
				// NOTE: java.lang.IncompatibleClassChangeError: Implementing class

				propertyUpdater = new UdeployPropertyUpdater(configuration, properties)
				
				propertyUpdater.updateConfiguration()
				configuration = propertyUpdater.getConfiguration()
				println "new configuration: " + configuration
				if (configurationFilePath != null)
					utils.writeToFile(configuration, configurationFilePath, true)
			    
				println 'Done.'
                break
            case 'udeploy2':
            // NOTE:  java.lang.IncompatibleClassChangeError: Implementing class
        	// at example.PropertyUpdater.class$(PropertyUpdater.groovy)
        	// at example.PropertyUpdater.$get$$class$example$UdeployPropertyUpdaterBootstrap(PropertyUpdater.groovy)            
			    def propertyUpdaterBootstrap = new UdeployPropertyUpdaterBootstrap( fileName, commandline)
			    if (filePath != null) 
			    	propertyUpdaterBootstrap.setFilePath(filePath)
				propertyUpdaterBootstrap.process()
				println 'Done.'
                break
                
            default:
                throw new IllegalArgumentException("Unknown syntax")
        }

	}
}
