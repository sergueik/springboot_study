package example

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional

import java.nio.file.Paths
import example.ApplicationPropertyUpdater
import example.CustomPropertyUpdater
import example.UdeployPropertyUpdater
import example.PropertyUpdater
import example.Utils
import java.util.Properties

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
      switch (syntax) {
            case 'java':
				propertyUpdater = new ApplicationPropertyUpdater()
                break
            case 'udeploy':                
				propertyUpdater = new UdeployPropertyUpdater()								
                break
            case 'custom': 
				propertyUpdater = new CustomPropertyUpdater()								
                break
            default:
                throw new IllegalArgumentException("Unknown syntax")
        }

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
		    // TODO: detect absolute path and prepend with ${user.dir} otherwise
			configurationFilePath = Paths.get(String.format("%s/%s/%s", System.getProperty("user.dir"), filePath, fileName)).normalize() .toAbsolutePath().toString()
			println "reading template configuration from file: " + configurationFilePath
			configuration = utils.getFileContent(configurationFilePath)
		}
		println 'template configuration: ' + configuration 
		
		// NOTE: observed some challenge with lambda inline in groovy code, give up temporarily
		def properties = utils.getPropertiesFromCommandline(commandline)
		propertyUpdater.setTrim(true)
		propertyUpdater.setProperties(properties)
			propertyUpdater.setConfiguration(configuration)
		propertyUpdater.updateConfiguration()
		configuration = propertyUpdater.getConfiguration()
		println "new configuration: " + configuration
		if (configurationFilePath != null)
			utils.writeToFile(configuration, configurationFilePath, true)
	    
		println 'Done.'
	}
}
