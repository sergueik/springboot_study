var Parser = require('./Parser.js');
var fs = require('fs');

var parser = new Parser();
fs.readFile('test.xml', 'utf-8', (err, data) => {
    if (err) {
        return console.error('Could not read file');
    }
    try {
    var result = parser.parseXML(data);
    // Dump
    console.log(result);
    // Demo
    console.log(result['wsdl:definitions']['wsdl:service']);
    console.log(result['wsdl:definitions']['wsdl:binding']['soap:binding']);
    
    } catch(exception) {
      console.error('Error "' + exception + '": Failed to parse the XML.');
    }
});
