class  Parser{
  constructor(){

  }
  parseXML(xmlSource){
    var jsonRes = {};// JSON object
    var tag,tmp,closeTagPositon,openTag, value;// helper variables
    // cleaning the XML
    xmlSource = this.unescapeString(xmlSource);
    // While XML string has <elemts>
    while (xmlSource.match(/<[^\/][^>]*>/)){
      // getting the current open Tag
      openTag = this.clearAttributes(xmlSource.match(/<[^\/][^>]*>/)[0]);
      // getting the tag name
      tag = openTag.substring(1, openTag.length - 1);
      console.error('tag: "' + tag + '"');
      // looking for the </element> tag
      // closeTagPosition will contain the index of the closing tag
      closeTagPositon = xmlSource.indexOf(openTag.replace('<', '</'));
      if (closeTagPositon == -1){
        tag = openTag.match(/[^<][:-\w$]*/)[0];
        //  console.error('tag: ' + tag );

         closeTagPositon = xmlSource.indexOf('</' + tag);
         if (closeTagPositon == -1) {
             closeTagPositon = xmlSource.indexOf('<\\/' + tag);
         }
      }
      // getting the value between tags
      value = xmlSource.substring(openTag.length, closeTagPositon);
      // if the inner value is a tag
       if (value.match(/<[^\/][^>]*>/)) {
         // call to the function with the childrens nodes
           tmp = this.parseXML(value);
       }
       else {
           tmp = value; // if is a simple value
       }
       // if the object doesn't have the tag already
       if (jsonRes[tag] === undefined) {
           jsonRes[tag] = tmp; // creating the tag
       }
       else if (Array.isArray(jsonRes[tag])) {
           // is there is a value with the same tag is an array
           jsonRes[tag].push(tmp);
       }

       xmlSource = xmlSource.substring(openTag.length * 2 + 1 + value.length);
    }
    return jsonRes;
  }

  unescapeString = function(data){
    // convert all whitespace to space
    var result = data.replace(/\n|\t|\r/g,' ');
    // remove element attributes
    result = this.clearAttributes(result);
    //remove whitespace
    result = result.replace(/ /g,'');
    return result;
  }

  clearAttributes = function(data){
    // remove name="value" pairs
    var result = data.replace(/(?:[:-\w]+) *= *"(?:[^"]+)"/g, '');
    return result;
  }

}
module.exports = Parser;
