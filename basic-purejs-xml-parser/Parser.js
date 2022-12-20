class  Parser{
  constructor(){
  this.jsonRes = {};// JSON object
  }
  parseXML(xmlSource){
    var tag,tmp,closeTagPositon,openTag, value;// helper variables
    // cleaning the XML
    xmlSource = this.unescapeString(xmlSource);
    // While XML string has <elemts>
    while (xmlSource.match(/<[^\/][^>]*>/)){
      // getting the current open Tag
      openTag = this.clearAttributes(xmlSource.match(/<[^\/][^>]*>/)[0]);
      // getting the tag name
      // the "substring" and "length" appear lacking in painless
      tag = openTag.replace('<', '').replace('>','');
      console.error('tag: "' + tag + '"');
      // looking for the </element> tag
      // closeTagPosition will contain the index of the closing tag
      closeTagPositon = xmlSource.indexOf(openTag.replace('<', '</'));
      // getting the value between tags
      // the "length" appears lacking in painless
      value = xmlSource.substring(openTag.indexOf('>') + 1, closeTagPositon);
      // if the inner value is a tag
       if (value.match(/<[^\/][^>]*>/)) {
         // call to the function with the childrens nodes
           tmp = this.parseXML(value);
       }
       else {
           tmp = value; // if is a simple value
       }
       // if the object doesn't have the tag already
       if (this.jsonRes[tag] === undefined) {
           this.jsonRes[tag] = tmp; // creating the tag
       }
       else if (Array.isArray(this.jsonRes[tag])) {
           // is there is a value with the same tag is an array
           this.jsonRes[tag].push(tmp);
       }

       xmlSource = xmlSource.substring(openTag.length * 2 + 1 + value.length);
    }
    return this.jsonRes;
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
