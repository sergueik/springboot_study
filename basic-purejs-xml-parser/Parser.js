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
      // the "substring" and "length" appeared to be lacking in painless (actually present)
      tag = openTag.replace('<', '').replace('>','');
      console.error('tag: "' + tag + '"');
      // looking for the </element> tag
      // closeTagPosition will contain the index of the closing tag
      closeTagPositon = xmlSource.indexOf(openTag.replace('<', '</'));
      // getting the value between tags
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
       if (jsonRes[tag] === undefined) {
           jsonRes[tag] = tmp; // creating the tag
       }
       else { 
           // is there is a value with the same tag make it an array
           console.error('converting value to array: "' + tag + '"');
           var tmpValue = [];
           tmpValue.push( jsonRes[tag]);
           tmpValue.push(tmp);
           jsonRes[tag] = tmpValue;
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
