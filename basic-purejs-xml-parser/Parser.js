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
        tag = openTag.match(/[^<][:-\w+$]*/)[0];
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

  unescapeString = function(string){
    var aux = string.replace(/\n|\t|\r/g,' ');
    aux = this.clearAttributes(aux);
    var chars = aux.split('');
    aux = '';
    chars.forEach((v,i) => {
      if(!(v == ' ')){
        aux +=v;
      }
    });
    return aux
  }
  clearAttributes = function(string){
    var aux = string.replace(/(?:[:-\w+]+) *= *"(?:[^"]+)"/, '');
    return aux
  }

}
module.exports = Parser;
