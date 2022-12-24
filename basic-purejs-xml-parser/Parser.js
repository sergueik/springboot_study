class Parser {
  constructor() {

  }
  parseXML(xmlSource) {
    var result = new Map();
    xmlSource = this.unescapeString(xmlSource);

    while (xmlSource.match(/<[^\/][^>]*>/)) {
      // getting the current open Tag
      var openTag = this.clearAttributes(xmlSource.match(/<[^\/][^>]*>/)[0]);
      // getting the tag name
      var tag = openTag.replace('<', '').replace('>', '');
      console.error('tag: "' + tag + '"');
      // looking for the closing tag tag
      var closeTag = openTag.replace('<', '</');
      var closeTagPositon = xmlSource.indexOf(closeTag);
      // getting the element value

      var value = xmlSource.substring(openTag.length, closeTagPositon);
      var tmp = '';

      if (value.match(/<[^\/][^>]*>/)) {
        // if the inner value is an Element
        // do recursive call
        tmp = this.parseXML(value);
      } else {
        tmp = value; // store a text value
      }
      // if the map doesn't have the tag already
      if (!result.has(tag)) {
        result.set(tag, tmp); // creating the tag
      } else {
        // replace the string value with array value
        console.error('converting value to array: "' + tag + '"');
        var tmpValue = [];
        tmpValue.push(result.get(tag));
        tmpValue.push(tmp);

        result.delete(tag);
        result.set(tag, tmpValue);
      }

      xmlSource = xmlSource.substring(openTag.length * 2 + 1 + value.length);
    }
    return result;
  }

  unescapeString = function(data) {
    // convert all whitespace to space
    var result = data.replace(/\n|\t|\r/g, ' ');
    // remove element attributes
    result = this.clearAttributes(result);
    //remove whitespace
    result = result.replace(/ /g, '');
    return result;
  }

  clearAttributes = function(data) {
    // remove name="value" pairs
    var result = data.replace(/(?:[:-\w]+) *= *"(?:[^"]+)"/g, '');
    return result;
  }

}
module.exports = Parser;