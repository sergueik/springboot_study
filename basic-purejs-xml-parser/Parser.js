class Parser {
  constructor() {

  }
  parseXML(data) {
    var result = new Map();
    data = this.replaceEmptyElements(this.sanitize(this.unescapeString(data)));
    // Error "RangeError: Maximum call stack size exceeded": Failed to parse the XML.
    while (data.match(/<[^\/][^>]*>/)) {
      // getting the current open Tag
      var openTag = this.clearAttributes(data.match(/<[^\/][^>]*>/)[0]);
      // getting the tag name
      var tag = openTag.replace('<', '').replace('>', '');
      console.error('tag: "' + tag + '"');
      // looking for the closing tag tag
      var closeTag = openTag.replace('<', '</');
      var closeTagPositon = data.indexOf(closeTag);
      // getting the element value

      var value = data.substring(openTag.length, closeTagPositon);
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

      data = data.substring(openTag.length * 2 + 1 + value.length);
    }
    return result;
  }

  // remove comments
  removeComments = function(data) {
    while (data.match(/<!--/) && data.match(/-->/)) {
      var l1 = data.indexOf('<!--');
      var l2 = data.indexOf('-->');
      var dataBefore = data.substring(0, l1 );
      var dataAfter = data.substring(l2 + '-->'.length);
      data = dataBefore + dataAfter;

    }
    return data
  }
  // remove xml declaration
  removeXMLDeclaration = function(data) {
    return data.replace(/<\?.*\?>/g, '')
  }

  // remove comments and xml declaration
  sanitize = function(data) {
    return this.removeComments(this.removeXMLDeclaration(data))
  }
  // replace empty elements with  a normal element with a blank content
  replaceEmptyElements = function(data) {
    var result = data.replace(/<([^>\/]+)\/>/g, '<$1></$1>');
    return result;
  }
  // prepare the body for DOM parsing
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