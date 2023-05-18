# based on https://github.com/manuphatak/pureyaml/tree/develop/tests
from pprint import pprint
import pureyaml
from pureyaml.nodes import *  # noqa
from pureyaml.parser import YAMLParser
pureyaml_parser = YAMLParser(debug=True)
text = """
  a: 1
  b:
    c: 3
    d: 4
"""

pprint(pureyaml.load(text))
print(pureyaml.dump(pureyaml.load(text)))
