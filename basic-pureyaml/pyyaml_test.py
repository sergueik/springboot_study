import yaml

from pprint import pprint
document = """
  a: 1
  b:
    c: 3
    d: 4
"""
pprint(yaml.load(document, Loader= yaml.SafeLoader))
print( yaml.dump(yaml.load(document, Loader= yaml.SafeLoader)))
