import getopt
import sys


def show_options():
  print(f'debug: {debug}\ninput: {input_file}') 
  for cnt in range(len(keys)):
    print('keys: {}, vals: {}'.format(keys[cnt],vals[cnt]))

help_message = 'usage: groups.py --keys <text> -vals <text> [--keys <text> --vals <text>] --input <input file>'

if __name__ == '__main__':
  try:
    opts, args = getopt.getopt(sys.argv[1:], 'hdk:v:i:', ['help', 'debug', 'keys=', 'vals=', 'input=' ])
  except getopt.GetoptError as err:
    print(help_message)


  # NOTE: it is quite easy to fall into 
  # NameError: name 'opts' is not defined
  # simply by providing bad option arguments like e.g.
  # --zinput a.txt instead of --input a.txt
  # the following code protects against the same
  #  to demonstrate uncommend the following lines
  # for option, argument in opts:
  #  pass
  # see also: 
  # https://stackoverflow.com/questions/1592565/determine-if-variable-is-defined-in-python 
  try: 
    opts
  except NameError: 
    print(help_message)
    opts = None
    exit()
  
  text = None
  keys = []
  vals = []
  input_file = None
  global debug
  # TODO:read through envinonment
  debug = True

  for option, argument in opts:
    if option in ('-d', '--debug'):
      debug = True
    elif option in ('-h', '--help'):
      print(help_message)
      exit()
    elif option in ('-k', '--keys'):
      if debug:
        print('processing "k" option: ' + argument)
      text = argument
      keys.append(argument) 
    elif option in ('-v', '--vals'):
      if debug:
        print('processing "v" option: ' + argument)
      text = argument
      vals.append(argument) 
    elif option in ('-i', '--input'):
      output_file = argument
    else:
      assert False, 'unhandled option: {}'.format(option)
  if text == None or output_file == None:
    print(help_message)
    exit()
  
  
  
