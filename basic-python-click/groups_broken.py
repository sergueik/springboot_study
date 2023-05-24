import click
import getopt
import sys


@click.group()
def messages():
  pass

def show_options():
  click.echo(f'search={text}\ndebug={debug}\noutput={output_file}') 

@click.command()
def method1():
  click.echo('Method 1 is called')
  show_options()

@click.command()
def method2():
  click.echo('Method 2 is called')
  show_options()

try:
  opts, args = getopt.getopt(sys.argv[1:], 'hds:o:', ['help', 'debug', 'search=', 'output='])
  
except getopt.GetoptError as err:
  print('usage: groups.py --search <text> --output <output file>')
text = None
outpu_file = None
global debug
debug = False
for option, argument in opts:
  if option in ('-d', '--debug'):
    debug = True
  elif option in ('-h', '--help'):
    print('usage: groups.py --search <text> --output <output file>')
    exit()
  elif option in ('-s', '--search'):
    text = argument
  elif option in ('-o', '--output'):
    output_file = argument
  else:
    assert False, 'unhandled option: {}'.format(option)
if text == None or output_file == None:
  print('usage: groups.py --search <text> --output <output file>')
  exit()


messages.add_command(method1)
messages.add_command(method2)


if __name__ == '__main__':
  messages()

