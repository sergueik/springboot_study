import click
import json
from pprint import pprint

# NOTE: cannot use both decorators - will be getting TypeError: Attempted to convert a callback into a command twice.
# @click.group("cli")
@click.command()
@click.pass_context
@click.argument( 'document', default = 'data.json' )
@click.argument('value', type = int, default = 0)
# NOTE: need to match function parameter / argumens. Cannot declare a placeholder parameter), otherwise
# TypeError: func() got an unexpected keyword argument 'value'
# def func(ctx, document):
# def func(ctx, document, _):
def func(ctx, document, value):
  # NoneType
  pprint(type(ctx.obj))
  with open(document) as _stream:
    ctx.obj = json.load(_stream)
    pprint(type(ctx.obj))
    pprint(ctx.obj)

@click.command()
@click.argument('values', type = str, nargs = -1)
def varargs(values):
  click.echo(*values)
  # for int values
  # click.echo(f'The product is {reduce(mul, vals, 1)}')

@click.command()
@click.option('-s', '--name', prompt = 'Your name argument', help = 'Provide "name" argument')
# parameter name is fixed
def named_ars(name):
  click.echo('name = "{}"'.format(name))

@click.command()
@click.option('--words', '-ww', multiple = True)
def repeated_args(words):
  click.echo('words: {}'.format(','.join(words)))

@click.command()
@click.option('--blue', is_flag=True, help='message in blue color')
@click.option('--red', is_flag=True, help='message in red color', default=None)
def flag_args(blue: bool, red: bool):
  if blue:
    click.secho('Hello there', fg='blue')
  else:
    click.secho('Hello there')


# NOTE: example1.py does not exercise group
if __name__ == '__main__':
  # NOTE: not passing arguments
  #  func()
  # varargs()
  # named_args()
  # repeated_args()
  flag_args()
