import click


@click.group()
def messages():
  pass


@click.command()
def method1():
  click.echo('Method 1 is called')


@click.command()
def method2():
  click.echo('Method 2 is called')


messages.add_command(method1)
messages.add_command(method2)


if __name__ == '__main__':
  messages()

