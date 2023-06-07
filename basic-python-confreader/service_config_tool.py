import getopt
import sys
from pprint import pprint
import os
sys.path.insert(0,os.path.join(os.path.curdir,'mod'))
from service_config import ServiceConfig

# command line entrance code
help_message = 'usage: service_config.py --path <text> -file <text> [--debug]'


def show_options():
    print(f'debug: {debug}\nconfig_filename: "{config_filename}"\nconfig_path: "{config_path}"')


if __name__ == '__main__':

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hdp:f:', ['help', 'debug', 'path=', 'file='])
    except getopt.GetoptError as err:
        print(help_message)

    # https://stackoverflow.com/questions/1592565/determine-if-variable-is-defined-in-python
    try:
        opts
    except NameError:
        print(help_message)
        opts = None
        exit()

    global debug
    # TODO:read through environment
    debug = False
    config_path = os.path.curdir
    config_filename = 'data.conf'

    for option, argument in opts:
        if option in ('-d', '--debug'):
            debug = True
        elif option in ('-h', '--help'):
            print(help_message)
            exit()
        elif option in ('-p', '--path'):
            config_path = argument
        elif option in ('-f', '--file'):
            config_filename = argument
        else:
            assert False, 'unhandled option: {}'.format(option)
    if config_path is None or config_filename is None:
        print(help_message)
        exit()
    if debug:
        show_options()

    service_config = ServiceConfig()
    service_config.path = config_path
    service_config.debug = debug
    service_config.filename = config_filename
    # repeat to examine idempotency
    # for cnt in range(3):
    pprint(service_config.lines)
    pprint(service_config.config)

