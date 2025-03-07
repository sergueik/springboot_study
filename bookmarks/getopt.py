import sys
import getopt

def download():
    print("Running download function...")

def upload():
    print("Running upload function...")

def main(argv):
    try:
        opts, args = getopt.getopt(argv, "", ["download", "upload"])
    except getopt.GetoptError as err:
        print(str(err))
        sys.exit(2)

    for opt, arg in opts:
        if opt == "--download":
            download()
        elif opt == "--upload":
            upload()

if __name__ == "__main__":
    main(sys.argv[1:])

