import struct
import sys

import matplotlib.pyplot as plt


def error(msg):
    print(f'Error: {msg}', file=sys.stderr) 
    return 1


def main(argv):
    if len(argv) <= 1:
        return error('Please specify an input file.')

    input_filepath = argv[1]
    try:
        floats = []
        with open(input_filepath, 'rb') as input_file:
            while (data := input_file.read(4)):
                floats.append(struct.unpack('f', data)[0])

        plt.plot(list(range(len(floats))), floats)
        plt.show()
    except FileNotFoundError:
        return error(f'File not found: "{argv[1]}".')
    
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
