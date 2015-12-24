#!/usr/bin/env python

'''
Anything after '#' on the line is ignored by the py interpreter
Except first line # followed by ! which is called hashbang/sha-bang.

In *nix, when a script with a shebang is executed, the program loader
parses the rest of the script's initial line as an interpreter
directive.

'''

a = 5
""" triple quote test """

"""
triple quote test
"""

print("hello world")

#test
print("a "*5)

print("test") #test
print("test2")

a=5
print(a)

for i in range(5):
    print(i)

def main():
    print("hello world")


'''
A multi-line
comment
'''

print('no comment')






main()

class MyClass:
    def __init__(self):
        print("just a class")

    def method(self):
        print("just a method")


m = MyClass()

if __name__ == '__main__':
    main()
