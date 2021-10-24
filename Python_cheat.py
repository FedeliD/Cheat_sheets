#Python CHEAT_Sheet
for value in variable:
    pass

while condition:
    pass


if condition:
    pass

    else

    elif


def fname(arg):
    pass

#####################################################################
#this prints mary had a little lamb
print("Mary had a little lamb.")
#this prints the second sentence. The "snow" string can be changed
#snow is not a variable. a variable wouldn't have had the
#single quotes around it.
print("Its fleece was white as {}.".format('snow'))
#prints next sentence and than ptrints 10 small dots.
print("And everywhere that Mary went.")
print("." * 10) #what'd that do?
#creates the ingredients of the "cheese burger" ending.
end1 = "C"
end2 = "h"
end3 = "e"
end4 = "e"
end5 = "s"
end6 = "e"
end7 = "B"
end8 = "u"
end9 = "r"
end10 = "g"
end11 = "e"
end12 = "r"

print("watch that comma at the end. try removing it to see what happens")
print(end1 + end2 + end3 + end4 + end5 + end6, end=' ')
print(end7 + end8 + end9 + end10 + end11 + end12)


#########################################
#here some cool stuff!
#we are giving functions arguments, than calling the function
#and then returning a value to a variable
#the syntax is like this: variable = function(a, b)
#here I'm adding
def add(a, b):
    print(f"ADDING {a} + {b}")
    return a + b
#here I'm subtracting
def subtract(a, b):
    print(f"SUBTRACTING {a} - {b}")
    return a - b
#here I'm multiplying
def multiply(a, b):
    print(f"MULTIPLYING {a} * {b}")
    return a * b
#here I'm dividing
def divide(a, b):
    print(f"DIVIDE {a} / {b}")
    return a / b
#here I'm printing some stuff
print("let's do some math with just functions")
#here I'm giving some variables the function's output value.
#that's cool, sincce I'm both calling a function and defining a new variable!
age = add(30, 5)
height = subtract(78, 4)
weight = multiply(90, 2)
iq = divide(100, 2)
#here I'm printing the variables
print(f"Age: {age}, Height: {height}, Weight: {weight}, IQ: {iq}")
#htere's the puzzle! YAYYYY
#A puzzle for the extra credit, type it anyway.
print("Here is a puzzle.")
#here I'm defining a new variable that uses all the previously defined functions
#and then I'm printing some cool stuff
what = add(age, subtract(height, multiply(weight, divide(iq, 2))))
#basically return allows to give a value to a variable from a function.
#you can return anything you could put on the right of a "="
print("That becomes: ", what, "Can you do it by hand?")


####################################
from sys import argv
script, first, second = argv
def my_first_function(arg1, arg2):
    print(f"the first argument is {arg1};")
    print(f"the second argument is {arg2}")
    print("now I'm going to sum them")
    boz = int(arg1)
    sboz = int(arg2)
    print(f""" the
    sum
    is:
    {boz + sboz}""")

print("now I'm going to run the fucntion using the arguments you inserted")
my_first_function(first, second)
