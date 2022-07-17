# Python lacks immutability at every turn
# this can be overcome by discipline and is not of any consequence
### If only typelevel discipline existed for Python

a = [1,2,3,4,5,6,7]
print(a)
for i in range(len(a)):
    if 0 == i % 2:
        a[i] = -a[i]
print(a)

# Just don't do this, and avoid the `=` operator
#
