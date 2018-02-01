#!/usr/bin/env python
# vim: set fileencoding=utf-8 :

from collections import defaultdict

pinyin_initial = defaultdict(list)

lines = open("unicode_to_pinyin.txt").readlines()

for line in lines :

    line = line.strip().split(' ')

    unicode_ord = line[0]
    char = unichr(int(unicode_ord, 16))

    pinyins = line[1][1:-1].split(',')
    initials = set([k[0] for k in pinyins])

    pinyin_initial[char].extend(list(initials))

# now generate an python module containing pinyin table
print("# vim: set fileencoding=utf-8 :")
print("\n" * 3)
print("pinyin_initial = {")
for key in pinyin_initial.keys():
    print("u'%s' : %s ," % (key.encode("utf-8"),  pinyin_initial[key] ))
print("}")

