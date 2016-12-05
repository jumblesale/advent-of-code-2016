import md5

def produceHash(prefix, index):
    return md5.md5("%s%s" % (prefix, index)).hexdigest()

def doesStringStartWithFiveZeroes(string):
    return string[0:5] == '00000'

def getSixthCharacter(string):
    return string [5:6]

def generateValidKeys(prefix, index):
    while True:
        hash = produceHash(prefix, index)
        index = index + 1
        print "\033[1;32;40m %s" % (hash,)
        if doesStringStartWithFiveZeroes(hash) == True:
            key = getSixthCharacter(hash)
            yield key

input = 'wtnhxymk'

keys = []

iterations = 8

generator = generateValidKeys(input, 0)

for i in range(0, iterations):
    keys.append(next(generator))

print ''.join(keys)
