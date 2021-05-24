from collections import deque
import re
import operator
import functools

# Party Codes
party_code = {
    'C': 'Conservative Party',
    'L': 'Labour Party',
    'UKIP': 'UKIP',
    'LD': 'Liberal Democrats',
    'G': 'Green',
    'Ind': 'Independent',
    'SNP': 'SNP'
}

# Split the result stream
def split_string(result):
    return result.split(',')

def get_constituency(result):
    return result[0]

def get_total_count(iter_result):
    p = re.compile('\d')
    total_votes = list(map(int, [ n for n in iter_result if p.match(n) ]))
    return functools.reduce(operator.add, total_votes)

def construct_info_map(result):
    clone = list(map(str.strip, split_string(result)))
    total_votes = get_total_count(clone)
    resultDict = {}
    resultDict['constituency'] = clone.pop(0)
    it = iter(clone)
    for x, y in zip(it, it):
        inner = {}
        inner['count'] = int(x)
        inner['share'] = int(x) / total_votes
        resultDict[party_code[y]] = inner
    return resultDict

print(construct_info_map('Cardiff West, 11014, C, 17803, L, 4923, UKIP, 2069, LD'))
print(construct_info_map('Islington South & Finsbury, 22547, L, 9389, C, 4829, LD, 3375, UKIP, 3371, G, 309, Ind'))
