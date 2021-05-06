from random import Random, choice
import collections
import functools
import itertools
from os import path
import random
import string
import unittest

TILES = {
  'a': 9,
  'b': 2,
  'c': 2,
  'd': 4,
  'e': 12,
  'f': 2,
  'g': 3,
  'h': 2,
  'i': 9,
  'j': 1,
  'k': 1,
  'l': 4,
  'm': 2,
  'n': 6,
  'o': 8,
  'p': 2,
  'q': 1,
  'r': 6,
  's': 4,
  't': 6,
  'u': 4,
  'v': 2,
  'w': 2,
  'x': 1,
  'y': 2,
  'z': 1,
}

LETTER_SCORE = {
  'a': 1,
  'b': 3,
  'c': 3,
  'd': 2,
  'e': 1,
  'f': 4,
  'g': 2,
  'h': 4,
  'i': 1,
  'j': 8,
  'k': 5,
  'l': 1,
  'm': 3,
  'n': 1,
  'o': 1,
  'p': 3,
  'q': 10,
  'r': 1,
  's': 1,
  't': 1,
  'u': 1,
  'v': 4,
  'w': 4,
  'x': 8,
  'y': 4,
  'z': 10,
}

def calculate_word_score(word, score_table=LETTER_SCORE):
    """Return the word's scrabble score"""
    score = 0
    for letter in word:
        if is_valid_letter(letter):
            score += score_table.get(letter.lower())
        else:
            return 0
    return score

def calculate_word_score_triple(word, score_table=LETTER_SCORE):
    """Return the word's scrabble score if the largest character can score a triple."""
    score = 0
    largest_char = get_highest_scoring_char(word)
    largest_char_index = word.find(largest_char)
    for i, letter in enumerate(list(word), start=0):
        if i == largest_char_index:
            score += score_table.get(letter) * 3
        else:
            score += score_table.get(letter)
    return score

def scoring_heuristic(fn, score_table=LETTER_SCORE, word=''):
    return fn(word, score_table)

def is_valid_letter(char):
    """Check whether a given character is a valid alphabet letter"""
    return char in string.ascii_letters

def pick_random(count=7, entry_list=list(string.ascii_lowercase),
                deletion=False):
    """Return a list of randomly selected characters.

    Args:
        count (int): Number of characters to pick
        entry_list (list): List containing ascii characters
        deletion (bool): Remove selected character from entry_list pool

    Returns:
        list: A list of selected characters. For example:
        ['a', 'c', 'a', 'g', 'z', 'e']

    """
    random_picks = []
    for _ in itertools.repeat(None, count):
        random_choice = choice(entry_list)
        random_picks.append(random_choice)
        if deletion:
            entry_list.remove(random_choice)
    return random_picks

def create_tiles_list(tiles):
    """Return a normalised tile list.

    Args:
        tiles (dict): Dictionary containing tile information, with key being the
        tile character, and the value being the tile count. For example:
        { 'a': 3, 'b': 1, ... }

    Returns:
        list: Returns a list containing a flattened tile pool, for example:
        [ 'a', 'a', 'a', 'b' ... ]

    """
    tile_list = []
    for k, v in tiles.items():
      tile_list.extend(itertools.repeat(k , v))
    return tile_list

def can_create_word(rack, scrabble_word):
    """Check whether a word rack can form a given scrabble word

    Args:
        rack (list | str): A list of characters representing the word rack
        scrabble_word (str): The scrabble word

    Returns:
        bool: Whether the particular scrabble word can be formed from the given word
        rack
    """
    rack_counter = collections.Counter(rack)
    word_counter = collections.Counter(scrabble_word)
    return not((word_counter - rack_counter))

def search_valid_creation(rack=[], dictionary_path='../dictionary-guardian.txt'):
    valid_list = []
    assert path.exists(dictionary_path), "Cannot find dictionary file"
    with open(dictionary_path, 'r') as file:
        lines = (word.strip().lower() for word in file)
        words = [ word for word in lines ]
    for scrabble_word in words:
        assert len(rack) > 0, "Empty word rack"
        if can_create_word(rack, scrabble_word):
            valid_list.append(scrabble_word)
    return valid_list

def longest_element(word_list):
    """Return the longest word from a word list."""
    return max(word_list, key=len)

def compile_score_list(word_list, method=calculate_word_score,
                       score_table=LETTER_SCORE):
    pfn = functools.partial(scoring_heuristic, score_table=score_table)
    return list(map(lambda word: pfn(fn=method, word=word), word_list))

def get_highest_scoring_word(word_list, fn=calculate_word_score):
    """Return the highest scoring word for list.

    Given a word list, returns the word that scores the highest. Users may define
    a custom scoring heuristic.

    Args:
        word_list (list): Word list
        method: Calculation heuristic

    Returns:
        str: Word that has the highest scrabble score

    """
    score_list = compile_score_list(word_list, method=fn)
    return word_list[score_list.index(max(score_list))]

def get_highest_scoring_char(word, score_table=LETTER_SCORE):
    """Return the character with the highest score.

    Caveat: Does not perform non-alphabet check

    Args:
        word (str): A single word
        score_table (dict): Scoring table. Defaults to LETTER_SCORE

    Returns:
        char: Single character in the word representing the character with the
        highest score
    """
    return max(list(word), key=lambda x: score_table[x])

class TestScrabble(unittest.TestCase):
    def test_is_valid_letter(self):
        self.assertTrue(is_valid_letter('a'))
        self.assertFalse(is_valid_letter('人'))

    def test_gest_highest_scoring_char(self):
        self.assertEqual(get_highest_scoring_char('python'), 'y')
        self.assertEqual(get_highest_scoring_char('zz'), 'z')
        self.assertEqual(get_highest_scoring_char('zest'), 'z')

    def test_random_pick(self):
        self.assertCountEqual(pick_random(7, entry_list=['a', 'b', 'c', 'd', 'e',
                                                    'f', 'g'], deletion=True),
                              ['a', 'b', 'c', 'd', 'e', 'f', 'g'])

    def test_highest_scoring_word(self):
        self.assertEqual(get_highest_scoring_word(search_valid_creation(['x', 'y', 'l', 'o', 'p', 'h',
                                              'o', 'n', 'e'])), 'xylophone')

if __name__ == '__main__':
    unittest.main()
