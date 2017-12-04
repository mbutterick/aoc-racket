#lang reader "main.rkt" ★★
abcde fghij ; is a valid passphrase.
abcde xyz ecdab ; is not valid - the letters from the third word can be rearranged to form the first word.
a ab abc abd abf abj ; is a valid passphrase, because all letters need to be used when forming another word.
iiii oiii ooii oooi oooo ; is valid.
oiii ioii iioi iiio ; is not valid - any of these words can be rearranged to form any other word.