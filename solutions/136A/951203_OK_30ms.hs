import List
main=interact$unwords.map(show.snd).sort.(`zip`[1..]).drop 2.(0:).map read.words
