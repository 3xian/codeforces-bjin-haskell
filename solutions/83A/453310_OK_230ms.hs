import Data.List
main=interact$show.sum.map((\x->x*(x+1)`div`2).genericLength).group.tail.words