import qualified Data.ByteString.Char8 as BS
import List
import Array
import Char
main=BS.getContents>>=print.s.BS.words
m v c=v*97+fromIntegral(ord c)
s[t,b,e]=sum[length$group$sort$g i x y|i<-[max u v..w]]
 where
  x=BS.findSubstrings b t
  y=map(+v)$BS.findSubstrings e t
  u=BS.length b
  v=BS.length e
  w=BS.length t
  h=listArray(0,w)$scanl m 0(BS.unpack t)
  g l(x:xs)(y:ys)
   |x+l<y=g l xs(y:ys)
   |x+l>y=g l(x:xs)ys
   |1>0=(h!y-h!x*97^l):g l xs ys
  g l _ _=[]
