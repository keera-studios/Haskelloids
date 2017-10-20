module Haskelloids.Geometry.Text (fromString,
                                  Alignment(..)
                                 )where

import Control.Arrow (second)

import Haskelloids.Geometry (Figure(..))
import Haskelloids.Graphics (Graphic)

-- #### Datatype definitions ###################################################

data Alignment = AlignRight | AlignCentre | AlignLeft

-- #### Constants ##############################################################

-- all charcter figures are drawn with respect to the screen origin (top left) and are drawn within a box 20x30 pixels

-- charWidth - the character width
charWidth :: Double
charWidth = 25

-- ## Character figures #########################

zero, one, two, three, four, five, six, seven, eight, nine :: [Figure]
zero  = [Line [(0,0), (20,0), (20,30), (0,30), (0,0)]]
one   = [Line [(10,0), (10,30)]]
two   = [Line [(0,0), (20,0), (20,15), (0,15), (0,30), (20,30)]]
three = [Line [(0,0), (20,0), (20,15), (0,15), (20,15), (20,30), (0,30)]]
four  = [Line [(0,5), (0,15), (20,15), (20,0), (20,30)]]
five  = [Line [(20,0), (0,0), (0,15), (20,15), (20,30), (0,30)]]
six   = [Line [(0,0), (0,30), (20,30), (20,15), (0,15)]]
seven = [Line [(0,0), (20,0), (20,30)]]
eight = [Line [(0,15), (0,0), (20,0), (20,15), (0,15), (0,30), (20,30), (20,15)]]
nine  = [Line [(20,15), (0,15), (0,0), (20,0), (20,30)]]

a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,sp :: [Figure]
a  = [Line [(0,30), (0,10), (10,0), (20,10), (20,30)], Line [(0,20),(20,20)]]
b  = [Line [(0,30), (0,0), (15,0), (20,5), (20,10), (15,15), (0,15)],
      Line [(15,15), (20,20), (20,25), (15,30), (0,30)]]
c  = [Line [(20,30), (0,30), (0,0), (20,0)]]
d  = [Line [(0,0), (10,0), (20,10), (20,20), (10,30), (0,30), (0,0)]]
e  = [Line [(20,30), (0,30), (0,0), (20,0)], Line [(0,15), (15,15)]]
f  = [Line [(0,30), (0,0), (20,0)], Line [(0,15), (15,15)]]
g  = [Line [(10,20), (20,20), (20,30), (0,30), (0,0), (20,0), (20,10)]]
h  = [Line [(0,0), (0,30)], Line [(0,15), (20,15)], Line [(20,0), (20,30)]]
i  = [Line [(0,0), (20,0)], Line [(10,0),(10,30)],
      Line [(0,30), (20,30)]]
j  = [Line [(0,20), (10,30), (20,30), (20,0)]]
k  = [Line [(0,0), (0,30)], Line [(0,15), (20,0)], Line [(0,15), (20,30)]]
l  = [Line [(0,0), (0,30), (20,30)]]
m  = [Line [(0,0), (0,30)], Line [(0,0), (10,10), (20,0)],
      Line [(20,0), (20,30)]]
n  = [Line [(0,0), (0,30)], Line [(0,0), (20,30)],
      Line [(20,0), (20,30)]]
o  = zero
p  = [Line [(0,30), (0,0), (20,0), (20,15), (0,15)]]
q  = [Line [(0,0), (20,0), (20,20), (10,30), (0,30), (0,0)], Line [(10,20), (20,30)]]
r  = [Line [(0,30), (0,0), (20,0), (20,15), (0,15)], Line [(0,15),(20,30)]]
s  = five
t  = [Line [(0,0), (20,0)], Line [(10,0), (10,30)]]
u  = [Line [(20,0), (20,30), (0,30), (0,0)]]
v  = [Line [(0,0), (10,30), (20,0)]]
w  = [Line [(0,0), (0,30), (10,20), (20,30), (20,0)]]
x  = [Line [(0,0), (20,30)], Line [(0,30), (20,0)]]
y  = [Line [(0,0), (10,10), (10,30)], Line [(20,30), (10,10)]]
z  = [Line [(0,0), (20,0), (0,30), (20,30)]]
sp = []

-- #### Function definitions ###################################################

fromString :: Alignment -> String -> [Figure]
fromString align str = 
  let offset = case align of
                 AlignLeft   -> 0
                 AlignCentre -> -width / 2
                 AlignRight  -> -width
      width  = charWidth * (fromIntegral . length $ str)
  in concat . zipWith (map . Translate) [(x+offset, 0.0) | x <- [0,charWidth..] ] . map charToFig $ str

charToFig :: Char -> [Figure]
charToFig ch = case ch of
                '0' -> zero; '1' -> one; '2' -> two;   '3' -> three; '4' -> four
                '5' -> five; '6' -> six; '7' -> seven; '8' -> eight; '9' -> nine
                'a' -> a;    'b' -> b;   'c' -> c;     'd' -> d;     'e' -> e
                'f' -> f;    'g' -> g;   'h' -> h;     'i' -> i;     'j' -> j
                'k' -> k;    'l' -> l;   'm' -> m;     'n' -> n;     'o' -> o
                'p' -> p;    'q' -> q;   'r' -> r;     's' -> s;     't' -> t
                'u' -> u;    'v' -> v;   'w' -> w;     'x' -> x;     'y' -> y
                'z' -> z;    ' ' -> sp;  _   -> zero
