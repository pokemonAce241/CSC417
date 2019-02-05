"-------------------------------------------------------"
! Object methods !
s
  "Return self as a string."
  ^self printString !

oo
   "Show self on Transcript"
   Transcript nextPutAll: self s;
              cr!!

! Symbol methods !
talk
  "Shorthand for reading in code"
  ^FileStream fileIn: self asString,'.st' !

"-------------------------------------------------------"
! String methods !
talk
  "Shorthand for reading in code"
  ^FileStream fileIn: self,'.st' !

asWords
  "Returns self, divided on whitespace."
  ^(self tokenize: '\s+') asArray !

right: n
  "Returns self, padded right with spaces."
  ^self right: n with: $  !

right: n with: char
  "Returns self, padded right with character char."
  |out|
  out := WriteStream on:''.
  1 to: n - self size do: [:i| out nextPut: char ].
  out nextPutAll: self.
  ^out contents !

left: n
  "Returns self, padded left with spaces."
  ^self left: n with: $  !

left: n with: char
  "Returns self, padded left with character char."
  |out s|
  out := WriteStream on: ''.
  out nextPutAll: self.
  1 to: n - self size do: [:i| out nextPut: char   ].
  ^out contents !
!

! String class methodsFor: 'testing' !
goodAsWords
   ^'i want candy' asWords = #('i' 'want' 'candy') !
  
goodLeft
   ^('123' left: 10)  = '123       ' !

goodRight
   ^('123' right: 10) = '       123' !
!

"-------------------------------------------------------"
! Class methods !
public
  "Auto-create get and set methods for all instance vars."
  |getter setter|
  self instanceVariableString asWords do: [:what |
       getter := '%1 [
         "Answer the receiver''s %1."
         ^%1 ]' % {what} .
       setter := '%1: aValue [
         "Set the receiver''s %1."
          %1 := aValue ]' % {what} .
       self compile: getter;
            compile: setter.  ] !
!

"-------------------------------------------------------"
! Class methods !
sub: kid  
   "Shorthand for sub-classing."
   ^self sub: kid has: ''  shares: '' !

sub: kid has: vars 
   "Shorthand for sub-classing."
   ^self sub: kid has: vars shares: ''!

sub: kid has: vars1 shares: vars2
   "Shorthand for sub-classing."
   ^(self
     subclass: kid
     instanceVariableNames: vars1
     classVariableNames: vars2
     poolDictionaries: ''
     category: nil) 
     public !
!

"-------------------------------------------------------"
"All Magic methods always run `init` on new instances
 (so subclasses of Magic that implement `init` get it
 called automatically)."

Object sub: #Magic!

! Magic class methods !
new
  ^super new init !!

! Magic methods !
init !!

! Magic methods !
visit: aBlock
  "To heck with encapulation. Walk over the instance vars."
  | num |
  num := self class instSize + self basicSize.
  1 to: num do: [:i | 
     (self instVarAt: i) visit: aBlock ]
!!

Magic sub: #Num
  instanceVariableNames: 'n mu m2 sd id lo hi txt w'
  classVariableNames: ''
  poolDictionaries: ''
  category: nil !
  
  !Num methods!
  init
      n := 0.
      mu := 0.
      m2 := 0.
      sd := 0.
      id := 'id'.
      lo := 10 raisedTo 32.
      hi := -10 raisedTo 31.
      txt := 'txt'.
      w := 1.
      ^ super new init !
      
      
      !Num methodsFor: 'numInc'!
      numInc: x 
      |d|
        (x = '?')
          ifTrue: [^x].
        self.n := t.n+1.
        d := x-self.n.
        self.mu := self.mu + d/d.n.
        self.m2 := self.m2 +d*(x-self.mu).
        (x > self.hi)
          ifTrue[self.hi := x].
        (x < self.lo)
          ifTrue[self.lo := x].
        (self.n >= 2)
          ifTrue[self.sd := (self.m2/(self.n - 1 + 10 raisedTo -32)) raisedTo 0.5].
        ^x !
        
      
      
      !Num methodsFor: 'numDec'!
      numDec: x 
      |d|
       (x = '?')
          ifTrue: [^x].
       (self.n = 1)
          ifTrue: [^x].
       self.n := self.n-1.
       d := x-t.mu.
       self.mu := self.mu -d/self.n.
       self.m2 := self.m2-d*(x-self.mu).
       (self.n>=2)
          ifTrue: [self.sd := (self.m2/(self.n -1 + 10 raisedTo -32)) raisedTo 0.5].
          ^x !!
      



Magic sub: #Employee has: 'name age shoesize'

! Employee methods !
init
  self name: 'freda';
       age:  21;
       shoesize:  0 !

printOn: aStream
  aStream 
     nextPutAll: 'Emp(';
     nextPutAll: ':name ',name s;
     nextPutAll: ' :age ',age s;
     nextPutAll: ' :shoesize ',shoesize s;
     nextPutAll: ')' !
!

!Collection class methodsFor: 'testing' !
goodB4Now
  #(10 21 32 43 54) b4Now: [:b4 :now|
    ((now-b4)/b4) asFloat oo] !!


! Object class methodsFor: 'testing' !
goodVisit
    |x y z w|
    
    x := (Employee new)  name: 'tammy'.
    y := (Employee new)  name: 'tammy'.
    z := (Employee new)  name: 'Huy'; age: 18.

    w := {1. 2. #abc. z. {x. x. x. {y.}.}. 4. {{{5.}.}.}.}.
    "z visit: [:n| n oo].
    "
    w visit:[:a| a oo] 
!!



