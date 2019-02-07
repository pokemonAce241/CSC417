Magic subclass: #Num has : 'n mu m2 sd id lo hi txt w'!
  
  !Num methods!
  init
      n := 0.
      mu := 0.
      m2 := 0.
      sd := 0.
      id := 'id'.
      lo := 10 raisedTo: 32.
      hi := -10 raisedTo: 31.
      txt := 'txt'.
      w := 1  
      !!
      
      
      !Num methodsFor: 'numInc'!
      numInc: x 
      |d|
        (x = '?') ifTrue: [^x].
        n := n + 1.
        d := x - n.
        mu := mu + d/n.
        m2 := m2 + d*(x - mu).
        (x > hi) ifTrue: [hi := x].
        (x < lo) ifTrue: [lo := x].
        (n >= 2) ifTrue: [sd :=  (m2/(n - 1 + 10 raisedTo: -32)) raisedTo: 0.5].
        ^x !
        
      
      
      !Num methodsFor: 'numDec'!
      numDec: x 
      |d|
       (x = '?') ifTrue: [^x].
       (n = 1)  ifTrue: [^x].
       n := n-1.
       d := x-mu.
       mu := mu - d/n.
       m2 := m2 - d*(x - mu).
       (n>=2) ifTrue: [sd := (m2/(n - 1 + 10 raisedTo: -32)) raisedTo: 0.5].
          ^x !      

!Num methodsFor: 'nextPutAll'!
nextPutAll: numbers
numbers do: [:x | self numInc: x].
!!
