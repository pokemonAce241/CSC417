!Num methodsFor: 'b4'!
  b4Now: aBlock
  |b4|
  self do: [:now|
  b4 = nil
    ifFalse: [aBlock value:b4 value: now].
    b4 := now]
  !
!
