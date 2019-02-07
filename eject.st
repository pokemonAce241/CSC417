!Collection methodsFor: 'ejection'!
eject: aBlock
^self select: [:x| (aBlock value:x)not]
!!
