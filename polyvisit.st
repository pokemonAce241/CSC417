! Collection methods !
visit: aBlock
     1 to (self size) do: [:x | ((self at :x) visit: aBlock)]
!!

! String methodsFor: 'visit' !
visit: aBlock
	aBlock value: self
	!!

! Object methods !
visit: aBlock
  aBlock value: self
  !!
