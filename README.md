Questions for Luke:

Is there any way to make a type derived from another type. Say have 'basic' uniforms, and than 'voice' uniforms derived from that?


No instance for (Random t0) arising from a use of ‘getRandom’
when used for a variable that isn't used else where. Is this laziness / type casting basically saying I don't know what to do with this?



When there was no uniforms, the following caused the 
volume:<<loop>> , without any sort of failure notice:

  uniformV3 uRepelPosition markerPos
  uniformF  uRepelStrength 1