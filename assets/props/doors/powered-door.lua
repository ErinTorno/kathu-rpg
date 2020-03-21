function onSignalChange(ety, power)
  if power > 0 then
    setAnimation(ety, "open")
    
    setCollisionCategory(ety, "intangible", "closed")
    setCollisionCategory(ety, "movement", "open")
  else
    setAnimation(ety, "closed")
    
    setCollisionCategory(ety, "movement", "closed")
    setCollisionCategory(ety, "intangible", "open")
  end
end