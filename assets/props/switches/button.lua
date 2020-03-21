mass = 0
isActive = false
massThreshold = getConfig("mass-threshold") or 10

log("mass threshold is " .. massThreshold)

function onSensorCollisionBegin(selfEty, userEty)
  mass = mass + (getMass(userEty) or 0)
  
  if not isActive and mass >= massThreshold then
    isActive = true
  
    setAnimation(selfEty, "pressed")
    
    modifyWirePower(selfEty, 1)
  end
end

function onSensorCollisionEnd(selfEty, userEty)
  mass = mass - (getMass(userEty) or 0)

  if isActive and mass < massThreshold then
    isActive = false
  
    setAnimation(selfEty, "unpressed")
    
    modifyWirePower(selfEty, -1)
  end
end