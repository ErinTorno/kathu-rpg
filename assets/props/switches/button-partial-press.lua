mass = 0
pressLevel = 0
massThreshold = getConfig("mass-threshold") or 10

function updatePressLevel(ety)
  local lastPressLevel = pressLevel
  
  if mass >= massThreshold then
    pressLevel = 2
  elseif mass >= massThreshold / 2 then
    pressLevel = 1
  else
    pressLevel = 0
  end
  
  if lastPressLevel ~= pressLevel then  
    if pressLevel == 2 then
      modifyWirePower(ety, 1)
    elseif lastPressLevel == 2 then
      modifyWirePower(ety, 1)
    end
    
    setAnimation(ety, (pressLevel == 2 and "pressed") or (pressLevel == 1 and "partial") or "unpressed")
  end
end

function onSensorCollisionBegin(selfEty, userEty)
  mass = mass + (getMass(userEty) or 0)
  
  updatePressLevel(selfEty)
end

function onSensorCollisionEnd(selfEty, userEty)
  mass = mass - (getMass(userEty) or 0)

  updatePressLevel(selfEty)
end