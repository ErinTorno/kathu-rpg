function onSensorCollisionBegin(selfEty, userEty)  
  setAnimation(selfEty, "pressed")
  
  modifyWirePower(selfEty, 1)
end

function onSensorCollisionEnd(selfEty, userEty)  
  setAnimation(selfEty, "unpressed")
  
  modifyWirePower(selfEty, -1)
end