isEnabled = false

function onInteract(selfEty, userEty)
  isEnabled = not isEnabled
  if isEnabled then
    setAnimation(selfEty, "on")
    modifyWirePower(selfEty, 1)
  else
    setAnimation(selfEty, "off")
    modifyWirePower(selfEty, -1)
  end
end