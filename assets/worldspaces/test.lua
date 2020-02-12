updateCount = 0

function onUpdate(ety)
  updateCount = updateCount + 1
  
  if updateCount > 120 then
    updateCount = 0
    
    state = getGlobalVar("orb-toggle-state") or false
  
    setGlobalBool("orb-toggle-state", not state)
  end
end

playerEty = getPlayerEntity()

log("test-world script has been initialized; player entity has ID " .. playerEty)