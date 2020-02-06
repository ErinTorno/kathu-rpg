function onHit(selfEty, attackerEty)
  local state = getGlobalBool("orb-switch-state") or false
  
  setGlobalBool("orb-switch-state", not state)
  
  -- local newAnim = not state and "warm" or "cold"
  -- log("orb-switch state changed to " .. newAnim)
  -- setAnimation(selfEty, newAnim)
end

log("orb-switch script has been initialized")