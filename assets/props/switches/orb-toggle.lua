local stateVar = "orb-toggle-state"

function onInit(ety)
  registerGlobalVarListener(stateVar, "onStateChange")
end

function onHit(selfEty, attackerEty)
  state = getGlobalVar(stateVar) or false
  
  setGlobalBool(stateVar, not state)
end

function onStateChange(ety, orbState)
  local newAnim = orbState and "warm" or "cold"
  
  setAnimation(ety, newAnim)
end

log("orb-toggle script has been initialized")