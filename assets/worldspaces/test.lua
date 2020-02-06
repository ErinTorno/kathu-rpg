updateCount = 0

lastCamEty = nil
nextCamEty = 2

function onUpdate(ety)
  updateCount = updateCount + 1
  
  if updateCount > 120 then
    updateCount = 0
    local camEty = getCameraEntity()
    setCameraEntity(nextCamEty)
    lastCamEty = nextCamEty
    nextCamEty = camEty
  end
end

playerEty = getPlayerEntity()

log("test-world script has been initialized; player entity has ID " .. playerEty)