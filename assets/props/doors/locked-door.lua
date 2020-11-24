function onSensorCollisionBegin(selfEty, userEty)
  playerEty = getPlayerEntity()
  if playerEty == userEty then
    worldID    = getCurrentWorldspaceID()
    worldInvID = getWorldSpaceInventoryID(worldID)
    if worldInvID and hasWorldInventoryItem(worldInvID, "key-small", 1) then
      removeWorldInventoryItem(worldInvID, "key-small", 1)

      setAnimation(selfEty, "open")
      
      setCollisionCategory(selfEty, "intangible", "closed")
      setCollisionCategory(selfEty, "movement", "open")
    end
  end
end