function onSignalChange(ety, power)
  if power > 0 then
    setAnimation(ety, "open")
  else
    setAnimation(ety, "closed")
  end
end