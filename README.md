# Kathu RPG

A dungeon/puzzle focused RPG written in Haskell.

## Future Goals

* Switch from SDL Surface to SDL Renderer for rendering
* Enable collision groups and support interaction and collision events
* Add AI support for other entities

## Known Issues

* When pushing up against a wall with its corner split by two triangles, objects will receive a slight bounce off of it
* Holes fully enclosed in solid tiles will also have solid collision