# Revision history for verda

## 0.1.0.0 -- TBD
Initial merger of verda game engine. New features are as follows:
* Sound clip and music playing
* Sprites can use a scrolling texture as their animation
* Sprites are able to define scaling size
* Sprites can be tinted to different colors with the Tint component
* Texture scaling can be set on a per file basis with @nearest or @linear file name suffixes
Issues to resolve before merging to master:
* Palettes no longer work, need to implement shader behavior for this
* Allow pixels per unit to be set