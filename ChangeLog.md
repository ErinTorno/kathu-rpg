# Revision history for kathu

## 0.1.0.0 -- 2019-01-28

* First release

## 0.1.3.0 -- 2019-04-24

* Implemented surface depth sorting

## 0.1.4.0 -- 2019-05-06

* Moved render system to a contained data type
* Render system can load palettes and switch between them to change the colors of the displayed sprites
* Additional parsing utilities
* Damage types can now have defaul resistances for when an actor doesn't specify one

## 0.1.4.1 -- 2019-05-07

* Objects on screen slightly decreases for small screen sizes, to prevent it from looking too zoomed out
* Offscreen sprites are no longer rendered
* Animations change faster from starting and stopping to prevent issue where rapid starting and stopping results in no animation
* Fixed entities facing north when first spawned, rather than south as intended
* Fixed issue with palettes past index 0 not fully updating sprites with background colors
* Fixed streaks between adjacent sprites