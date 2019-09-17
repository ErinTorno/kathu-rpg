# Revision history for kathu

## 0.1.7.0 -- 2019-09-16

* A major refactor was done to clean up code and eliminate warnings and redundancies
* The library was fully split into two parts: kathu-lib and kathu app
* kathu-lib provides the game's main logic, is system-independent, and doesn't make use of SDL, pointers, etc.
* kathu app forms the code for turning kathu-lib into a desktop executable
* IO.Parsing was eliminated, as was SystemLink; a new Dependency system was added to ease up on the orphan instances and weird structure
* Utils was split up and reorganized
* Some minor graphic improvements like outlines and taller walls

## 0.1.6.0 -- 2019-05-16

* Palette sets can be loaded and are fully functional, changing all colors in a scene
* All horizontal fields within the player's view are drawn, even if the player isn't within them
* Debug mode now changes on key just pressed, so that it doesn't scroll or toggle too quickly

### 0.1.6.1 -- 2019-05-16

* Health and mana are now drawn in the top left corner
* Layers in a map are now drawn; there are slight overlaying issues with floors as of now

### 0.1.6.2 -- 2019-06-07

* Entities can now be placed in world spaces
* Additional color shader functions and fixes
* World now keeps track of in-game time of day

## 0.1.5.0 -- 2019-05-09

* World Spaces can now be properly loaded and drawn to the screen; currently only the field in which the player is in is drawn
* The world can contain dropped items now; picking them up is not yet implemented
* Keyboard controls can be customized using the settings.config file; you can set each with the appropriate SDL Scancode integer value
* Added debug option; to allow, set can-use-debug to true in settings.config
  While enabled, you can use Numpad + and Numpad - to zoom in or out, and use F5 to switch to the next display palette
* Stripped unneccessary data from .png files; this should fix warnings when Libpng-1.6 is used

### 0.1.5.1 -- 2019-05-09

* Added support for loading filters from JSON
* Units for items and entities in maps now use 1.0 for each tile, rather than 16.0

## 0.1.4.0 -- 2019-05-06

* Moved render system to a contained data type
* Render system can load palettes and switch between them to change the colors of the displayed sprites
* Additional parsing utilities
* Damage types can now have defaul resistances for when an actor doesn't specify one

### 0.1.4.1 -- 2019-05-07

* Objects on screen slightly decreases for small screen sizes, to prevent it from looking too zoomed out
* Offscreen sprites are no longer rendered
* Animations change faster from starting and stopping to prevent issue where rapid starting and stopping results in no animation
* Fixed entities facing north when first spawned, rather than south as intended
* Fixed issue with palettes past index 0 not fully updating sprites with background colors
* Fixed streaks between adjacent sprites

### 0.1.4.2 -- 2019-05-07

* Fixed sprites being hidden when very close but not entirely offscreen from the top
* Fixed player not being fully centered on some screen sizes
* Refactored Util modules
* Additional parsing for world configuration files

## 0.1.3.0 -- 2019-04-24

* Implemented surface depth sorting

## 0.1.0.0 -- 2019-01-28

* First release