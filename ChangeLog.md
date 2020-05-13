# Revision history for kathu

## 0.1.11.0 -- 2020-04-24
* Added map editor mode enabled by starting the program with either the --editor argument, or by passing in a .world file as an argument
* Editor can load worlds into the game and play them; saving isn't supported yet
* Editor has free camera mode, which pauses the game and can move/zoom using the middle mouse press or by scrolling
* Added JSON instances for many worldspace related types
* Palettes are now created by .palette files instead of being embedded in worldspaces
* Added misc new sprites and updated game icon
* Minor bug fixes and code cleanup

### 0.1.11.1 -- 2020-04-26
* Overhauled control processing: now mouse buttons can be freely assigned as controls and mixed with keys
* Mouse position and movement is kept as a global entity
* Scripts can access the mouse's position in the world with getCursorPosition()
* Additional toolmode changes and some render code cleanup

### 0.1.11.2 -- 2020-04-30
* The editor can now switch between tool modes; some modes enable the tile grid or free camera
* One new tool is the tile drawer, where tiles can be selected in the editor and placed in the world
* Entity instances in worldspaces can be given IDs

### 0.1.11.3 -- 2020-05-10
* The editor can now save worldspace files
* The editor now displays an example line from the beginning to the target when drawing a line of tiles
* The editor can now display script paths and modify their event listeners
* Change .world files to use many tile fields instead of one giant config, preventing origin changing issues
* .world tile legends can now be auto-generated
* Added module for serializing "pretty" yaml files

### 0.1.11.4 -- Unreleased
* Debug collision bounds are now colored according to their collision group

## 0.1.10.0 -- 2020-01-27
* Lua scripts can now be loaded and ran from files
* Scripts can be attached to entities and worldspaces
* Scripts are able to get and set most entity components, get global and unique ones, and change the world's palette
* Scripts can be configured for different events to run when they occur
* Identifiers are now pre-hashed for faster lookup

### 0.1.10.1 -- 2020-02-05
* Lua scripts can now check if debug mode is enabled, and can get and set global and world variables
* Lua scripts can be set to be singletons, and will use a shared script instance among all objects marked as such
* Fixed scripts not logging exceptions through execFor/runFor
* Fixed LifeTime roll-over issue causing timed-out entities to not be deleted

### 0.1.10.2 -- 2020-02-11
* Scripts can register listeners on global and world variables that will be called when the variable changes
* Added on-init and on-destroy script events
* Scripts can set animation state
* Fixed issue where singleton scripts would get closed prematurely
* More tiles and decorations were added

### 0.1.10.3 -- 2020-03-16
* Each sprite in an object's render can have a layer given to allow for background/foreground sprites
* Entity instances in the world can have signal emitters/receivers to call script events
* Collision groups now work, and movement-sensor was added that detects movement, but doesn't block anything
* Added on-signal-chance, on-sensor-collision-begin, and on-sensor-collision-end script events

### 0.1.10.4 -- 2020-03-21
* Entity instances can have custom config variables that can be read by scripts
* Scripts can now set collision filters for an entity's shapes and can filter out certain shapes based on tag
* Opened doors no longer block collision, and buttons have their mass threshold set at the instance itself

## 0.1.9.0 -- 2019-12-15

* SDL rendering is done through textures instead of surfaces, significantly reducing CPU at the cost of GPU
* The ImageManager now has no limit besides system memory, making it easy to support thousands of themes without too much memory overhead. For most sprites, only 8 bytes are used per palette
* The ImageManager now makes use of PaletteManagers to control which palettes it is using
* PaletteManagers may be animated from keyframes, with customizable interpolation, looping, and switching behavior
* Debug collisions are now drawn with lines, resulting in exact collision shapes being drawn
* The tile collision generator no longer triangulates rectangles, as we can be assured they are convex
* Fixed issue with worlds with more than one horizontal field having repeating tiles
* The player can hold shift to focus to slow down movement
* The player's sprite now has more frames; still mostly programmer art though

### 0.1.9.1 -- 2019-12-21

* New text rendering system that is easier on the GC; still needs improvements
* Added support for window icons
* Overhaul of game's base palette; now uses a modified version of Davit Masia's MATRIAX8C
* Mana and magic is now blue instead of green, to prevent issues with deuteranopia and add more contrast when using extreme map shaders
* Fixed bug where player would occasionally face wrong direction when walking after quickly changing directions
* Started adding support for languages and custom UI's

### 0.1.9.2 -- 2019-12-22

* Added support for loading fonts from files; fonts are defined in .lang files
* Languages are also supported, although replacing text with it isn't yet supported
* Fixed issue with displaying and printing unicode characters

### 0.1.9.3 -- 2020-01-19

* Tiles arranged so as to create shapes with holes in them now correctly have walkable collision inside of the holes
* Config file formats are now determined by adding ~FORMAT before the .EXTENSION
* Added support for parsing some UI stuctures to use for future file-config-based game UIs
* Added color util functions for brightness/darkness

## 0.1.8.0 -- 2019-11-17

* The game now has physics! Entities can specify their collision shapes and mass, density, friction, elasticity, and collision group
* Entities can now have lifetimes, and destroy themselves when time has passed
* The world itself also has collision; further improvements will be needed to improve sliding along tile walls
* The world now makes use of 1 unit per tile, instead of 16 units per tile, making logic easier; in addition, positions are now double precision
* The world is no longer separated into foreground and background layers
* New config files with the .floor extension were added to specify how floor friction works
* New props and items were added
* Two shader options were added: set forces everything to a certain color, and match-nearest fits colors to their closest match in a given palette
* Basic font support has been added
* When debugging, world, player position, and collision boxes are shown
* Added debug key F7 to print the physic properties of all identified entities

### 0.1.8.1 -- 2019-11-19

* Tile collision now merges and triangulates to prevent issues with getting stuck on tile boundaries
* Reorganized tests and cleaned up some outdated comments and commented-out code

## 0.1.7.0 -- 2019-09-16

* A major refactor was done to clean up code and eliminate warnings and redundancies
* The library was fully split into two parts: kathu-lib and kathu app
* kathu-lib provides the game's main logic, is system-independent, and doesn't make use of SDL, pointers, etc.
* kathu app forms the code for turning kathu-lib into a desktop executable
* IO.Parsing was eliminated, as was SystemLink; a new Dependency system was added to ease up on the orphan instances and weird structure
* Utils was split up and reorganized
* Some minor graphic improvements like outlines and taller walls

### 0.1.7.1 -- 2019-09-17

* Tiles can now be animated just like entities; they will currently only ever play the first animation strip in their configuration
* Tiles can now be given random variants by setting 'should-choose-render-randomly' to true, and supplying it with multiple renders
* Doors have been added, although they currently are non-functional
* Grass now has random sprites, and waves in the wind
* WorldSpace tile information now uses unboxed tilestates to save memory and add ease to animation
* Re-added the Inventory component to entities

### 0.1.7.2 -- 2019-09-19

* The world logic is now 2D-based, rather than 3D
* Worlds can load in variables associated with them

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