# Glorious Labor Registry for Papers Please

This is a simple curses-based program to manage a group of people playing the beautiful game [Papers Please](http://papersplea.se/) cooperatively.

Basically, you make a list of people, a list of tasks, and then randomly assign tasks to people.
Each person marks YES or NO for their specific tasks, quickly alerting the person at the mouse/keyboard as to what is wrong.

Both a web API (intended for a mobile phone or similar) and controllers (Xbox 360 ones intended) are supported for the players.

![Screenshot of the labor registry](https://raw.githubusercontent.com/mtolly/glory/master/screenshot.png)

## Controls

  * `A` adds a web code player
  * `3` adds a 360 controller player
  * `Delete` deletes a player
  * `+` adds a task
  * `-` deletes a task
  * `L` runs the lottery, assigning tasks to players randomly
  * `V` starts a vote, where players use their controls to settle a choice up for debate
  * `Space` starts an inspection, where players use their controls to perform their assigned tasks
  * `C` gives a citation to a player
  * `1` chooses a random player, e.g. to come up and control the tranquilizer gun

## Tech/Thanks

  * Haskell, my one true programming love
  * [GHC's `RecordWildCards` extension](https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html), making immutable state easy
  * SDL2 and SDL2_mixer, for audio and joystick input
  * [Warp](http://hackage.haskell.org/package/warp-3.0.13.1/docs/Network-Wai-Handler-Warp.html), a dead-simple Haskell web server
  * [`vty`](http://hackage.haskell.org/package/vty), making curses functional/bearable
