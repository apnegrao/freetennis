#Free Tennis simulator

Tennis simulator for Linux based on Free Tennis (http://freetennis.sourceforge.net/) by Maurizio Colucci - check the original website for more info. Forked from Debian's repositories.

Description of the original game taken from Debian repositories:
> Free Tennis is a free software tennis simulation game. The game can be played against an A.I. or human-vs-human via LAN or internet. 

##Main Changes
- Player movement is controlled using the keyboard, instead of the mouse. Ball placement and speed are still controlled using the mouse.
- The following commands were assigned to different keys:
  * Dive: key 'e'
  * Sprint: key 'shift'
  * Slice: key 'space'
- Code was restructured and refactored.

##Installation
Download and extract the zip file. From the terminal, 'cd' to the extracted dir and run 'make'.

##How to Play
To start playing, open a terminal, go to the game dir and run one of the following commands:
   './freetennis -newbie'
   './freetennis -realistic' 
With the newbie option, the game shows the parabola corresponding to the player's shot, allowing him to see the trajectory and placement of the ball before the shot is made. This is recommended for...newbies.  
With the realistic option, the parabola is not visible, except on a few situations. Check section "Making the game realistic" of the game's [manual](http://freetennis.sourceforge.net/manual).  
There are several other optional command line parameters available. Type './freetennis -help' to see the full list.

After the game starts, press 'g' to attach the mouse to the game or 'f' to play in fullscreen.

#####Serving
To hit a serve press either the right or left mouse button after the player tosses the ball. Left clicks are for first serves (powerfull and more difficult to control). A right click will play a slower and safer shot, appropriate for second serves. 
Moving the mouse right or left will change the direction of the shot accordingly, but it is currently too sensitive (small movements will cause big changes in direction).

#####Player movement
The movement of the player is controlled by the following keyboard keys:
- 'w': move the player forward/upward.
- 's': move the player backwards/downward.
- 'a': move the player to the left.
- 'd': move the player to the right.
- 'shift': makes the player move faster. The player will waste energy when sprinting. A vertical yellow bar on the right side of the screen shows the energy left. When the bar disapears, the player will no longer be able to sprint.
- 'e': Causes the player to dive to hit a ball that it would normally not be able to. Diving is only possible when the player becomes yellow, a condition that is automatically set by the game.

#####Shot placement
The placement of the ball is controlled with the mouse:
- Move the mouse to change the direction of the trajectory.
- Press the left mouse button to flatten the trajectory (more speed)
- Press the right mouse button to increase the curvature of the trajectory (less speed. Good for lobs).  
To slice the ball press 'space' before hitting the ball.

Check the original game's manual for more detailed info, but keep in mind the different controls of the new version.
