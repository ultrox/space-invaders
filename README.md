## Classic Game: Space Invaders

This is my attempt to recreate minimum features set of space invaders in Scheme
(dialect of Lisp).

## Features
- User can interact with tank, move it left/right and shoot
- User can kill enemy ships by shooting projectiles
- User can see score how many enemy ships killed
- Use will win if kill 100 ships
- User will lose if any of ships touch the 'ground'

# Tasks:

## 0 Setup boilerplate
- Translate domain-analysis Constants to syntactically correct code
- Setup minimum working example with big-bang with stubs
- make sure your boilerplate run correctly

## 1 User Can interact with tank, move it left/right and shoot
- Make data definitions for tank
- Write key-handler function to consume Tank moving l/r, when arrows are pressed
  connect it
  with BIG-BANG
- Extend key-handler and shoot project when Space is pressed


## 2 User will win if kill 100 ships
- Make GST data definition to setup tracking
- Make function that consume GST
- Make flashing MSG "YOU WON!!"
- Test & setup with main
- 3 needs to be done


## 3 User can kill enemy ships by shooting projectiles
- 1 needs to be done
- detect collision and remove ship from screen


## User can see score how many enemy ships killed
- When 3 is done, this is trivial

## User will lose if any of ships touch the 'ground'
- detect collision between ENEMY & ground
- Update Score, notify user, END game
- related to 3, can be done in parallel
