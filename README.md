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
  connect it with BIG-BANG
3. Extend key-handler and shoot project when Space is pressed


## 2 User will win if kill 100 ships
1. Make GST(game state) data definition to setup tracking
2. Make function that consume GST
3. Make flashing MSG "YOU WON!!"
4. Test & setup with main
5. 3 needs to be done


## 3 User can kill enemy ships by shooting projectiles
1. 1 needs to be done
2. detect collision and remove ship from screen


## 4 User can see score how many enemy ships killed
1. When 3 is done, this is trivial

## 5 User will lose if any of ships touch the 'ground'
1. detect collision between ENEMY & ground
2. Update Score, notify user, END game
3. related to 3, can be done in parallel

## 6 Enemies
1. Ships are showing random position on 0 - WIDTH x-axis
2. Ships flying "down" on y-axis 
3. Ships can bounce out of edges in 45degrees
4. When ship touch bottom, flash msg "GAME OVER"

## General Suggestion to follow:
- When committing make sure all your tests pass, never commit with fail tests

