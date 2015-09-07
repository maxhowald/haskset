The game of Set, as a multiplayer webapp, written in haskell. 
Built with Yesod, an excellent web framework for haskell. 

Most of the code for the server related functions is in
Setserve.hs. It contains the http request handler functions, the
database related code, and the websockets handler that does most of
the core gameplay stuff.

SetAssets.hs exports the functions and datatypes that implement the
core game logic of set. It should be fairly readable even if you know
no Haskell at all.

The hamlet, julius, and lucius files are html, javascript, and css
template files resepctively. See play_page.html for an example
gameplay page.


The game uses websockets for realtime, low-latency communication,
together with Software Transactional Memory (STM) and Channels to
handle multiple players.

See INSTALLING.txt for instructions on building and running. 

Helpful link:
https://github.com/nbartlomiej/tfoo

An implementation of Five in a Row online with Haskell and Server-sent
Events (instead of websockets.) A lot of the code for managing global,
shared state with Yesod was repurposed from here.

Note: This code is released with a BSD license, but the game of Set is
copyright someone else, so beware. 


