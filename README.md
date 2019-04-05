Schelling's Model of Segregation

Overview:
This was a final project for my Functional Programming class. We were tasked with creating an appliction using Haskell
that demonstrated Schelling's Model of Segregation. In this application, we have a grid of a given size that is occupied
with red and blue residents with a given threshold of diversity that they prefer. Each turn, a person who is not at the
given threshold looks to see if there is an occopied location that would be more diverse. If it's above their threshold, 
they move. 

How to Run:
In terminal run: 

Text Mode:
smhs -t  <grid file> <R> <threshold> <max_steps>

GUI Mode
smhs <grid size> <red_percentage> <blue_percentage> <empty_percentage> <max_steps>

This application requires Haskell Gtk and GI to be installed, as that is used for the GUI.
https://github.com/haskell-gi/haskell-gi
