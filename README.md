# Slippery

1D sliding-block model of earthquake rupture/interaction.  Simulates a strike-slip
fault using a variable number of linked sliding blocks.  Blocks move when net
force exceeds frictional resistance (stick/slip).  Block interactions extend two
blocks in either direction for force computations.  Interactions extend further.

Block interactions, static friction, size distribution and number are user
selected.  Here size is a interchangeable proxy for fault strength--
a long block is equivalent to a strong block (high friction).

Most parameters are chosen to make results realistic.  They are not derived from
observation.  This is an analog model after all...  Use it to study the development of
complex behavior in simple systems, or for basic fault dynamics concepts.  

Developed and works with:

R 3.4.3

shiny 1.0.5

shinythemes 1.1.1

tidyverse 1.2.1

magrittr 1.5

RColorBrewer 1.1-2
