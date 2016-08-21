CodeWorld Blocks
================


Building a program
==================

In order to create a program we need to instruct it where to start.
When the program loads it searches for a program block, these can be one of:

* drawingOf - Creates a drawing of a picture
* animationOf - Creates a drawing, with steps to make a new one for each time
  interval
* simulationOf - Similar to the animation, but it allows you to store
  information about the current state of the program.
* interactionOf - Similar to a simulation, but allows you to specify how the
  program changes on user input such as mouse movement or the press of a key. 

For a simple drawing a picture can be attached to drawingOf:

<xml><block type="cwDrawingOf" x="10" y="10"><value
name="VALUE"><shadow type="cwBlank"></shadow><block
type="cwCodeWorldLogo"></block></value></block></xml>

Here we drag the codeWorldLogo picture block and connect it to the drawingOf
block to form a program.


Pictures
--------

`drawingOf` takes a single block as input. This input block should be a picture.
You can make different types of pictures such as:

* `circle`: Play around with the *radius* to get a feel for how different sizes
  look on the screen.
* `solidCircle`: Use `solidCircle` instead of `circle`, and your circle will be
  filled in.
* `rectangle`:  You can draw a rectangle by giving both a width and a height.
* `solidRectangle`: Just like with circles, you can use `solidRectangle` to
  fill in the shape.
* `text("I Love Pandas!")`: You can write text (such as letters and words) to the
  screen by using `text`.  

There are plenty more of these which can be found in the Pictures toolbox.

Connecting blocks
------------

You might have noticed that blocks usually take other blocks as inputs. Some
blocks have default inputs:


<xml><block type="cwCircle" x="10" y="10"><value name="RADIUS"><shadow type="numNumber">
     <field name="NUMBER">5</field></shadow></value></block>
</xml>


But you can easily replace them with different values or attach other blocks:

<xml><block type="cwCircle" id="/Q0fw[{f@E:J0gR||8qA" x="12"
y="12"><value name="RADIUS"><shadow type="numNumber"
id="C]#wm1_YL2,Av,zg-Qm1"><field name="NUMBER">5</field></shadow><block
type="numAdd" id="U4M9YP{|CQq|~xasJfrB" ><value
name="LEFT"><shadow type="numNumber" id="9=%.c*E?@h_Mu}u;.g9{"><field
name="NUMBER">1</field></shadow></value><value name="RIGHT"><shadow
type="numNumber" id=")Ln0Ap[AgmmHL{:1Y/wN"><field
name="NUMBER">2</field></shadow></value></block></value></block></xml>

To create a block simply drag it from the toolbox to the workspace. If you want
to connect a block to another drag it to the input. This is usually indicated by
an indent on the block.

Each block has a type. You can only connect a block to an input if they are the
same type. You can easily see if they are the same type by the connector shapes.
If the shapes match then you can connect the block.

<xml><block type="cwDrawingOf" id=";K:F=))!Q%~JPr4~9+Bm" x="6" y="10"><value
name="VALUE"><shadow type="cwBlank" id="DaNDD+9h2L-0@oc?Ux[c"></shadow><block
type="cwRotate" id="=1}hVAJm%S:PTHQm|*LA"><value name="PICTURE"><shadow
type="cwBlank" id="v+p=kkC?[Xen[?}P#JJ^"></shadow></value><value
name="ANGLE"><shadow type="numNumber" id="G.rMkdmcxB74!#Oi=NHr"><field
name="NUMBER">0</field></shadow></value></block></value></block><block
type="cwCoordinatePlane" id="||@WyV^pnCW2nEfbcN`Q" x="206"
y="102"></block><block type="cwRed" id="!rsFc*U]|*5Y?K8#tts4" 
x="206" y="139"></block></xml>

In the above program, we can connect the coordinatePlane block to the first
input of the rotated block. This creates the program:

<xml><block type="cwDrawingOf" id=";K:F=))!Q%~JPr4~9+Bm" x="6" y="10"><value
name="VALUE"><shadow type="cwBlank" id="DaNDD+9h2L-0@oc?Ux[c"></shadow><block
type="cwRotate" id="=1}hVAJm%S:PTHQm|*LA"><value name="PICTURE"><shadow
type="cwBlank" id="v+p=kkC?[Xen[?}P#JJ^"></shadow><block
type="cwCoordinatePlane" id="||@WyV^pnCW2nEfbcN`Q"></block></value><value
name="ANGLE"><shadow type="numNumber" id="G.rMkdmcxB74!#Oi=NHr"><field
name="NUMBER">0</field></shadow></value></block></value></block></xml>

But we can't connect red to the picture input because the types don't match !
