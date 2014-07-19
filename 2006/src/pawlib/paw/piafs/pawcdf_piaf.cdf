*
* $Id: pawcdf_piaf.cdf,v 1.3 1996/09/17 07:07:46 couet Exp $
*
* $Log: pawcdf_piaf.cdf,v $
* Revision 1.3  1996/09/17 07:07:46  couet
* - Some old QP action routine removed
*
* Revision 1.2  1996/09/09 09:28:39  couet
* - Old QP removed
*
* Revision 1.1  1996/04/02 21:47:11  thakulin
* Moved from cdf directory.
*
* Revision 1.1  1996/03/13 09:49:02  cremel
* Instead of having "cpp" directives into pawcdf.cdf, cut the file
* into 4 versions:
*
* 1) pawcdf.cdf:
* #if defined(CERNLIB_QP) (new Query Processor)
* #if !defined(CERNLIB_PIAF) (no PIAF)
*
* 2) pawcdf_old.cdf:
* #if !defined(CERNLIB_QP) (old Query Processor)
* #if !defined(CERNLIB_PIAF) (no PIAF)
*
*
* 3) pawcdf_piaf.cdf:
* #if defined(CERNLIB_QP) (new Query Processor)
* #if defined(CERNLIB_PIAF) (with PIAF code)
*
*
* 4) pawcdf_oldpiaf.cdf:
* #if !defined(CERNLIB_QP) (old Query Processor)
* #if defined(CERNLIB_PIAF) (with PIAF code)
*
* Revision 1.3  1996/03/12 16:57:01  cremel
* cpp run over the CDFs
*
* Revision 1.2  1996/03/11 17:24:01  couet
* Upgrade in the HELP of NT/LOOP
*
* Revision 1.1.1.1  1996/03/01 11:38:52  mclareni
* Paw
*
*
>Name HISDEF

>Menu HISTOGRAM
>Guidance
Manipulation of histograms, Ntuples.
Interface to the HBOOK package.

>Command FILE
>Parameters
LUN 'Logical unit number' I R=0:128
FNAME 'File name' C
+
LRECL 'Record length in words' I D=1024
CHOPT 'Options' C D=' '
-  Existing file is opened (read mode only).
-N A new file is opened.
-U Existing file is opened to be modified.
-D Reset lock.
>Guidance
Open an HBOOK direct access file.
If LUN is 0 the next free logical unit will be used.
If LRECL is 0 the system will determine the correct
record length of an existing file.
>Action PAHIST

>Command LIST
>Parameters
+
CHOPT 'Options' C D=' '
-  List histograms and Ntuples in the current directory.
-I A verbose format is used (HINDEX), (only for //PAWC).
-S List with histograms sorted by increasing IDs.
>Guidance
List histograms and Ntuples in the current directory.
>Action PAHIST

>Command DELETE
>Parameters
ID 'Histogram Identifier' C Loop
>Guidance
Delete histogram/Ntuple ID in Current Directory (memory).
If ID=0 delete all histograms and Ntuples.
To delete histograms in disk files use command HIO/HSCRATCH.
>Action PAHIST

>Command PLOT
>Parameters
+
ID 'Histogram Identifier' C Minus Loop
CHOPT 'Options' C D=' '
-       Draw the histogram.
-C      Draw a smooth curve.
-S      Superimpose plot on top of existing picture.
-+      Add contents of ID to last plotted histogram.
--      Substract contents of ID to last plotted histogram.
-+-     Draw the delta with the last plotted histogram.
-B      Select Bar chart format.
-L      Connect channels contents by a line.
-P      Draw the current polymarker at each channel or cell.
-*      Draw a * at each channel.
-K      Must be given if option 'U' is given later.
-U      Update channels modified since last call.
-E      Draw error bars and current marker.
-E0     Draw error bars without symbols clipping.
-E1     Draw small lines at the end of the error bars.
-E2     Draw error rectangles.
-E3     Draw a filled area through the end points_
        of the vertical error bars.
-E4     Draw a smoothed filled area through the end points_
        of the vertical error bars.
-A      Axis labels and tick marks are not drawn.
-BOX    Draw 2-Dim with proportional boxes.
-COL    Draw 2-Dim with a color table.
-Z      Used with COL or SURF, it draws the color map.
-SURF   Draw as a surface plot (angles are set via the command angle).
-SURF1  Draw as a surface with color levels
-SURF2  Same as SURF1 but without cell lines.
-SURF3  Same as SURF but with the contour plot (in color) on top.
-SURF4  Draw as a surface with Gouraud shading.
-LEGO   Draw as a lego plot (angles are set via the command angle).
-LEGO1  Draw lego plot with light simulation.
-LEGO2  Draw lego plot with color levels.
-BB     Suppress the Back Box on 3D plots.
-FB     Suppress the Front Box on 3D plots.
-CONT   Draw 2-Dim as a contour plot (15 levels).
-TEXT   Draw 2-Dim as a table.
-CHAR   Draw 2-Dim with characters (a la HBOOK).
-HIST   Draw only histogram (no errors or associated function).
-FUNC   Draw only the associated function (not the histogram).
-CYL    Cylindrical coordinates for 3D plots.
-POL    Polar coordinates for 3D plots.
-SPH    Spherical coordinates for 3D plots.
-PSD    Pseudo-rapidity/phi coordinates for 3D plots.
>Guidance
Plot a single histogram or a 2-Dim projection.
If ID=0 or ID=* all the histograms in the current directory are plotted.
Each plotted histogram will start either a new picture or a new zone in
the current picture.

 Histogram subranges can be specified in 2 different ways:
  1- h/pl id(ic1:ic2) with ic1 and ic2 integers means plot
                      from channel ic1 to channel ic2
  2- h/pl id(x1:x2)   with x1 and x2 reals (with a .) means plot
                      from channel corresponding to x1
  Note that the mixed mode h/pl id(x1:ic2) is also accepted
  This subrange works also for 2-DIM cases.
  Ex: Histo/plot 10(25:1.) or Histo/plot 20(4:18,0.:0.5).

A specific histogram cycle can be accessed:
  PAW > h/pl id;nc | cycle number nc is used (default is highest cycle)

1 Dim histograms could be plotted with option LEGO or SURF.
In this case the angles are THETA=1 and PHI=-1.
.
When option 'E' is used, the marker type can be changed with SET MTYP,
the marker size with SET KSIZ, the marker color with SET PMCI.
.
With Option E1, the size of the tick marks at the end of the error
bars is equal to the marker size and can be changed with SET KSIZ.
.
When the option E is used with the option SURF1, SURF2, SURF3 or LEGO1,
the colors are mapped on the errors not on the content of the histogram.
.
 To plot projection X of ID type
   PAW > HI/PLOT ID.PROX
 To plot band 1 in Y of ID type
   PAW > HI/PLOT ID.BANY.1
 To plot slice 3 in Y of ID type
   PAW > HI/PLOT ID.SLIY.3
.
In addition to the Cartesian coordinate systems, Polar, cylindrical,
spherical, pseudo-rapidity/phi coordinates are available for LEGO and
SURFACE plots, including stacked lego plots.
For example:
      PAW > Histo/plot 10+20+30  LEGO1,CYL | stacked cylindrical lego plot
      PAW > Histo/plot 10+20+30  LEGO1,POL |         polar
      PAW > Histo/plot 10+20+30  LEGO1,SPH |         spherical
      PAW > Histo/plot 10+20+30  LEGO1,PSD |         pseudo-rapidity/phi
Note that the viewing angles may be changed via the command ANGLES.
The axis, the front box, and the back box can be suppressed on 3D plots
with the options 'A', 'FB' and 'BB'.
>Action PAHIST

>Command ZOOM
>Parameters
+
ID 'Histogram Identifier' C Minus Loop
CHOPT 'Options' C D=' '
-   Plot the zoomed histogram.
-C  Draw a smooth curve.
-S  Superimpose plot on top of existing picture.
-+  Add contents of ID to last plotted histogram.
-B  Select Bar chart format.
-L  Connect channels contents by a line.
-P  Draw the current polymarker at each channel.
-*  Draw a * at each channel.
ICMIN 'First channel' I D=1
ICMAX 'Last channel'  I D=9999
>Guidance
Plot a single histogram between channels ICMIN and ICMAX.
Each plotted histogram will start either a new picture or a new zone
in the current picture.
If no parameters are given to the command, then the system waits
for two points using the graphics cursor.
To quit ZOOM, click the right button of the mouse or CRTL/E.
>Action PAHIST

>Command MANY_PLOTS
>Parameters
IDLIST 'List of histogram Identifiers' C VARARG
>Guidance
Plot one or several histograms into the same plot.
Plotted histograms are superimposed on the same zone
of the picture.
>Action PAHIST

>Command PROJECT
>Parameters
ID 'Histogram Identifier' C Loop
>Guidance
Fill all booked projections of a 2-Dim histogram.
Filling is done using the 2-D contents of ID.
>Action PAHIST

>Command COPY
>Parameters
ID1 'First histogram Identifier' C
ID2 'Second histogram Identifier' C Loop
+
TITLE 'New title' C D=' '
>Guidance
Copy a histogram onto another one.
Bin definition, contents, errors, etc. are preserved.
If TITLE is not given, ID2 has the same title as ID1.
.
It is possible to copy a projection of a 2D histogram
into a 1D histogram.
.
Ranges can be specify in the first histogram identifier to
reduce or enlarge the X or Y scale.
.
Example:
    Fun2 2 x*y 40 0 1 40 0 1 ' '  | Create a 2D histogram
    Slix 2 10 ; H/proj 2          | Slices on X
    H/Copy 2.slix.3 3             | Copy the slice 3
    H/Copy 2(0.:.5,-1.:2.) 4      | Copy with new X and Y scales
>Action PAHIST

>Command FIT
>Parameters
ID 'Histogram Identifier' C
FUNC 'Function name' C D=' '
+
CHOPT 'Options' C D=' '
-   Do the fit, plot the result and print the parameters.
-0  Do not plot the result of the fit. By default the fitted _
    function is drawn unless the option 'N' below is specified.
-N  Do not store the result of the fit bin by bin with the histogram._
    By default the function is calculated at the middle of each bin_
    and the fit results stored with the histogram data structure.
-Q  Quiet mode. No print
-V  Verbose mode. Results after each iteration are printed_
    By default only final results are printed.
-B  Some or all parameters are bounded. The vectors STEP,PMIN,PMAX_
    must be specified. Default is: All parameters vary freely.
-L  Use Log Likelihood. Default is chisquare method.
-D  The user is assumed to compute derivatives analytically using the_
    routine HDERIV. By default, derivatives are computed numerically.
-W  Sets weights equal to 1. Default weights taken from the square root_
    of the contents or from HPAKE/HBARX (PUT/ERRORS)._
    If the L option is given (Log Likelihood), bins with errors=0_
    are excluded of the fit.
-M  The interactive Minuit is invoked. (see Application HMINUIT below).
-E  Performs a better Error evaluation (MIGRAD + HESSE + MINOS).
-U  User function value is taken from /HCFITD/FITPAD(24),FITFUN.
-K  Keep the settings of Application HMINUIT for a subsequent command.
NP 'Number of parameters' I D=0 R=0:34
PAR 'Vector of parameters' C
STEP 'Vector of steps size' C
PMIN 'Vector of lower bounds' C
PMAX 'Vector of upper bounds' C
ERRPAR 'Vector of errors on parameters' C
>Guidance
Fit a user defined (and parameter dependent) function
to a histogram ID (1-Dim or 2-Dim) in the specified range.
FUNC may be:
 A- The name of a file which contains the user defined
    function to be minimized. Function name and file name
    must be the same. For example file FUNC.FOR is:
      FUNCTION FUNC(X)   or FUNC(X,Y) for a 2-Dim histogram
      COMMON/PAWPAR/PAR(2)
      FUNC=PAR(1)*X +PAR(2)*EXP(-X)
      END
     Ex: His/fit 10 func.for ! 5 par
.
    When the option U is given, the file FUNC.FOR should look like:
      FUNCTION FUNC(X)   or FUNC(X,Y) for a 2-Dim histogram
      DOUBLE PRECISION FITPAD(24),FITFUN
      COMMON/HCFITD/FITPAD,FITFUN
      FITFUN=FITPAD(1)*X +FITPAD(2)*EXP(-X)
      FUNC=FITFUN
      END
.
 B- One of the following keywords (1-Dim only):
    G : to fit Func=par(1)*exp(-0.5*((x-par(2))/par(3))**2)
    E : to fit Func=exp(par(1)+par(2)*x)
    Pn: to fit Func=par(1)+par(2)*x+par(3)*x**2......+par(n+1)*x**n
     Ex: His/fit 10 g
.
 C- A combination of the keywords in B with the 2 operators + or *.
    Ex: His/Fit 10 p4+g ! 8 par
        His/Fit 10 p2*g+g ! 9 par
      Note that in this case, the order of parameters in PAR must
      correspond to the order of the basic functions.
      For example, in the first case above, par(1:5) apply to
      the polynomial of degree 4 and par(6:8) to the gaussian while
      in the second case par(1:3) apply to the polynomial of degree 2,
      par(4:6) to the first gaussian and par(7:9) to the second gaussian.
      Blanks are not allowed in the expression.

For cases A and C, before the execution of this command, the vector PAR
must be filled (via Vector/Input) with the initial values.
For case B, if NP is set to 0, then the initial values of PAR
will be calculated automatically.
After the fit, the vector PAR contains the new values
of parameters. If the vector ERRPAR is given, it will contain
the errors on the fitted parameters.
A bin range may be specified with ID.
  Ex. Histo/Fit 10(25:56).
.
When the Histo/it command is used in a macro, it might be convenient
to specify MINUIT directives in the macro itself via the Application
HMINUIT as described in this example:
     Macro fit
     Application HMINUIT exit
     name 1 par_name1
     name 2 par_name2
     migrad
     improve
     exit
     Histo/fit id fitfun.f M
     Return
>Action PAFITH

>Menu 2D_PLOT
>Guidance
Plotting of 2-Dim histograms in various formats.

>Command LEGO
>Parameters
+
ID 'Histogram Identifier' C Loop
THETA 'Angle THETA in degrees' R D=30.
PHI 'Angle PHI in degrees' R D=30.
CHOPT 'Options' C D=' '
-  Hidden line algorithm is used.
-1 Hidden surface algorithm is used. The colour of the lego_
   is given by SET HCOL CI where CI is a colour index. For the_
   top and the sides of the lego the same hue is used but with_
   a different light.
-2 Hidden surface algorithm is used. The colour of each bar_
   changes according to the value of Z. It is possible to change_
   the set of colours used with SET HCOL c.L where L define a palette_
   of colours given by the command ATT/PALETTE.
>Guidance
Draw a lego plot from 2-Dim or 1-Dim histograms.
It is also possible to produce stacked lego plots. A stacked lego plot
consists of a superimposition of several histograms, whose identifiers are
given in the command LEGO separated by the character '+'.

 PAW > LEGO ID1+ID2+ID3    | Maximum number of ID's is 10. The colours of
                           | each IDn is given by the command ATT/PALETTE
Examples:

 PAW > SET HCOL 2          | The colour the histogram is 2 (red)
 PAW > LEGO 20             | Display a lego with lines
 PAW > LEGO 20 ! ! 1       | Display a lego with different lights
 PAW > LEGO 20 ! ! 2       | Display a lego with colours

 PAW > PALETTE  1 3 2 3 4  | Create the palette number 1 with 3
                           | elements: 2,3
 PAW > SET HCOL 0.1        | The subsequent stack lego plots will use list 1
 PAW > LEGO 10+20+30       | Plot a stack of lego plots with lines
 PAW > LEGO 10+20+30 ! ! 1 | Plot a stack of lego plots with light

Notes: - The commands OPTION BAR, SET BARW and SET BARO act on lego plots
       - The options 1 and 2 must be used only on selective erase
         devices.
>Action PAHIST

>Command SURFACE
>Parameters
+
ID 'Histogram Identifier' C Loop
THETA 'Angle THETA in degrees' R D=30.
PHI 'Angle PHI in degrees' R D=30.
CHOPT 'Options' C D=' '
-  Hidden line algorithm is used.
-1 Hidden surface algorithm is used and each cell is filled_
   with a colour corresponding to the Z value (or grey scale_
   with PostScript). It is possible to change the set of colours_
   used with SET HCOL ic.L where L define a palette of colours_
   given by the command ATT/PALETTE.
-2 Similar to option '1' except that the cell lines_
   are not drawn. This is very useful to draw contour_
   plots with colours if THETA=90 and PHI=0.
-3 Surface is drawn with a contour plot in color on top. The_
   contour plot is drawn with the colors defined with the command_
   PALETTE.
-4 Surface is drawn with Gouraud shading.
>Guidance
Draw a surface plot from 2-Dim or 1-Dim histograms.
With this command it is possible to draw color contour plots:
    PAW > ATT/PAL 1 3 2 3 4   | Define the palette 1 with 3 elements
    PAW > SET HCOL 0.1        | Set the list 1 as colours for histograms
    PAW > SET NDVZ 4          | Set the number of Z divisions to 4
    PAW > SURF id 90 0 2      | Draw the contour
.
Note: - The options 1 to 4 must be used only on selective erase devices.
>Action PAHIST

>Command CONTOUR
>Parameters
+
ID     'Histogram Identifier'     C Loop
NLEVEL 'Number of contour lines'  I D=10
CHOPT  'Options'                  C D='1'
-0 Use colour to distinguish contours.
-1 Use line style to distinguish contours.
-2 Line style and colour are the same for all contours.
-3 The contour is drawn with filled colour levels. The_
   levels are equidistant. The color indices are taken_
   in the current palette (defined with the command PALETTE)._
   If the number of levels (NLEVEL) is greater than the number_
   of entries in the current palette, the palette is explore_
   again from the beginning in order to reach NLEVEL.
-S Superimpose plot on top of existing picture.
PARAM  'Vector of contour levels' C
>Guidance
Draw a contour plot from a 2-Dim histogram.
If PARAM is not given, contour levels are equidistant.
If given, the vector PARAM may contain up to 50 values.
.
Example:
    Fun2 2 x*y 40 0 1 40 0 1 ' '   | Create a 2D histogram
    V/Cr PAR(5) R .1 .11 .3 .31 .5 | Define the contours
    Contour 2 5 ! PAR              | Draw the non equidistant contours
.
Note: The non equidistant contours are not implemented with the option '3'.
>Action PAHIST

>Menu ../CREATE
>Guidance
Creation ('booking') of HBOOK objects in memory.

>Command 1DHISTO
>Parameters
ID 'Histogram Identifier' C Loop
TITLE 'Histogram title' C D=' '
NCX  'Number of channels' I D=100
XMIN 'Low edge' R  D=0.
XMAX 'Upper edge' R  D=100.
+
VALMAX 'Maximum bin content' R D=0.
>Guidance
Create a one dimensional histogram.
The contents are set to zero.
If VALMAX=0, then a full word is allocated per channel, else VALMAX
is used as the maximum bin content allowing several channels to be
stored into the same machine word.
>Action PAHBOK

>Command PROFILE
>Parameters
ID 'Histogram Identifier' C
TITLE 'Histogram title' C D=' '
NCX  'Number of channels' I D=100
XMIN 'Low edge in X' R D=0.
XMAX 'Upper edge in X' R D=100.
YMIN 'Low edge in Y' R D=-1.E30
YMAX 'Upper edge in Y' R D=1.E30
+
CHOPT 'Options' C D=' '
-  Error on mean
-S Spread option
>Guidance
Create a profile histogram.
Profile histograms accumulate statistical quantities of a variable y
in bins of a variable x. The contents are set to zero.
>Action PAHBOK

>Command BINS
>Parameters
ID 'Histogram Identifier' C
TITLE 'Histogram title' C D=' '
NCX  'Number of channels' I D=100
XBINS 'Vector of NCX+1 low-edges' C
+
VALMAX 'Maximum bin content' R D=0.
>Guidance
Create a histogram with variable size bins.
The low-edge of each bin is given in vector XBINS (NCX+1) values.
The contents are set to zero. See 1DHISTO for VALMAX.
>Action PAHBOK

>Command 2DHISTO
>Parameters
ID 'Histogram Identifier' C Loop
TITLE 'Histogram title' C D=' '
NCX  'Number of channels in X' I D=40
XMIN 'Low edge in X' R D=0.
XMAX 'Upper edge in X' R D=40.
NCY  'Number of channels in Y' I D=40
YMIN 'Low edge in Y' R  D=0.
YMAX 'Upper edge in Y' R D=40.
+
VALMAX 'Maximum bin content' R D=0.
>Guidance
Create a two dimensional histogram.
The contents are set to zero. See 1DHISTO for VALMAX.
>Action PAHBOK

>Command PROX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
>Guidance
Create the projection onto the x axis.
The projection is not filled until the Histo/Project command is executed.
 To plot projection X of ID type:
   PAW > HI/PLOT ID.PROX
>Action PAHBOK

>Command PROY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
>Guidance
Create the projection onto the y axis.
The projection may be filled with Histo/Project.
 To plot projection Y of ID type:
   PAW > HI/PLOT ID.PROY
>Action PAHBOK

>Command SLIX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
NSLICES 'Number of slices' I
>Guidance
Create projections onto the x axis, in y-slices.
The projection may be filled with Histo/Project.
 To plot slice 3 in X of ID type:
   PAW > HI/PLOT ID.SLIX.3
>Action PAHBOK

>Command SLIY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
NSLICES 'Number of slices' I
>Guidance
Create projections onto the y axis, in x-slices.
The projection may be filled with Histo/Project.
 To plot slice 2 in Y of ID type:
   PAW > HI/PLOT ID.SLIY.2
>Action PAHBOK

>Command BANX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
YMIN 'Low edge in Y' R
YMAX 'Upper edge in Y' R
>Guidance
Create a projection onto the x axis, in a band of y. Several bands can be
defined on the one histogram. The projection may be filled with
Histo/Project.
 To plot band 1 in X of ID type:
   PAW > HI/PLOT ID.BANX.1
>Action PAHBOK

>Command BANY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
XMIN 'Low edge in X' R
XMAX 'Upper edge in X' R
>Guidance
Create a projection onto the y axis, in a band of x. Several bands can be
defined on the one histogram. The projection may be filled with
Histo/Project.
 To plot band 1 in Y of ID type:
   PAW > HI/PLOT ID.BANY.1
>Action PAHBOK

>Command TITLE_GLOBAL
>Parameters
+
CHTITL 'Global title' C D=' '
CHOPT  'Options'      C D=' '
-  The global title is plotted at the top of each picture.
-U If the option 'UTIT' is on, a user title is plotted_
   at the bottom of each histogram.
>Guidance
Set the global title.
The size and the Y position of the global title may be changed
by the commands SET GSIZ and SET YGTI respectively. The size and
the Y position of the user title may be changed by the commands
SET TSIZ and SET YHTI respectively.
>Action PAHBOK

>Menu ../HIO
>Guidance
Input/Output operations of histograms.

>Command HRIN
>Parameters
ID 'Histogram Identifier' C Loop
+
ICYCLE 'Cycle number' I D=999
IOFSET 'Offset' I D=0
>Guidance
Read histogram/Ntuple ID from the current directory
on direct access file to memory.
An identical histogram is created but with an ID equal to that
of the original histogram plus the offset IOFSET.
Identifier may be '0' or '*' (for all histograms).
If ICYCLE > 1000 and ID=0 read all histograms in all subdirectories as well.
If IOFSET = 99999 then the contents of histogram ID on the disk file
are added to the current histogram in memory if it exists. For example
to add all histograms from FILE1 and FILE2 in memory, the sequence
of commands can be:
 PAW > Histo/File 1 FILE1
 PAW > Hrin 0
 PAW > Histo/File 2 FILE2
 PAW > Hrin 0 ! 99999
>Action PAHIO

>Command HROUT
>Parameters
ID 'Histogram Identifier' C Loop
+
CHOPT 'Options' C D=' '
-  Write histo/Ntuple ID from memory to current directory.
-T Writes all histograms in subdirectories as well.
>Guidance
Write histo/Ntuple ID from memory to current directory.
Identifier may be '0' or '*' (for all histograms).
>Action PAHIO

>Command HSCRATCH
>Parameters
ID 'Histogram Identifier' C Loop
>Guidance
Delete histogram ID in Current Directory on disk.
If ID='0' or '*' delete all histograms.
To delete histograms in memory use command HISTO/DELETE.
>Action PAHIO

>Command HFETCH
>Parameters
ID 'Histogram Identifier' C
FNAME 'File name' C
>Guidance
Fetch histogram ID from file FNAME.
FNAME has been created by the old version of HBOOK3 (Unformatted).
>Action PAHIO

>Command HREAD
>Parameters
ID 'Histogram Identifier' C
FNAME 'File name' C
>Guidance
Read histogram ID from file FNAME.
FNAME has been created by the old version of HBOOK3 (Formatted).
>Action PAHIO

>Command PRINT
>Parameters
ID 'Histogram Identifier' C Loop
+
CHOPT 'Options' C D=' '
-  Print histograms.
-S Only statistics (Number of entries, mean, RMS, underflow,_
   overflow) are printed.
>Guidance
Print histograms (line-printer format) on screen.
The command OUTPUT_LP may be used to change the output file.
>Action PAHIO

>Command DUMP
>Parameters
ID 'Histogram Identifier' C Loop
>Guidance
Dump the histogram ZEBRA data structure on the terminal.
>Action PAHIO

>Command OUTPUT_LP
>Parameters
+
LUN 'Logical unit number' I D=6
FNAME 'File name' C D=' '
>Guidance
Change the HBOOK 'line printer' file name.
If FNAME=' ' then OUTPUT is appended to an already opened file on unit LUN.
If LUN is negative, the file is closed and subsequent output
is directed to unit 6.
>Action PAHIO

>Command GLOBAL_SECT
>Parameters
GNAME 'Global section name' C D=' '
>Guidance
Map the global section GNAME.
The current directory is changed to //GNAME.
This command doesn't work on HPUX.
>Action PAHIO

>Command GRESET
>Parameters
ID 'Histogram Identifier' C
>Guidance
Reset histogram ID in the global section.
>Action PAHIO

>Menu ../OPERATIONS
>Guidance
Histogram operations and comparisons.

>Command ADD
>Parameters
ID1 'First histogram Identifier' C
ID2 'Second histogram Identifier' C
ID3 'Result histogram Identifier' C
+
C1 'Scale factor for ID1' R D=1.
C2 'Scale factor for ID2' R D=1.
OPTION 'Option' C D=' ' R=' ,E'
>Guidance
Add histograms: ID3 = C1*ID1 + C2*ID2.
Applicable to 1-Dim and 2-Dim histograms.
See command HRIN to add histograms with same IDS
from different files.
If option 'E' is set, error bars are calculated for ID3.
>Action PAHOPE

>Command SUBTRACT
>Parameters
ID1 'First histogram Identifier' C
ID2 'Second histogram Identifier' C
ID3 'Result histogram Identifier' C
+
C1 'Scale factor for ID1' R D=1.
C2 'Scale factor for ID2' R D=1.
OPTION 'Option' C D=' ' R=' ,E'
>Guidance
Subtract histograms: ID3 = C1*ID1 - C2*ID2.
Applicable to 1-Dim and 2-Dim histograms.
If option 'E' is set, error bars are calculated for ID3.
>Action PAHOPE

>Command MULTIPLY
>Parameters
ID1 'First histogram Identifier' C
ID2 'Second histogram Identifier' C
ID3 'Result histogram Identifier' C
+
C1 'Scale factor for ID1' R D=1.
C2 'Scale factor for ID2' R D=1.
OPTION 'Option' C D=' ' R=' ,E'
>Guidance
Multiply histogram contents: ID3 = C1*ID1 * C2*ID2.
Applicable to 1-Dim and 2-Dim histograms.
If option 'E' is set, error bars are calculated for ID3.
>Action PAHOPE

>Command DIVIDE
>Parameters
ID1 'First histogram Identifier' C
ID2 'Second histogram Identifier' C
ID3 'Result histogram Identifier' C
+
C1 'Scale factor for ID1' R D=1.
C2 'Scale factor for ID2' R D=1.
OPTION 'Option' C D=' ' R=' ,E'
>Guidance
Divide histograms: ID3 = C1*ID1 / C2*ID2.
Applicable to 1-Dim and 2-Dim histograms.
If option 'E' is set, error bars are calculated for ID3.
>Action PAHOPE

>Command RESET
>Parameters
ID 'Histogram Identifier' C Loop
+
TITLE 'New title' C D=' '
>Guidance
Reset contents and errors of an histogram.
Bin definition is not modified.
>Action PAHOPE

>Command DIFF
>Parameters
ID1 'First Histogram Identifier' C
ID2 'Second Histogram Identifier' C
+
CHOPT 'Options' C D='D'
-    The comparison is done only on the shape of the two histograms.
-N   Include also comparison of the relative normalisation of the_
     two histograms, in addition to comparing the shapes._
     PROB is then a combined confidence level taking account_
     of absolute contents.
-D   Debug printout, produces a blank line and two lines of_
     information at each call, including the ID numbers, the number_
     of events in each histogram, the PROB value, and the maximum_
     Kolmogorov distance between the two histograms._
     For 2-Dim histograms, there are two Kolmogorov distances_
     (see below). If 'N' is specified, there is a third line of_
     output giving the PROB for shape alone, and for normalisation.
-O   Overflow, requests that overflow bins be taken into account.
-U   Underflow, requests that underflow bins be taken into account.
-L   Left: include x-underflows
-R   Right: include x-overflows
-T   Top: include y-overflows
-B   Bottom: include y-underflows
-F1  Histogram 1 has no error (is a function)
-F2  Histogram 2 has no error (is a function)
>Guidance
Test of compatibility for two 1-Dim histograms ID1 and ID2.
A probability PROB is calculated as a number between zero and one,
where PROB near
one indicates very similar histograms, and PROB near zero
means that it is very unlikely that the two arose from the same
parent distribution.
For two histograms sampled randomly from the same distribution,
PROB will be (approximately) uniformly distributed between 0 and 1.
See discussion in HBOOK manual under 'HDIFF- Statistical Considerations'.
By default (if no options are selected with CHOPT) the comparison
is done only on the shape of the two histograms, without consideration
of the difference in numbers of events, and ignoring all underflow and
overflow bins.
>Action PAHOPE

>Command SORT
>Parameters
ID 'Histogram Identifier' C Loop
+
CHOPT 'Options' C D='XA'
-X   X-axis is being treated.
-Y   Y-axis is being treated.
-Z   Z-axis is being treated.
-A   Alphabetically.
-E   Reverse alphabetical order.
-D   By increasing channel contents.
-V   By decreasing channel contents.
>Guidance
Sort the alphanumeric labels of the histogram ID according
to the value of CHOPT.
>Action PAHOPE

>Command SMOOTH
>Parameters
ID 'Histogram or Ntuple Identifier' C Minus
+
OPTION 'Options' C D='2M'
-0 Replace original histogram by smoothed.
-1 Replace original histogram by smoothed.
-2 Store values of smoothed function and its parameters without_
   replacing the original histogram (but see note below) - the smoothed_
   function can be displayed at editing time - see HISTOGRAM/PLOT.
-M Invoke multiquadric smoothing (see HBOOK routine HQUAD).
-Q Invoke the 353QH algorithm (see HBOOK routine HSMOOF).
-S Invoke spline smoothing.
-V Verbose (default for all except 1-D histogram).
-N Do not plot the result of the fit.
-F Write Fortran77 function to HQUADF.DAT (multiquadric only)
SENSIT 'Sensitivity parameter' R D=1. R=0.3:3.
SMOOTH 'Smoothness parameter' R D=1. R=0.3:3.
>Guidance
Smooth a histogram or 'simple' ntuple.  ('simple' = 1, 2, or 3 variables.)
.
For multiquadric smoothing, SENSIT controls the sensitivity to statistical
fluctuations.  SMOOTH controls the (radius of) curvature of the
multiquadric basis functions.
.
Notes:
.
1) The multiquadric basis functions are SQRT(R**2+D**2), where R is
the distance from the 'centre', and D is a scale parameter and
also the curvature at the 'centre'.  'Centres' are located at
points where the 2nd differential or Laplacian of event density
is statistically significant.
.
2) The data must be statistically independent, i.e. events (weighted
or unweighted) drawn randomly from a parent probability
distribution or differential cross-section.
.
For spline smoothing, SENSIT and SMOOTH control the no. of knots (= 10 *
SENSIT) and degree of splines (= SMOOTH + 2) (thus if SENSIT and SMOOTH
are at their default values a 10-knot cubic spline is used).
.
Notes:
.
1) The spline option ALWAYS replaces the contents of a 2-D histogram.
(Also chi-squared is unavailable in this case.)
.
2) Use the SPLINE command for more flexibility.
>Action PAHFIT

>Command SPLINE
>Parameters
ID 'Histogram Identifier' C
+
ISEL 'Option flag' I D=2
KNOTX 'Number of knots' I D=10
KX 'Degree of the spline' I D=3
>Guidance
Smooth 1-Dim or 2-Dim histogram ID using B-splines.
If ID is a 1-Dim histogram then:
 ISEL = 0,1 replace original histogram by smoothed.
      = 2   superimpose as a function when editing.
If ID is a 2-Dim histogram then original contents are replaced.
>Action PAHFIT

>Command FUNCTION
>Parameters
ID    'Histogram Identifier' C
UFUNC 'Name of the function' C
>Guidance
Associate the function UFUNC with the histogram ID.
.
Example:

    HIS/OP/FUN 110 X**2
    H/PL 110
>Action PAHFIT


>Command PARAM
>Parameters
ID 'Histogram Identifier' C
+
ISEL 'Control word' I D=11
R2MIN 'Min correlation coefficient' R D=1.
MAXPOW 'Max degree of polynomials' I D=5 R=1:20
>Guidance
Perform a regression on contents of the 1-Dim histogram ID.
Find the best parameterisation in terms of elementary functions
(regressors). See HBOOK guide HPARAM.
Control word ISEL=1000*T +100*W +10*S +P
 S = 1 resulting parametric fit superimposed on histogram
     0 no superposition
 P = 0 minimal output: the residual sum of squares is printed
     1 normal output: in addition, the problem characteristics and
       options are printed; also the standard deviations and
       confidence intervals of the coefficients.
     2 extensive output: the results of each iteration are printed
       with the normal output.
 W = 0 weights on histogram contents are already defined via HBARX
       or HPAKE. If not they are taken to be equal to the
       square-root of the contents.
     1 weights are equal to 1.
 T = 0 monomials will be selected as the elementary functions
     1 Chebyshev polynomials with a definition region: [-1,1]
     2 Legendre polynomials with a definition region: [-1,1]
     3 shifted Chebyshev polynomials with a definition region: [0,1]
     4 Laguerre polynomials with a definition region: [0,+infinite]
     5 Hermite polynomials with a definition region: [-inf,+inf]
The FORTRAN code of the parameterisation is written onto the file
FPARAM.DAT.
>Action PAHFIT

>Command HSETPR
>Parameters
PARAM 'Parameter name' C D='FEPS'
VALUE 'Parameter value' R D=0.001
>Guidance
Set various parameters for command PARAM.
>Action PAHFIT

>Menu ../GET_VECT
>Guidance
Fill a vector from values stored in HBOOK objects.

>Command CONTENTS
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Get contents of histogram ID into vector VNAME.
>Action PAHVEC

>Command ERRORS
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Get errors of histogram ID into vector VNAME.
>Action PAHVEC

>Command FUNCTION
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Get function associated to histogram ID into vector VNAME.
>Action PAHVEC

>Command ABSCISSA
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Get values of center of bins abscissa into vector VNAME.
>Action PAHVEC

>Command REBIN
>Parameters
ID 'Histogram Identifier' C
X 'Name of vector X' C
Y 'Name of vector Y' C
EX 'Name of vector EX' C
EY 'Name of vector EY' C
+
N 'Number of elements to fill' I  D=100
IFIRST 'First bin' I D=1
ILAST 'Last bin' I D=100
CHOPT 'Option' C D=' '
-N  Do not normalize values in Y
>Guidance
The specified channels of the 1-Dim histogram ID are cumulated (rebinned)
into new bins. The final contents of the new bin is the average
of the original bins by default. If the option N is given, the final
contents of the new bin is the sum of the original bins.
Get contents and errors into vectors, grouping bins.
Bin width and centers are also extracted.
Allow to combine 2, 3 or more bins into one.
   E.g.:  REBIN 110 X Y EX EY 25 11 85
           will group by 3 channels 11 to 85  and return
           new abscissa, contents and errors.
           Errors in X are equal to 1.5*BINWIDTH.
   N.B.:
          REBIN ID X Y EX EY  is a convenient way to return in
          one call abscissa, contents and errors for 1-Dim histogram.
          In this case the errors in X are equal to 0.5*BINWIDTH.
>Action PAHVEC

>Menu ../PUT_VECT
>Guidance
Replace histogram contents with values in a vector.

>Command CONTENTS
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Replace contents of histogram with values of vector VNAME.
>Action PAHVEC

>Command ERRORS
>Parameters
ID 'Histogram Identifier' C
VNAME 'Vector name' C
>Guidance
Replace errors of histogram with values of vector VNAME.
>Action PAHVEC

>Menu ../SET
>Guidance
Set histogram attributes.

>Command MAXIMUM
>Parameters
ID 'Histogram Identifier' C Loop
VMAX 'Maximum value' R
>Guidance
Set the maximum value on the Y axis.
To select again an automatic scale, just set VMAX equal to
the minimum.
.
Example:
    MIN id 0 ; MAX id 0
Reset the default scaling.
>Action PAHSET

>Command MINIMUM
>Parameters
ID 'Histogram Identifier' C Loop
VMIN 'Minimum value' R
>Guidance
Set the minimum value on the Y axis.
To select again an automatic scale, just set VMIN equal to
the maximum.
.
Example:
    MIN id 0 ; MAX id 0
Reset the default scaling.
>Action PAHSET

>Command NORMALIZE_FACTOR
>Parameters
ID 'Histogram Identifier' C
+
XNORM 'Normalisation factor' R D=1
>Guidance
Set the contents/errors normalisation factor.
Only valid for histograms (1-Dim).
(does not change contents, only presentation).
>Action PAHSET

>Command SCALE_FACTOR_2D
>Parameters
ID 'Histogram Identifier' C
+
XSCALE 'Scale factor' R D=0
>Guidance
Set the scale factor for histograms (2-Dim).
>Action PAHSET

>Command IDOPT
>Parameters
ID     'Histogram Identifier' C
OPTION 'Options' C
-SETD* Set all options to the default values
-SHOW  Print all the options currently set
-BLAC  1 Dim histogram printed with X characters
-CONT* 1 Dim histogram is printed with the contour option
-STAR  1 Dim histogram is printed with a * at the Y value
-SCAT* Print a 2 Dim histogram as a scatter-plot
-TABL  Print a 2 Dim histogram as a table
-PROE* Plot errors as the error on mean of bin in Y for_
       profile histograms
-PROS  Plot errors as the Spread of each bin in Y for_
       profile histograms
-STAT  Mean value and RMS computed at filling time
-NSTA* Mean value and RMS computed from bin contents only
-ERRO  Errors bars printed as SQRT(contents)
-NERR* Do not print print error bars
-INTE  Print the values of integrated contents bin by bin
-NINT* Do not print integrated contents
-LOGY  1 Dim histogram is printed in Log scale in Y
-LINY* 1 Dim histogram is printed in linear scale in Y
-PCHA* Print channel numbers
-NPCH  Do not print channel numbers
-PCON* Print bin contents
-NPCO  Do not print bin contents
-PLOW* Print values of low edge of the bins
-NPLO  Do not print the low edge
-PERR  Print the values of the errors for each bin
-NPER* Do not print the values of the errors
-PFUN  Print the values of the associated function bin by bin
-NPFU* Do not print the values of the associated function
-PHIS* Print the histogram profile
-NPHI  Do not print the histogram profile
-PSTA* Print the values of statistics (entries,mean,RMS,etc.)
-NPST  Do not print values of statistics
-ROTA  Print histogram rotated by 90 degrees
-NROT* Print histogram vertically
-1EVL  Force an integer value for the steps in the Y axis
-AEVL* Steps for the Y axis are automatically computed
-2PAG  Histogram is printed over two pages
-1PAG* Histogram is printed in one single page
-AUTO* Automatic scaling
>Guidance
Set options for histogram ID. (* means default).
>Action PAHSET

>Name NTUDEF

>Menu NTUPLE
>Guidance
Ntuple creation and related operations.

>Command CREATE
>Parameters
IDN     'Ntuple Identifier'           C
TITLE   'Ntuple title'                C D=' '
NVAR    'Number of variables'         I D=1    R=1:512
CHRZPA  'RZ path'                     C D=' '
NPRIME  'Primary allocation'          I D=1000
VARLIST 'Names of the NVAR variables' C  VARARG
>Guidance
Create a Row_Wise_Ntuple. (See below how to create a Column_Wise_Ntuple).
The Ntuple may be created either purely in memory or possibly
using an automatic overflow to an RZ file.
Memory allocation works in the following way.
If CHRZPA = '  ', then a bank of NPRIME
words is created. When the space in this bank is exhausted at filling time,
a new linear structure of length NPRIME is created and this process will be
repeated should the structure become exhausted.
If CHRZPA contains the top directory name of an already existing RZ file (as
declared with HISTO/FILE), then a bank of length NPRIME is also created,
but at filling time, this bank is moved to the RZ file when full, and
then it is overwritten by any new entries.
The Ntuple can be filled by calling HFN from an interactively
defined subroutine called by the command NTUPLE/LOOP or by NTUPLE/READ.
The number of variables per data point is given in the parameter NVAR.
.
To create a Column_Wise_Ntuple, create a file, eg. newnt.f  with:
      Subroutine Newnt
*
*        Example of a COMIS subroutine to create a Ntuple interactively.
*        Data is read from a text input file
*
      character*8 mother,in1,in2
      common/ntupc/mother,in1,in2
      common/ntupr/xover
*
      lin=41
      lout=42
      id=1
      open(unit=lin,file='datafile.dat',status='old')
      call hropen(lout,'NTUPLE','New_Ntuple.hbook','N',1024,istat)
*
      call hbnt(id,'New Ntuple',' ')
      call hbname(id,'ntupr',xover,'XOVER')
      call hbnamc(id,'ntupc',mother,'MOTHER:c*8,in1:c*8,in2:c*8')
*
  10  read(lin,1000,end=20,err=20)xover,mother,in1,in2
1000  format(e15.7,2x,a,7x,a,7x,a)
      call hfnt(1)
      go to 10
*
  20  call hrout(id,icycle,' ')
      call hrend('NTUPLE')
      close (lin)
      close (lout)
      end


and then call this routine via the CALL command:
   PAW > call newnt.f
>Action PNUTIL

>Command LIST
>Guidance
List all Ntuples in the Current Directory.
Note that the command HISTO/LIST lists all histograms and Ntuples
in the Current Directory.
>Action PNUTIL

>Command PRINT
>Parameters
IDN 'Ntuple Identifier' C
>Guidance
Print a summary about Ntuple IDN.
Number of entries, variables names and limits are listed.
>Action PNUTIL

>Command HMERGE
>Parameters
OUTFILE 'Output file name' C D=' '
INFILES 'Input file names' C D=' ' VARARG
>Guidance
Merge HBOOK files containing histograms and/or ntuples. Ntuples are merged
and histograms with the same ID are added. The INFILES are merged into a new
file OUTFILE. If OUTFILE already exists, it is overwritten.
.
Note that if there is a histogram in PAW memory that has the
same identifier as a histogram in one of the files to be merged,
then the contents of the histogram in memory are added to those of
the histogram in the file. This can be avoided by deleting the
memory histogram (using H/DEL) before issuing the HMERGE command.
>Action MERGIN

>Command DUPLICATE
>Parameters
ID1 'Source Ntuple' C
ID2 'New Ntuple' I
+
NEWBUF 'Buffer size' I D=-1
TITLE  'Title of ID2' C D=' '
OPTION 'Options' C D='A' R=' ,A,M'
-       Copy ID1 structure in ID2. Reset addresses of variables.
-A      Set the Addresses of variables in common /PAWCR4,etc/.
-M      Create ID2 as a Memory resident Ntuple.
>Guidance
The structure of Ntuple ID1 is duplicated in a new ntuple ID2.
This command is useful when one wants to create an ntuple with the same
variables but only a subset of the events. NEWBUF is the buffer size
for ID2. If NEWBUF<0 the buffer size of ID1 is taken. If NEWBUF=0 the current
buffer size is taken (10000 words for RWNs). NEWBUF>0 will be the new
buffer size. If TITLE=' ' ID2 has the same title as ID1.
In case of a disk-resident ntuple (default), ID2 is created into the
current working directory which must be open in WRITE mode.
.
Example with a Row Wise Ntuple:
    Macro DUPRWN
    Close 0
    Hi/File 1 source.hbook
    Hi/File 2 new.hbook ! N
    Nt/Dup //lun1/30 2
    *
    Application Comis Quit
          Real Function Dup(dum)
          Include ?
          If (X.gt.0..Or.Y.gt.0.) call hfn(2,X)
          dup=1.
          end
    Quit
    *
    nt/loop //lun1/30 dup
    hrout 2

Note that the statement 'include ?' allows to create automatically the inlude
file (comis.inc) corresponding to the ntuple structure. The command UWFUNC
is not required in this case.
.
Example with a Column Wise Ntuple:
    Macro DUPCWN
    *
     Close 0                    | Close all the currently opened file
     H/file 1 source.hbook
     Uwfunc //lun1/1 source.inc | generate source.inc
     H/file 2 new.hbook ! N     | Create a new hbook file
     Nt/Dup //lun1/1 2          | Duplicate the ntuple 1 in the ntuple 2
    *
    * Comis routine which Loop on all events of Id1 and select some events
    * to be written in the new ntuple Id2.
    *
    Application COMIS quit
          Subroutine ntdup(Id1,Id2)
          Include 'source.inc'
          Call Hnoent(Id1,Noent)
          Do Ievent=1,Noent
             Call Hgnt(Id1,Ievent,Ierr)
             If (Ierr.ne.0) Goto 20
             If (X.Gt.0..Or.Y.Gt.0.) Then
                Call Hfnt(Id2)
             Endif
          Enddo
      20  Continue
    *
          End
    Quit
    *
     Call Ntdup(1,2)             | Execute the routine Ntdup
     Hrout 2                     | Write Id2 on disk
>Action PNUTIL

>Command RECOVER
>Parameters
IDN 'Ntuple Identifier' I
>Guidance
To recover Ntuple ID. If the job producing the Ntuple crashed
or the header was not stored correctly in the file with HROUT,
RECOVER will scan the Ntuple to rebuild the header table and
recompute the number of entries. The file on which the Ntuple
resides must be open in Update mode.
>Action PNUTIL

>Command SCAN
>Parameters
IDN    'Ntuple Identifier'   C
+
UWFUNC 'User cut function'   C D='1.'
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
OPTION 'Options' C D=' ' R=' ,S'
-       Alphanumeric output of the Ntuple.
-S      Graphical scan (spider plot).
-S2     Graphical scan (segments plot).
-A      Used with 'S' it displays the average spider.
VARLIS 'Names of the NVARS variables to scan' C  D=' ' VARARG
>Guidance
Scan the entries of an Ntuple subject to user cuts.
Scan the variables for NEVENT events starting at IFIRST, requiring that
the events satisfy cut UWFUNC. In the case of Alphanumeric output
Up to 8 variables may be scanned, the default is to scan the first 8
variables.
.
When the option S (Spider plot) is specified, each event is presented
in a graphical form (R versus PHI plot) to give a multi dimensional view of
the event. Each variable is represented on a separate axis with a scale
ranging from the minimum to the maximum value of the variable. A line joins
all the current points on every axis where each point corresponds to
the current value of the variable. When the HCOL parameter is specified
(eg SET HCOL 1002) a fill area is drawn.
.
VARLIS may contain a list of the original variables, expressions
of the original variables or/and ranges of variables.
A range can be given in the following form:
 :          means all variables (default).
 var1:var2  means from variable var1 to variable var2 included.
 var1:      means from variable var1 to the last.
 :var2      means from variable 1 to variable var2
.
For example, if IDN=30 has the 3 variables X,Y,Z,U,V,W one can do:
 PAW > scan 30
 PAW > scan 30 option=s
       each event is drawn as a spider plot.
 PAW > scan 30 option=sa
       each event is drawn as a spider plot and the average spider
       plot is also drawn.
 PAW > set 2BUF 1
 PAW > scan 30 option=s2
 More...? ( <CR>/N/G ) G
       answering 'G' with double buffer on, create a graphical
       animation of the ntuple content.
 PAW > scan 30 option=s  X:Z W
 PAW > scan 30 z>10
 PAW > scan 30 z>10 ! ! ! z abs(x) y+z x func.for
       where func.for is a COMIS function returning an expression
       of the original variables. This function func.for may be
       generated automatically by the PAW command:
 PAW > uwfunc 30 func.for
>Action PANNTU

>Command LOOP
>Parameters
IDN 'Identifier of Ntuple' C
UWFUNC 'Selection function or cut identifier' C D='1.'
+
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
>Guidance
Invoke the selection function UWFUNC for each event starting
at event IFIRST.
In UWFUNC, the user can fill one or several histograms previously booked.
The loop will be terminated if UWFUNC returns a negative value.
For more information about UWFUNC, see command NTUPLE/PLOT.
.
The ntuple identifier IDN, is an integer in this command. It make no
sense to have an expression like 10.x.
>Action PANNTU

>Command GCUT
>Parameters
CID 'Cut Identifier' C
IDN 'Ntuple Identifier' C
+
UWFUNC 'Selection function' C D='1.'
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
NUPD   'Frequency to update histogram' I D=100000000
OPTION 'Options' C D=' ' R=' ,C,S,+,B,L,P,*,U,E,A'
-       Draw the ntuple as an histogram.
-C      Draw a smooth curve.
-S      Superimpose plot on top of existing picture.
-+      Add contents of IDN to last plotted ntuple.
-B      Bar chart format.
-L      Connect channels contents by a line.
-P      Draw the current polymarker at each channel or cell.
-*      Draw a * at each channel.
-U      Update channels modified since last call.
-E      Compute (HBARX) and draw error bars with current marker.
-A      Axis labels and tick marks are not drawn.
-PROF   Fill a Profile histogram (mean option).
-PROFS  Fill a Profile histogram (spread option).
-PROFI  Fill a Profile histogram (integer spread option).
IDH     'Identifier of histogram to fill' I D=1000000
WKID    'Workstation identifier' I D=1
>Guidance
Define a graphical cut on a one or two dimensional plot.
First Project and plot an Ntuple as a (1-Dim or 2-Dim) histogram
with automatic binning (ID=1000000), possibly using a selection
algorithm. See NTUPLE/PLOT for full details on what expressions can be
plotted and which options can be given.
Then the graphical cut is defined using the mouse.
>Action PANNTU

>Command PROJECT
>Parameters
IDH 'Identifier of histogram to fill' C
IDN 'Identifier of Ntuple' C
+
UWFUNC 'Selection function or cut identifier' C D='1.'
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
>Guidance
Project an Ntuple onto a 1-Dim or 2-Dim histogram,
possibly using a selection function or predefined cuts.
IDN may be given as IDN or IDN.X , IDN.Y%X , IDN.1, IDN.2%1.
Y%X means variable Y of Ntuple IDN versus variable X.
For more information about UWFUNC, see command NTUPLE/PLOT.
The histogram IDH is not reset before filling. This allows
several PROJECTs from different Ntuples.
>Action PANNTU

>Command READ
>Parameters
IDN    'Ntuple Identifier' C
FNAME  'File name'         C
+
FORMAT 'Format'            C D='*'
OPT    'Options'           C D=' '
NEVENT 'Number of events'  I D=1000000
MATCH  'Matching pattern'  C D=' '
>Guidance
Read Ntuple values from the alphanumeric file FNAME with
the format specifications in FORMAT.
.
This command works for row wise Ntuple only.
.
Before executing this command, the Ntuple IDN must have been
created with the command Ntuple/Create.
.
MATCH is used to specify a pattern string, restricting
the Ntuple filling only to the records in the file which
verify the pattern. The possible patterns are:
.
  /string/      match a string (starting in column 1)
 -/string/      do not match a string (starting in column 1)
  /string/(n)   match a string, starting in column n
  /string/(*)   match a string, starting at any column
.
Example:
    H/del *
    Appl Data ntmatch.dat
     101. 201. 301.   C
     102. 202. 302.
     103. 203. 303.   C
     104. 204. 304.   C
     105. 205. 305.
     106. 206. 306.
     107. 207. 307.
     108. 208. 308.
     109. 209. 309.
    ntmatch.dat
    Nt/Create 4 'Test of Match' 3 ! ! Xmatch Ymatch Zmatch
    Nt/Read 4 ntmatch.dat ! ! ! -/C/(*)
    Nt/SCAN 4

In this macro all the lines with a C at the end are not read.

>Action PNUTIL

>Command PLOT
>Parameters
IDN 'Ntuple Identifier' C
+
UWFUNC 'Selection function' C D='1.'
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
NUPD   'Frequency to update histogram' I D=100000000
OPTION 'Options' C D=' ' R=' ,C,S,+,B,L,P,*,U,E,A'
-       Draw the ntuple as an histogram.
-C      Draw a smooth curve.
-S      Superimpose plot on top of existing picture.
-+      Add contents of IDN to last plotted ntuple.
-B      Bar chart format.
-L      Connect channels contents by a line.
-P      Draw the current polymarker at each channel or cell.
-*      Draw a * at each channel.
-U      Update channels modified since last call.
-E      Compute (HBARX) and draw error bars with current marker.
-A      Axis labels and tick marks are not drawn.
-PROF   Fill a Profile histogram (mean option).
-PROFS  Fill a Profile histogram (spread option).
-PROFI  Fill a Profile histogram (integer spread option).
IDH     'Identifier of histogram to fill' I D=1000000
>Guidance
Project and plot an Ntuple as a (1-Dim or 2-Dim) histogram
with automatic binning (ID=1000000), possibly using a selection
algorithm. See parameter CHOPT in command HISTO/PLOT to have
more details on the possible OPTION.
 IDN may be given as IDN
                     IDN.X
                     IDN.Y%X
                     IDN.1
                     IDN.2%1
                     IDN.expression1
                     IDN.expression1%expression2
Y%X means a scatter-plot Y(I) versus X(I) where I is the event number.
2%1 means a scatter-plot variable 2 versus variable 1.
In this example, X and Y are the names of the variables 1 and 2
respectively.
Expression 1 is any numerical expression of the Ntuple variables.
It may include a call to a COMIS function.
 UWFUNC may have the following forms:

 1- UWFUNC='0' or missing (only IDN given). No selection is applied.

 2- UWFUNC is a CUT or combination of valid CUTS created by the
    command NTUPLE/CUTS. Ex:
           UWFUNC=$1            means use cut $1
           UWFUNC=$1.AND.$2
           UWFUNC=.NOT.($1.AND.$2)
           UWFUNC=($1.OR.$2).AND.$3

 3- UWFUNC is a FORTRAN expression
    Ex:    X>3.14.AND.(Y<Z+3.15)

 4- UWFUNC is a variable name or an arithmetic expression
    Ex:   NT/PLOT 30.X Y  weight of each event is variable Y
          NT/PLOT 30.X X**2+Y**2

 5- UWFUNC is the name of a selection function in a text file with
    the name UWFUNC.FTN, UWFUNC.FOR, UWFUNC FORTRAN (Apollo, VAX, IBM).
The command UWFUNC may be used to generate automatically this function.
For example if IDN=30 is an Ntuple with 3 variables per event and 10000
events, then
    NTUPLE/PLOT 30.X SELECT.FOR
will process the 10000 events of the Ntuple IDN=30. For each event,
the function SELECT is called. It returns the weight of the event.
Example:
    FUNCTION SELECT(X)
    DIMENSION X(3)
    IF(X(1)**1+X(2)**2.LT.1.5)THEN
       SELECT=0.
    ELSE
       SELECT=1.
    ENDIF
    END
The file SELECT.FOR (VAX), SELECT.FTN (Apollo) or SELECT FORTRAN (IBM)
can be edited from PAW using the command EDIT. Note that if the suffix
(.FTN, .FORTRAN or .FOR) is omitted, then COMIS will start from the
precompiled version in memory and not from the file. Results of a
selection can be saved in a MASK (See NTUPLE/MASK).

 Ex: NT/PLOT 30.X Z<0.4>>MNAME(4)
     means mark bit 4 in mask MNAME for all events satisfying
     the condition Z<0.4

A MASK may also be given as input to a selection expression.

 Ex:  NT/PLOT 30.X MNAME(4).and.Z<0.4
      means all events satisfying bit 4 of MNAME AND Z<0.4

It is possible to plot expressions of the original variables.

 Ex 1:  NT/PLOT 30.SIN(X)%SQRT(Y**2+Z**2)  Z<0.4
     plots a scatter-plot of variable U versus V for all events
     satisfying the condition Z<0.4. U and V are defined as being
     U=SIN(X) and V=SQRT(X**2+Y**2)

 Ex 2:  NT/PLOT 30.FUNC.FTN(X)%(SIN(Y)+3.)  Z<0.2.and.TEST.FTN>6
     plots a scatter-plot of variable U versus V for all events
     satisfying the condition (Z<0.2 and the result of the COMIS
     function TEST.FTN >6). U and V are defined as being
     U=Result of the COMIS function FUNC.FTN,  V=SIN(Y)+3.

The default identifier of the histogram being filled is IDH=1000000.
At the next invocation of this command, it will be overwritten.

If either NEVENT or IFIRST or NUPD are negative, then the identifier
of the histogram being filled will be taken as IDH=-NEVENT or
IDH=-IFIRST or IDH=-NUPD. IDH may have been created with H/CREATE.
Before filling IDH, the contents of IDH are reset if IDH already
exists. Use NTUPLE/PROJECT to cumulate several passes into IDH.
Note that IDH not equal to 1000000 is a convenient way to force user
binning.

Every NUPD events, the current status of the  histogram is displayed.
>Action PANNTU

>Command CHAIN
>Parameters
+
CNAME 'Chain Name' C D=' '
ENTRY 'Chain Member(s) | -P Path' C D=' ' VARARG
>Guidance
Using the chain command one can build logical Ntuples of unlimited size.
The chain command creates an Ntuple chain CNAME and add member(s) ENTRY.
If the chain already exists the member is simply added.
More than one member may be specified at a time.
A chain can contain three different type of members: files,
logical units and other chains. The member type is deduced from the format
of the member. Entries containing the characters . / : ; $ are considered
to be files, entries like //LUN4 are assumed to be logical units
and all other type of entries are chains. Chain names must be unique.
After a chain has been defined it can be traversed, by all Ntuple
commands (NT/PLOT, NT/PROJ, NT/LOOP), by changing the current working
directory to the chain: CD //CNAME.

A member may be deleted from a chain by
preceding it by a - sign. A complete chain can be deleted by preceding
the chain name by a -. All chains can be deleted by giving a - as chain
name. Not specifying any parameters results in the listing of all defined
chains. A chain tree will be printed by appending a > character to the
chain name.

The path of all chain members, from chain CNAME downwards, can be changed
by specifying a chain path. This is done by giving a chain name followed
by the -P option and a path specification. The chain path will be pre-pended
to the member names. Chains down the tree can override a path specified
higher up in the tree.

 Examples of chain (Ntuple tree) definition:

 CHAIN   Year93  Jan Feb March April May ...
 CHAIN   Jan     Week1 Week2 Week3 Week4
 CHAIN   Week1   file1.hbook file2.hbook ...
 CHAIN   Week2   file3.hbook file4.hbook ...

 CD //Jan
 NT/PLOT 10.e     ; loop over all files in chains Week1, Week2, Week3, ...
 CD //Year93      ; loop over all files in chains Jan, Feb, March, ...
 CHAIN Year93 -P /user/delphi   ; all files from chain Year93 downward will
                                  be changed to /user/delphi/file1.hbook, ...

 CHAIN Year93>    ; print the chain tree Year93
 CHAIN -Feb       ; delete chain Feb
 CHAIN Jan -file3.hbook   ; delete file3.hbook from chain Jan
>Action PCHAIN

>Command CUTS
>Parameters
CUTID 'Cut identifier' C
+
OPTION 'Options' C D='P'
-P  Print definition of cut CUTID.
--  Reset cut CUTID.
-R  Read definition of cut CUTID from file FNAME.
-W  Write definition of cut CUTID on file FNAME (text file).
-D  Draw cut contour.
FNAME 'File name' C D=' '
WKID  'Workstation identifier' I D=1
>Guidance
Define the CUTID with the format $nn.
nn is an integer between 1 and 99.
This cut can then be used in subsequent commands NTUPLE/PLOT, PROJECT.

  OPTION='expression'  allows to define the cut CUTID. For example
  the command:

   PAW > CUTS $1 X<0.8.and.Y<SQRT(X)

  defines the cut $1.

Note that CUTID=$0 means all cuts.
>Action PANCUT

>Command CSELECT
>Parameters
+
CHOPT 'Options' C D='N'
-  Comment is left adjusted to the current zone
-R Comment is right adjusted to the current zone
-C Comment is centered to the current zone
-B Comment is drawn below the top zone line
-N All subsequent NTUPLE/PLOT commands will print the selection_
   mechanism with the options specified in CHOPT.
CSIZE 'Comment size' R D=0.28
>Guidance
To write selection mechanism as a comment on the picture.
By default, the comment is drawn left justified above the top zone line.
Example:
 CSEL          All coming NT/PLOT commands will draw a comment
               of size CSIZE=0.28cm Left justified.
 CSEL NRB 0.4  All coming NT/PLOT commands will draw a comment
               of size 0.4 cm Right justified Below the top line.
 CSEL  CB      Draw previous selection mechanism Centered Below
               the top zone line.

The Global title font (SET GFON) with precision 1 is used to draw the
text.
>Action PNUTIL

>Command UWFUNC
>Parameters
IDN 'Ntuple Identifier' C
FNAME 'File name' C
+
CHOPT 'Options' C D=' '
-  Generate the FORTRAN skeleton of a selection function.
-E Present the selection function in the local editor.
-P Code to print events is generated (not valid for new Ntuples).
-T Names of the Ntuple variables are generated in DATA statements _
   (not valid for new Ntuples).
>Guidance
To generate the FORTRAN skeleton of a selection function or the
INCLUDE file with the columns declaration.
.
A FORTRAN function is generated if the FNAME is of the form,
xxx.f, xxx.for, xxx.fortran. Otherwise an INCLUDE file is generated.
Example:
If Ntuple ID=30 has variable names [X,Y,Z,ETOT,EMISS,etc] then:
.
NTUPLE/UWFUNC 30 SELECT.FOR   will generate the file SELECT.FOR with:
      FUNCTION SELECT(XDUMMY)
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X,Y,Z,ETOT,EMISS,etc
*
      SELECT=1.
      END
Then using the command EDIT one can modify this file which could then
look something like (IDNEVT is the event number):
      FUNCTION SELECT(XDUMMY)
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X,Y,Z,ETOT,EMISS,etc
*
      IF(X**2+Y**2.GT.Z**2.OR.ETOT.GT.20.)THEN
         SELECT=1.
      ELSE
         SELECT=0.
      ENDIF
      END
If in a subsequent command NTUPLE/PLOT, the selection function SELECT
is used, then:
   If NTUPLE/PLOT 30.ETOT SELECT.FOR
      VIDN1=ETOT
   If NTUPLE/PLOT 30.SQRT(X**2+Y**2)%(ETOT-EMISS)
      VIDN1=ETOT-EMISS
      VIDN2=SQRT(X**2+Y**2)
.
NTUPLE/UWFUNC 30 SELECT.INC will generate an include file. This include file
may be referenced in a selection function in the following way:
      FUNCTION SELECT(XDUMMY)
      include 'select.inc'
*
      SELECT=1.
      IF(X.LE.Y)SELECT=0.
      END
.
Note that the command UWFUNC is not required if the SELECT function has the
following form:
      FUNCTION SELECT(XDUMMY)
      include ?
*
      SELECT=1.
      IF(X.LE.Y)SELECT=0.
      END
In this case (thanks to the statement 'include ?') the include file will be
generated automatically with the name 'comis.inc'.
>Action PAWUWF

>Command LINTRA
>Parameters
IDN    'Ntuple Identifier'   C
+
CHOPT  'Options'  C D=' '
-N The variables are normalized._
   This option is useful in the case the ranges of variables_
   are very different
-P Print more results about the analysis
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
NVARS  'Number of the most significant variables ' I D=20 R=0:20
VARLIS 'Names of the NVARS most significant variables ' C
>Guidance
Data reduction on Ntuple.
The method used is the PRINCIPAL COMPONENTS ANALYSIS.
The Principal Components Analysis method consists in applying
a linear transformation to the original variables of a ntuple.
This transformation is described by an orthogonal matrix and
is equivalent to a rotation of the original space to a new set
of coordinates vectors, which hopefully provide easier identification
and dimensionality reduction. This matrix is real positive definite
and symmetric and has all its eigenvalues greater than zero.
Among the family of all complete orthonormal bases, the basis
formed by the eigenvectors of the covariance matrix and belonging
to the largest eigenvalues corresponds to the most significant features
for the description of the original ntuple.

Reduction of the variables for NEVENT events starting at IFIRST
The default is to take all the 20 first variables.

This command creates a file : -> XTOXSI.FORTRAN or xtoxsi.for,xtoxsi.ftn.
This file contains a Fortran function which computes the new variables.
These new variables can be visualized in PAW with for example:
 PAW > Ntuple/plot id.xtoxsi.ftn(1)
 PAW > Ntuple/plot id.xtoxsi.ftn(1)%xtoxsi.ftn(3)
>Action PALINT

>Command VMEM
>Parameters
+
MXSIZE    'Maximum size of dynamic memory buffer in MBytes' I  D=-1 R=-2:128
>Guidance
Change or show the size of the dynamic memory buffer used to store
Ntuple columns during Ntuple analysis. The default is 10 MB. Giving
a value of 0 turns the buffer facility off. The upper limit is 128 MB,
but be sure you have enough swap space and realize that when the buffer
is swapped to disk you loose part of the benefit of the buffer facility
(which is to reduce the number of disk accesses). Omitting the argument
or specifying -1 will show you the current upper limit and used and free
space. Giving -2 shows which columns are currently stored in memory.
>Action PVMEM

>Command DUMP
>Parameters
IDN 'Ntuple Identifier' C
+
UWFUNC 'Selection function' C D='1.'
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
FILENAME 'Output filename' C D=' '
OPTION 'Options' C D=' '
>Guidance
Print values of selected expressions.
>Action PANNTU

>Command QP_FLAGS
>Parameters
+
OPTION    'Option name'   C
VALUE     'Option value'   I
>Guidance
Set debug options for the Query Processor
>Action QPFLAGS

>Menu MASK

>Command FILE
>Parameters
FNAME 'File name' C
+
CHOPT 'Options' C D='R'
-R Existing file is opened (read mode only).
-N A new file is opened.
-U Existing file is opened to be modified.
>Guidance
Open a MASK file.
>Action PANMAS

>Command CLOSE
>Parameters
MNAME 'Mask name' C
>Guidance
Close a MASK file.
>Action PANMAS

>Command LIST
>Parameters
+
MNAME 'Mask name' C D='*'
>Guidance
List the MASK files currently open.
>Action PANMAS

>Command RESET
>Parameters
MNAME 'Mask name' C
IBIT 'Number of bit to reset' I D=1 R=1:32
>Guidance
Reset on bit in a mask file.
>Action PANMAS

>Command CONVERT
>Parameters
MNAME 'Mask name' C
ONAME 'Mask name' C
>Guidance
Convert an old mask file to a new one.
>Action PANMAS

>Name ZEBDEF

>Menu ZEBRA
>Guidance
Interfaces to the ZEBRA RZ, FZ and DZ packages.


>Menu RZ
>Guidance
ZEBRA/RZ package: direct access Input/Output.

>Command FILE
>Parameters
LUN 'Logical unit number' I R=1:128
FNAME 'File name' C
+
LRECL 'Record length in WORDS' I D=1024
CHOPT 'Options' C D=' '
-  Read only mode.
-U Update mode.
>Guidance
Open an existing direct access file.
>Action PAZRZ

>Command MAKE
>Parameters
LUN 'Logical unit number' I R=1:128
FNAME 'File name' C
+
LRECL  'Record length in WORDS' I D=1024
NREC   'Number of records' I D=1000
NWKEY  'Number of words per Key' I D=1
CHFORM 'Key format' C D='I' R='I,B,A,H'
CHTAGS 'List of Tags' C D='HBOOK-ID'
>Guidance
Open a new direct access file.
>Action PAZRZ

>Command MDIR
>Parameters
CHDIR 'Directory name' C
+
NWKEY  'Number of words per Key' I D=1
CHFORM 'CHFORM' C D='I'
CHTAGS 'List of Tags' C D='HBOOK-ID'
>Guidance
Create a new RZ directory below the current directory.
>Action PAZRZ

>Command DDIR
>Parameters
CHDIR 'Directory name' C
>Guidance
Delete the directory CHDIR from the current directory.
>Action PAZRZ

>Command LDIR
>Parameters
+
CHPATH 'Path name' C D=' '
CHOPT 'Options' C D=' '
-  List contents of a directory.
-A List all the Ntuple extensions.
-T List a directory Tree.
>Guidance
List contents of a directory (memory or disk).
To list all RZ files currently opened, type 'LD //'.
Note that if the Current Directory is //PAWC, this command
uses the same format as HISTO/LIST.
>Action PAZRZ

>Command CDIR
>Parameters
+
CHPATH 'Path name' C D=' '
CHOPT 'Options' C D=' '
>Guidance
Change the current working directory (CWD).
IF CHPATH is given make it the new CWD.
Otherwise, print the pathname of the CWD.
 Ex.  CD dir1         ; make DIR1 the new CWD
      CD //file1/dir2 ; make //FILE1/DIR2 the new CWD
      CD              ; print the name of the CWD
>Action PAZRZ

>Command PURGE
>Parameters
+
KEEP 'Number of cycles to be kept' I D=1
>Guidance
Purge an RZ directory.
>Action PAZRZ

>Command LOCK
>Parameters
+
CHLOCK 'Lock identifier' C D='RZFILE'
>Guidance
Lock an RZ directory.
>Action PAZRZ

>Command FREE
>Parameters
+
CHLOCK 'Lock identifier' C D='RZFILE'
>Guidance
Free an RZ directory.
>Action PAZRZ

>Command STAT
>Parameters
CHPATH 'Name of top directory' C
>Guidance
Print space statistics for an RZ file.
>Action PAZRZ


>Menu ../FZ
>Guidance
ZEBRA/FZ package: sequential access Input/Output.

>Command FILE
>Parameters
LUN 'Logical unit number' I R=1:128
FNAME 'File name' C
+
LRECL 'Record length in words' I D=900
CHOPT 'Options' C D='IX'
-I  Input file.
-O  Output file.
-X  Binary exchange mode.
-A  Alphanumeric exchange mode.
>Guidance
Open an FZ sequential formatted or unformatted file.
>Action PAZFZ

>Command TOFZ
>Parameters
LUN 'Logical unit number of FZ file' I R=1:128
+
CHOPT 'Options' C D=' '
>Guidance
Copy the current directory tree onto an FZ file.
>Action PAZFZ

>Command FRFZ
>Parameters
LUN 'Logical unit number of FZ file' I R=1:128
+
CHOPT 'Options' C D=' '
>Guidance
Copy the FZ file into the current directory tree.
>Action PAZFZ

>Command TOALPHA
>Parameters
FNAME 'Name of the FZ text file' C
>Guidance
Copy the current directory tree onto a FZ file.
An alphanumeric format is used.
The file FNAME can be exchanged between different machines.
>Action PAZFZ

>Command FRALPHA
>Parameters
FNAME 'Name of the FZ text file' C
>Guidance
Copy the FZ alphanumeric file into the current directory.
>Action PAZFZ

>Menu ../DZ
>Guidance
ZEBRA/DZ package: debugging.

>Command SHOW
>Parameters
NAME 'Bank name' C
+
NUMBER 'Bank number' I D=1
CHOPT 'Options' C D='BSV'
-B  Print the bank.
-S  Print the bank contents from left to right Sideways_
    with up to ten elements per line.
-V  Print the vertical (down) structure.
-D  Print the bank contents from top to bottom Downwards_
    with five elements per line.
-L  Print the linear structure.
-Z  Print the data part of each bank in hexadecimal format
>Guidance
Display the contents of a bank or a data structure
identified by its NAME and NUMBER.
The output format of the data part is controlled by the internal
or external I/O characteristic.
>Action PAZDZ

>Command SURV
>Parameters
NAME 'Bank name' C
+
NUMBER 'Bank number' I D=1
>Guidance
Print a survey of the structure identified by NAME, NUMBER.
>Action PAZDZ

>Command SNAP
>Parameters
+
IDIV 'Division number ' I D=2 R=0:24
CHOPT 'Options' C D='M'
-M  Print Map entry for each bank
-E  Extend map entry to dump all links of each bank_
    (otherwise only as many links as will fit on a line)
-F  Full. Dump all active banks, links and data
-K  Kill. Dropped banks to be treated as active_
    (dropped banks are not normally dumped under D or F option)
-L  Dump all Link areas associated with the store
-W  Dump the Working space, links and data
-Z  Dump the information in hexadecimal.
>Guidance
Snap of one or more divisions.
Provides a snapshot of one or more divisions in a ZEBRA store.
The kind of information provided is controlled by CHOPT.
>Action PAZDZ

>Command VERIFY
>Parameters
+
IDIV 'Division number ' I D=0 R=0:24
CHOPT 'Options' C D='CLSU'
-C Check chaining of banks only
-L Check validity of the structural links (implies 'C')
-S Check the store parameters
-U Check the validity of the up and origin (implies 'C')
-F Errors are considered fatal and generate a call to ZFATAL
>Guidance
Check the structure of one or more ZEBRA divisions.
The verification detail depends on the settings in CHOPT.
>Action PAZDZ

>Command STORE
>Parameters
+
IXSTOR 'Store number' I D=0  R=0:24
>Guidance
Display the structure of the ZEBRA store IXSTOR.
Output the parameters characterizing the store, followed by a
list of all divisions and all link areas associated with the store in
question.
>Action PAZDZ

>Name FORDEF

>Menu FORTRAN
>Guidance
Interface to MINUIT, COMIS, SIGMA and FORTRAN Input/Output.

>Command HMINUIT
>Guidance
To input commands for Interactive MINUIT in a macro.
Example:
 Application HMINUIT EXIT
 SET EPS 1.E-14
 MIGRAD
 SET PRIN 2
 MINOS
 EXIT
 Histo/fit 10 g m
>Action PAFORT

>Command COMIS
>Guidance
Invoke the COMIS FORTRAN interpreter.
COMIS allows to execute FORTRAN routines without recompiling
and relinking. It communicates
with PAW commands through vectors and functions. COMIS has its
PAW-independent command structure.
Example in command mode:
 PAW > Comis
 CS >    do 10 i=1,10
 MND>       x=sqrt(i)*10.
 MND>       print *,i,x
 MND> 10 continue
 MND>    END
 CS > quit
 PAW >
COMIS code may be inserted into a macro. Example:
 Vector/Create Y(10) r 1 2 3 4 5 6 7 8 9 10
 *
 *  In the following COMIS code, the statement 'Vector Y' declares
 *  to COMIS an existing KUIP vector. KUIP dimension is assumed.
 *  The statement 'Vector X(10)' creates a new KUIP vector.
 *  (Note that SUBROUTINEs must be declared before the MAIN program)
 *  (KUIP vectors cannot be created into the MAIN program)
 *
 APPLIcation COMIS QUIT
       SUBROUTINE DEMO
       Vector Y
       Vector X(10)
       do 10 i=1,10
          XX=i
          X(i)=Y(i)*sqrt(XX)*10.
   10  CONTINUE
       END
       CALL DEMO
       END
 QUIT
 Vector/print X       | Print KUIP vector created by COMIS
>Action PAFORT

>Command CALL
>Parameters
UROUT 'User routine' C
>Guidance
Execute the routine UROUT.
UROUT may be a routine compiled and linked with PAW.
For example : CALL HPRINT(10).
.
UROUT may also be the name of a file which can be edited interactively
with the command EDIT. For example if file UROUT.FOR contains:

     SUBROUTINE UROUT(N)
     SUM=0.
     DO 10 I=1,N
       SUM=SUM+I
  10 CONTINUE
     PRINT *,SUM
     END
Then one can type CALL UROUT.FOR(10). The routine UROUT may also
contain references to the library routines mentioned below.
.
The functions $CALL, $ICALL, and $DCALL allow to call REAL, INTEGER,
and DOUBLE PRECISION functions, respectively.
The function call must be enclosed in quotes, for example:

     $CALL('fun.f(1.5)')

with file fun.f containing

     FUNCTION FUN(X)
     FUN=X**2
     END
.
The following routines from the CERN Program Library can be called:
>User_help PAHELP
>Action PAFORT

>Command LOOP
>Parameters
NTIMES 'Number of calls' I D=1
UROUT  'User routine' C
>Guidance
The routine UROUT is called NTIMES times.
See command CALL for explanation of UROUT.
>Action PAFORT

>Command FILE
>Parameters
LUN 'Logical unit number' I
FNAME 'File name' C
+
STATUS 'File status' C D=DONTKNOW Option
-OLD        Open existing file for reading.
-APPEND     Open existing file and position at EOF.
-NEW        Create new file; error if already existing.
-UNKNOWN    Open existing or create new file.
-DONTKNOW   Like UNKNOWN except on VMS opens highest cycle.
>Guidance
Open a FORTRAN formatted text file.

UNKNOWN opens a file for write access without flagging an error if the
file already exists.  On VMS a new cycle is created.
DONTKNOW is the same as UNKNOWN except on VMS
where the highest cycle is opened.
This option should be used if it is not yet known whether the file will
be read or written.
>Action PAFORT

>Command CLOSE
>Parameters
LUN 'Logical unit number' I R=0:128
>Guidance
Close the file on unit LUN.
If the file has been opened with HISTO/FILE, PICTURE/FILE, etc, then
before closing the unit, PAW will close correctly the file with
CALL HREND or FZENDI(O), ICLWK, etc.
Giving 0 as unit will close all open files.
>Action PAFORT

>Command UNITS
>Parameters
>Guidance
List all Input/Output logical units currently open.
The files attached to them are also shown.
>Action PAFORT

>Command REWIND
>Parameters
LUN 'Logical unit number' I R=1:128
>Guidance
Rewind the file on unit LUN.
>Action PAFORT

>Command SIGMA
>Parameters
+
EXPR 'Expression' C D=' '
>Guidance
Invoke the SIGMA package.
SIGMA is an array manipulation package using its own
vector-oriented language, outside the PAW command conventions.
SIGMA may be invoked in one of the three following ways:
 1- Using the KUIP $SIGMA function. Example:
    PAW > Vector/Create x(10) r 1 2 3 4 5 6 7 8 9 10
    PAW > Graph 10 x $sigma(sqrt(x))
.
 2- Using the SIGMA command. Example:
    PAW > sigma x=array(10,1#10)
    PAW > sigma y=sqrt(x)
    PAW > Graph 10 x y
.
 3- Using the APPLication command. Example:
    PAW > APPLication SIGMA
    SIGMA > x=array(10,1#10)
    SIGMA > y=sqrt(x)
    SIGMA > exit
    PAW > Graph 10 x y
>Action PASIGM
