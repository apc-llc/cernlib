*
* $Id: pawcdf.cdf,v 1.53 2004/06/09 11:00:49 couet Exp $
*
* $Log: pawcdf.cdf,v $
* Revision 1.53  2004/06/09 11:00:49  couet
* - complete the help of HIS/LIST (option T)
*
* Revision 1.52  2002/10/17 06:54:17  couet
* - The help of the command NT/LOOP was wrong
*
* Revision 1.51  2002/06/19 14:59:35  couet
* - Help improvement 2DHISTO and 1DHISTO
*
* Revision 1.50  2001/04/12 12:16:04  couet
* - HMINUIT help updated
*
* Revision 1.49  2001/04/03 09:24:30  couet
* - update the help of MANY_PLOTS
* - run ispell on the cdf files
* - revise the keywords in pawcdf.cdf
*
* Revision 1.48  2001/03/14 16:16:07  couet
* - Nt/Dup help had a mistake
*
* Revision 1.47  2001/01/31 13:48:57  couet
* - contour plot help updated
*
* Revision 1.46  2000/06/06 14:04:11  couet
* - New option 'A' in 3DNULL documented.
*
* Revision 1.45  2000/01/18 09:58:53  couet
* - update FUN/PLOT help
*
* Revision 1.44  2000/01/13 10:08:46  couet
* - update HELP of the command CHAIN
*
* Revision 1.43  1999/08/31 08:47:09  couet
* - A new set of PAW commands using Neural Networks.
*   These new commands are using the MLPfit package.
*
* Revision 1.42  1999/04/16 14:25:19  couet
* - Update in nt/plot help
*
* Revision 1.41  1999/04/08 09:36:42  couet
* - fix a typo
*
* Revision 1.40  1999/03/26 15:09:45  couet
* - It is now possible to change the record length used to open the input
*   files of the command HMERGE.
*
* Revision 1.39  1999/03/01 09:03:34  couet
* - HELP update for the command BOX
*
* Revision 1.38  1999/01/27 13:44:35  couet
* - help update
*
* Revision 1.37  1999/01/21 09:26:23  couet
* - HELP improvements to have a better look in the html version
*   of the HELP.
*
* Revision 1.36  1999/01/19 14:21:46  couet
* - command METAFILE: - unused parameter CHMETA removed
*                     - help updated
*
* Revision 1.35  1998/12/10 10:49:09  couet
* - New commands 3DNULL 3DPLINE and 3DPMARKER
*
* Revision 1.34  1998/12/09 10:54:36  couet
* - Commands in OBSOLETE/GRAPHICS/ATTRIBUTES/ print now an error
*   message instead of a warning and the command is NOT executed.
*
* Revision 1.33  1998/11/16 11:18:40  couet
* - PI/PRINT can now scale output GIFs
*
* Revision 1.32  1998/09/02 09:47:16  couet
* - Help of UWFUNC updated
*
* Revision 1.31  1998/04/09 10:09:03  couet
* - New option in VLOCATE:
*       Q  Do not print the number of points entered
*
* Revision 1.30  1998/03/24 10:51:44  couet
* - help improvements in H/FIT and V/FIT
*
* Revision 1.29  1998/02/12 14:11:01  couet
* - Upgrade the UWFUNC help. It was still referring to the old query processor !
*
* Revision 1.28  1998/02/11 15:47:49  couet
* - KUIP/UNITS has been replaced by FORTRAN/UNITS and is now able to list the
*   HBOOK files again.
*
* Revision 1.27  1997/06/20 09:01:32  couet
* *** empty log message ***
*
* Revision 1.26  1997/05/22 09:20:20  couet
* - nt/plot help upgrade
*
* Revision 1.25  1997/03/17 10:19:21  couet
* - V/PLOT help upgrade
*
* Revision 1.24  1997/01/22 16:34:52  couet
* - update the help of H/FIT
*
* Revision 1.23  1997/01/08 16:31:11  couet
* - Help upgrade
*
* Revision 1.22  1996/12/06 16:05:36  dinofm
* Some more HELP about PLINE,PAREA,PMARKER (they do not take into account
* LOGX or LOGY options).
*
* Revision 1.21  1996/12/06 15:50:18  dinofm
* Help information updated for NT/LOOP, NT/PROJ, NT/SCAN, NT/PLOT
* (IFIRST & NEVENT parameters are not meaningful for chains).
*
* Revision 1.20  1996/10/22 08:56:23  couet
* - improve help of fun1, fun2 and fun/plot
*
* Revision 1.19  1996/09/24 08:57:04  lecointe
* Add guidance for option 'N' in NTUPLE/PLOT
*
* Revision 1.18  1996/09/10 13:36:59  couet
* - NT/MERGE is obsolete (use HMERGE instead)
*
* Revision 1.17  1996/09/09 14:51:00  couet
* - old cdf removed
*
* Revision 1.16  1996/08/30 09:39:26  couet
* - update the help of NT/PLOT
*
* Revision 1.15  1996/08/21 12:51:41  lecointe
* Updated the guidance and the available options for ntuple/scan
*
* Revision 1.14  1996/08/19 15:13:22  couet
* - help update
*
* Revision 1.13  1996/08/14 07:12:25  couet
* - Some fixes in the help of TEXT
* - New command PICTURE/LOAD
*
* Revision 1.12  1996/06/27 15:40:50  couet
* - Dummy command F/UNIT suppressed (ambiguous with K/UNITS)
*
* Revision 1.11  1996/05/23 14:19:48  couet
* - The S option in NT/SCAN (spider plot) has not been re-implemented in the
*   new QP. The help concerning this option is now removed from the cdf.
*
* Revision 1.10  1996/05/06 15:17:27  couet
* - Update the help of the command HISTOGRAM
*
* Revision 1.9  1996/04/18 13:01:01  maartenb
* - Changed ntuple/dump to have configurable separators
*
* - Improved help of ntuple/dump
*
* Revision 1.8  1996/04/12 15:12:04  cremel
* - Update help in some commands of menu NTUPLE (syntax of ntuple identifier
*   corresponding to the selection mechanism).
* - Add a series of keywords for interactive HELP.
*
* Revision 1.7  1996/03/14 13:21:10  couet
* Menu OLD suppressed
*
* Revision 1.6  1996/03/14 11:14:47  couet
* MASK is now in the OBSOLETE menu and give some details how to
* access the new masks commands
*
* Revision 1.5  1996/03/13 16:00:56  couet
* QP_FLAGS command is now CALLED FLAGS_QP because it was ambiguous with
* QUIT
*
* Revision 1.4  1996/03/13 09:48:56  cremel
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
>Keyword
HBOOK
>Guidance
Manipulation of histograms. Interface to the HBOOK package.

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
>Keyword
HBOOK Open
>Guidance
Open an HBOOK direct access file.
.
If LUN is 0 the next free logical unit will be used.
.
If LRECL is 0 the system will determine the correct record length of an 
existing file. The maximal record length which can be auto detected is 
LRECL=8192
>Action PAHIST

>Command LIST
>Parameters
+
CHOPT 'Options' C D=' '
-  List histograms and Ntuples in the current directory.
-I A verbose format is used (HINDEX), (only for //PAWC).
-S List with histograms sorted by increasing IDs.
-T List the directory Tree.
>Keyword
Ntuple dir directory 
>Guidance
List histograms and Ntuples in the current directory.
>Action PAHIST

>Command DELETE
>Parameters
ID 'Histogram Identifier' C Loop
>Keyword
scratch remove rm
>Guidance
Delete histogram/Ntuple ID in Current Directory (memory).
.
If ID=0 all histograms and Ntuples are deleted.
.
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
-+      Add contents of ID to last plotted histogram._
        Use option K with previous histogram if you have several zones.
--      Subtract contents of ID to last plotted histogram._
        Use option K with previous histogram if you have several zones.
-+-     Draw the delta with the last plotted histogram._
        Use option K with previous histogram if you have several zones.
-B      Select Bar chart format.
-L      Connect channels contents by a line.
-P      Draw the current polymarker at each channel or cell.
-*      Draw a * at each channel.
-K      Must be given if option 'U' or '+' is given later.
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
-COL    Draw 2-Dim with a colour table.
-Z      Used with COL or SURF, it draws the colour map.
-SURF   Draw as a surface plot (angles are set via the command angle).
-SURF1  Draw as a surface with colour levels
-SURF2  Same as SURF1 but without cell lines.
-SURF3  Same as SURF but with the contour plot (in colour) on top.
-SURF4  Draw as a surface with Gouraud shading.
-LEGO   Draw as a lego plot (angles are set via the command angle).
-LEGO1  Draw lego plot with light simulation.
-LEGO2  Draw lego plot with colour levels.
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
>Keyword
graphics draw smooth bar chart channel axis
SURF LEGO Gouraud contour table Cylindrical Polar
Spherical coordinate polymarker
>Guidance
Plot a single histogram or a 2-Dim projection.
If ID=0 or ID=* all the histograms in the current directory are plotted.
Each plotted histogram will start either a new picture or a new zone in
the current picture.
.
Histogram subranges can be specified in 2 different ways:
.
  PAW > h/pl id(ic1:ic2) | with ic1 and ic2 integers means plot
                         | from channel ic1 to channel ic2
  PAW > h/pl id(x1:x2)   | with x1 and x2 reals (with a .) means plot
                         | from channel corresponding to x1
.
Note that the mixed mode h/pl id(x1:ic2) is also accepted. 
.
If ic1 or x1 are omitted, the first channel is used. If ic2 or x2 are omitted,
the last channel is used.
.
  PAW > h/pl 10(:20)     | equivalent to h/pl 10(1:20)
  PAW > h/pl 10(20:)     | plot from channel 20 to the last channel
.
This subrange works also for 2-DIM cases. Example:
.
  PAW > Histo/plot 10(25:1.) 
  PAW > Histo/plot 20(4:18,0.:0.5)
.
A specific histogram cycle can be accessed:
.
  PAW > h/pl id;nc | cycle number nc is used (default is highest cycle)
.
1 Dim histograms could be plotted with option LEGO or SURF.
In this case the angles are THETA=1 and PHI=-1.
.
When option 'E' is used, the marker type can be changed with SET MTYP,
the marker size with SET KSIZ, the marker colour with SET PMCI.
.
With Option E1, the size of the tick marks at the end of the error
bars is equal to the marker size and can be changed with SET KSIZ.
.
When the option E is used with the option SURF1, SURF2, SURF3 or LEGO1,
the colours are mapped on the errors not on the content of the histogram.
.
Options LOGX and LOGY are not active on contour plots (option CONT). ie: 
contour plots are always plotted with linear scales along X and Y axis.
.
To plot projection X of ID type
.
  PAW > HI/PLOT ID.PROX
.
To plot band 1 in Y of ID type
.
  PAW > HI/PLOT ID.BANY.1
.
To plot slice 3 in Y of ID type
.
  PAW > HI/PLOT ID.SLIY.3
.
In addition to the Cartesian coordinate systems, Polar, cylindrical,
spherical, pseudo-rapidity/phi coordinates are available for LEGO and
SURFACE plots, including stacked lego plots.
For example:
.
      PAW > Histo/plot 10+20+30  LEGO1,CYL | stacked cylindrical lego plot
      PAW > Histo/plot 10+20+30  LEGO1,POL |         polar
      PAW > Histo/plot 10+20+30  LEGO1,SPH |         spherical
      PAW > Histo/plot 10+20+30  LEGO1,PSD |         pseudo-rapidity/phi
.
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
Plot one or several 1D histograms into the same plot.
The plotted 1D histograms are superimposed on the same zone
of the picture. The minimums and maximums on the X and Y directions are
computed to allow all the histograms to fit on the same plot. If 1 2 3 4 5
are five 1D histograms the following command will plot them all on the same
zone:
.
    MANY_PLOTS 1 2 3 4 5

>Action PAHIST

>Command PROJECT
>Parameters
ID 'Histogram Identifier' C Loop
>Keyword
projection
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
-S  Superimpose plot on top of existing picture.
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
-U  User function value is taken from /HCFITD/FITPAD(24),FITFUN._
    Allows to do fitting in DOUBLE PRECISION.
-K  Keep the settings of Application HMINUIT for a subsequent command.
NP 'Number of parameters' I D=0 R=0:34
PAR 'Vector of parameters' C
STEP 'Vector of steps size' C
PMIN 'Vector of lower bounds' C
PMAX 'Vector of upper bounds' C
ERRPAR 'Vector of errors on parameters' C
>Keyword
Likelihood chisquare weight Minuit HMINUIT 
minimisation
HBARX HPAKE
>Guidance
Fit a user defined (and parameter dependent) function
to a histogram ID (1-Dim or 2-Dim) in the specified range.
FUNC may be:
.
A) The name of a file which contains the user defined
function to be minimised. Function name and file name
must be the same. The function must be of type REAL. 
For example file 'func.f' is:
.
      REAL FUNCTION FUNC(X)   or FUNC(X,Y) for a 2-Dim histogram
      COMMON/PAWPAR/PAR(2)
      FUNC=PAR(1)*X +PAR(2)*EXP(-X)
      END
.
  PAW > His/fit 10 func.f ! 2 par
.
To do fitting in DOUBLE PRESICION option U should be used. In that case, 
the file 'func.f' should look like:
.
      REAL FUNCTION FUNC(X)   or FUNC(X,Y) for a 2-Dim histogram
      DOUBLE PRECISION FITPAD(24),FITFUN
      COMMON/HCFITD/FITPAD,FITFUN
      FITFUN=FITPAD(1)*X +FITPAD(2)*EXP(-X)
      FUNC=FITFUN
      END
.
B) One of the following keywords (1-Dim only):
.
 G : to fit Func=par(1)*exp(-0.5*((x-par(2))/par(3))**2)
 E : to fit Func=exp(par(1)+par(2)*x)
 Pn: to fit Func=par(1)+par(2)*x+par(3)*x**2......+par(n+1)*x**n
.
  PAW > His/fit 10 G
.
C) A combination of the keywords in B with the 2 operators + or *.
.
  PAW > His/Fit 10 p4+g ! 8 par
  PAW > His/Fit 10 p2*g+g ! 9 par
.
In this case, the order of parameters in PAR must correspond to the order of the
basic functions. For example, in the first case above, par(1:5) apply to the 
polynomial of degree 4 and par(6:8) to the Gaussian while in the second case 
par(1:3) apply to the polynomial of degree 2, par(4:6) to the first Gaussian and
par(7:9) to the second Gaussian.  Blanks are not allowed in the expression.
.
For cases A and C, before the execution of this command, the vector PAR must be
filled (via Vector/Input) with the initial values. For case B, if NP is set to 
0, then the initial values of PAR will be calculated automatically. After the 
fit, the vector PAR contains the new values of parameters. If the vector ERRPAR 
is given, it will contain the errors on the fitted parameters. A bin range may
be specified with ID.
.
  PAW > Histo/Fit 10(25:56).
.
When the Histo/it command is used in a macro, it might be convenient to specify 
MINUIT directives in the macro itself via the Application HMINUIT as described 
in this example:
.
     Macro fit
     Application HMINUIT exit
     name 1 par_name1
     name 2 par_name2
     migrad
     improve
     exit
     Histo/fit id fitfun.f M
     Return
.
Some plotting options available in the command HISTOGRAM/PLOT can be also used.
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
>Keyword
PALETTE 
>Guidance
Draw a lego plot from 2-Dim or 1-Dim histograms.
It is also possible to produce stacked lego plots. A stacked lego plot
consists of a super-imposition of several histograms, whose identifiers are
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
.
The commands OPTION BAR, SET BARW and SET BARO act on lego plots
.
The options 1 and 2 must be used only on selective erase devices.
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
-3 Surface is drawn with a contour plot in colour on top. The_
   contour plot is drawn with the colours defined with the command_
   PALETTE.
-4 Surface is drawn with Gouraud shading.
>Keyword
PALETTE Gouraud shading contour
>Guidance
Draw a surface plot from 2-Dim or 1-Dim histograms.
With this command it is possible to draw colour contour plots:
    PAW > ATT/PAL 1 3 2 3 4   | Define the palette 1 with 3 elements
    PAW > SET HCOL 0.1        | Set the list 1 as colours for histograms
    PAW > SET NDVZ 4          | Set the number of Z divisions to 4
    PAW > SURF id 90 0 2      | Draw the contour
.
The options 1 to 4 must be used only on selective erase devices.
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
   levels are equidistant. The colour indices are taken_
   in the current palette (defined with the command PALETTE)._
   If the number of levels (NLEVEL) is greater than the number_
   of entries in the current palette, the palette is explore_
   again from the beginning in order to reach NLEVEL.
-S Superimpose plot on top of existing picture.
PARAM  'Vector of contour levels' C
>Keyword
PALETTE
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
The non equidistant contours are not implemented with the option '3'.
.
Options LOGX and LOGY are not active on contour plots. ie:      
contour plots are always plotted with linear scales along X and Y axis.
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
>Keyword
channel
>Guidance
Create a one dimensional histogram.
The contents are set to zero.
If VALMAX=0, then a full word is allocated per channel, else VALMAX
is used as the maximum bin content allowing several channels to be
stored into the same machine word.
.
The histogram titles may have the following form: 
    'Histogram_title ; XXX ; YYY'
Where 'XXX' and 'YYY' are respectively the X, and axis titles.
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
>Keyword
channel
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
>Keyword
channel variable
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
>Keyword
channel
>Guidance
Create a two dimensional histogram.
The contents are set to zero. See 1DHISTO for VALMAX.
.
The histogram titles may have the following form: 
    'Histogram_title ; XXX ; YYY ; ZZZ'
Where 'XXX', 'YYY' and 'ZZZ' are respectively the X, Y and Z axis titles.
>Action PAHBOK

>Command PROX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
>Keyword
projection project
>Guidance
Create the projection onto the x axis.
The projection is not filled until the Histo/Project command is executed.
To plot projection X of ID type:
.
   PAW > HI/PLOT ID.PROX
>Action PAHBOK

>Command PROY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
>Keyword
projection project
>Guidance
Create the projection onto the y axis.
The projection may be filled with Histo/Project.
To plot projection Y of ID type:
.
   PAW > HI/PLOT ID.PROY
>Action PAHBOK

>Command SLIX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
NSLICES 'Number of slices' I
>Keyword
projection project slice
>Guidance
Create projections onto the x axis, in y-slices.
The projection may be filled with Histo/Project.
To plot slice 3 in X of ID type:
.
   PAW > HI/PLOT ID.SLIX.3
>Action PAHBOK

>Command SLIY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
NSLICES 'Number of slices' I
>Keyword
projection project slice
>Guidance
Create projections onto the y axis, in x-slices.
The projection may be filled with Histo/Project.
To plot slice 2 in Y of ID type:
.
   PAW > HI/PLOT ID.SLIY.2
>Action PAHBOK

>Command BANX
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
YMIN 'Low edge in Y' R
YMAX 'Upper edge in Y' R
>Keyword
projection project band
>Guidance
Create a projection onto the x axis, in a band of y. Several bands can be
defined on the one histogram. The projection may be filled with
Histo/Project.
To plot band 1 in X of ID type:
.
   PAW > HI/PLOT ID.BANX.1
>Action PAHBOK

>Command BANY
>Parameters
ID 'Histogram (2-Dim) Identifier' C Loop
XMIN 'Low edge in X' R
XMAX 'Upper edge in X' R
>Keyword
projection project band
>Guidance
Create a projection onto the y axis, in a band of x. Several bands can be
defined on the one histogram. The projection may be filled with
Histo/Project.
To plot band 1 in Y of ID type:
.
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
>Keyword
Read memory disk
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
>Keyword
Write memory disk
>Guidance
Write histo/Ntuple ID from memory to current directory.
Identifier may be '0' or '*' (for all histograms).
>Action PAHIO

>Command HSCRATCH
>Parameters
ID 'Histogram Identifier' C Loop
>Keyword
SCRATCH disk
>Guidance
Delete histogram ID in Current Directory on disk.
If ID='0' or '*' delete all histograms.
To delete histograms in memory use command HISTO/DELETE.
>Action PAHIO

>Command HFETCH
>Parameters
ID 'Histogram Identifier' C
FNAME 'File name' C
>Keyword
FETCH HBOOK3
>Guidance
Fetch histogram ID from file FNAME.
FNAME has been created by the old version of HBOOK3 (Unformatted).
>Action PAHIO

>Command HREAD
>Parameters
ID 'Histogram Identifier' C
FNAME 'File name' C
>Keyword
READ HBOOK3
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
>Keyword
printer plotter
>Guidance
Print histograms (line-printer format) on screen.
The command OUTPUT_LP may be used to change the output file.
>Action PAHIO

>Command DUMP
>Parameters
ID 'Histogram Identifier' C Loop
>Keyword
ZEBRA structure
>Guidance
Dump the histogram ZEBRA data structure on the terminal.
>Action PAHIO

>Command OUTPUT_LP
>Parameters
+
LUN 'Logical unit number' I D=6
FNAME 'File name' C D=' '
>Keyword
print printer plotter
>Guidance
Change the HBOOK 'line printer' file name.
If FNAME=' ' then OUTPUT is appended to an already opened file on unit LUN.
If LUN is negative, the file is closed and subsequent output
is directed to unit 6.
>Action PAHIO

>Command GLOBAL_SECT
>Parameters
GNAME 'Global section name' C D=' '
>Keyword
global section
>Guidance
Map the global section GNAME.
The current directory is changed to //GNAME.
This command doesn't work on HPUX.
>Action PAHIO

>Command GRESET
>Parameters
ID 'Histogram Identifier' C
>Keyword
RESET global section
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
>Keyword
compare comparison
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
-M Invoke multi-quadric smoothing (see HBOOK routine HQUAD).
-Q Invoke the 353QH algorithm (see HBOOK routine HSMOOF).
-S Invoke spline smoothing.
-V Verbose (default for all except 1-D histogram).
-N Do not plot the result of the fit.
-F Write Fortran77 function to HQUADF.DAT (multi-quadric only)
SENSIT 'Sensitivity parameter' R D=1. R=0.3:3.
SMOOTH 'Smoothness parameter' R D=1. R=0.3:3.
>Keyword
multi-quadric spline B-spline
>Guidance
Smooth a histogram or 'simple' ntuple.  ('simple' = 1, 2, or 3 variables.)
.
For multi-quadric smoothing, SENSIT controls the sensitivity to statistical
fluctuations.  SMOOTH controls the (radius of) curvature of the
multi-quadric basis functions.
.
Notes:
.
1) The multi-quadric basis functions are SQRT(R**2+D**2), where R is
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
>Keyword
Smooth B-spline
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
>Keyword
Chebyshev Legendre Laguerre Hermite
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
 T = 0 monomial will be selected as the elementary functions
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
>Keyword
vector
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
>Keyword
bin
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
-N  Do not normalise values in Y
>Keyword
channel bin
>Guidance
The specified channels of the 1-Dim histogram ID are cumulated (rebinned)
into new bins. The final contents of the new bin is the average
of the original bins by default. If the option N is given, the final
contents of the new bin is the sum of the original bins.
Get contents and errors into vectors, grouping bins.
Bin width and centres are also extracted.
Allow to combine 2, 3 or more bins into one. Example:
.
  PAW > REBIN 110 X Y EX EY 25 11 85
.
will group by 3 channels 11 to 85  and return
new abscissa, contents and errors.
Errors in X are equal to 1.5*BINWIDTH.
.
  PAW > REBIN ID X Y EX EY  
.
is a convenient way to return in
one call abscissa, contents and errors for 1-Dim histogram.
In this case the errors in X are equal to 0.5*BINWIDTH.
>Action PAHVEC

>Menu ../PUT_VECT
>Keyword
vector
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
>Keyword
GRAPHICS
>Guidance
Set histogram attributes.

>Command MAXIMUM
>Parameters
ID 'Histogram Identifier' C Loop
VMAX 'Maximum value' R
>Keyword
axis scale
>Guidance
Set the maximum value on the Y axis.
To select again an automatic scale, just set VMAX equal to
the minimum.
Example:
.
  PAW > MIN id 0 
  PAW > MAX id 0
.
Reset the default scaling.
>Action PAHSET

>Command MINIMUM
>Parameters
ID 'Histogram Identifier' C Loop
VMIN 'Minimum value' R
>Keyword
axis scale
>Guidance
Set the minimum value on the Y axis.
To select again an automatic scale, just set VMIN equal to
the maximum.
Example:
.
  PAW > MIN id 0 
  PAW > MAX id 0
.
Reset the default scaling.
>Action PAHSET

>Command NORMALIZE_FACTOR
>Parameters
ID 'Histogram Identifier' C
+
XNORM 'Normalisation factor' R D=1
>Keyword
normalisation
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
>Keyword
channel logarithmic axis
SETD default SHOW option BLAC CONT contour STAR SCAT scatter TABL table
PROE error profile PROS Spread STAT  Mean RMS NSTA ERRO NERR bar
INTE  integrate integral NINT LOGY  Log scale LINY linear PCHA channel
NPCH PCON NPCO PLOW edge NPLO PERR NPER PFUN NPFU PHIS NPHI PSTA statistics
NPST ROTA  rotate rotation NROT 1EVL AEVL EVL Step 2PAG  page 1PAG AUTO 
Automatic
>Guidance
Set options for histogram ID. (* means default).
>Action PAHSET

>Name FUNDEF

>Menu FUNCTION
>Keyword
GRAPHICS
>Guidance
Operations with Functions. Creation and plotting.

>Command FUN1
>Parameters
ID    'Histogram Identifier' C
UFUNC 'Name of the function' C
NCX   'Number of channels'   I D=100 R=1:
XMIN  'Low edge'             R D=0.
XMAX  'Upper edge'           R D=100.
+
CHOPT 'Options'              C D='C'
-       Create the histogram (don't draw).
-C      Draw a smooth curve.
-S      Superimpose plot on top of existing picture.
-E      Draw error bars and current marker.
-E0     Draw error bars without symbols clipping.
-E1     Draw small lines at the end of the error bars.
-E2     Draw error rectangles.
-E3     Draw a filled area through the end points_
        of the vertical error bars.
-E4     Draw a smoothed filled area through the end points_
        of the vertical error bars.
-A      Axis labels and tick marks are not drawn.
>Keyword
smooth histogram
>Guidance
Create a one dimensional histogram and fill the bins with the
values of a (single-valued) function.
.
The function UFUNC may be given in two ways:
.
- As an expression of the variable X in case of a simple function. Example:
.
  PAW > FUN1  10 SIN(X)/X  100 0 10
.
- As a COMIS function in a text file (ftest.f for example). The file ftest.f 
contains:
.
  FUNCTION FTEST(X)
  FTEST=SIN(X)/X
  END
.
  PAW > FUN1 10 FTEST.F(X) 100 0 10
.
- If the extension ".f77" is used (the file still having the extension ".f")
the local fortran compiler is invoked.
.
- If the extension ".c" is used the local C compiler is invoked.
>Action PAFUNC

>Command FUN2
>Parameters
ID    'Histogram (2-Dim) Identifier' C
UFUNC 'Name of the function'         C
NCX   'Number of channels in X'      I D=40 R=1:
XMIN  'Low edge in X'                R D=0.
XMAX  'Upper edge in X'              R D=40.
NCY   'Number of channels in Y'      I D=40 R=1:
YMIN  'Low edge in Y'                R D=0.
YMAX  'Upper edge in Y'              R D=40.
+
CHOPT 'Options'                      C D='SURF'
-       Create the histogram (don't draw).
-S      Superimpose plot on top of existing picture.
-A      Axis labels and tick marks are not drawn.
-BOX    Draw 2-Dim with proportional boxes.
-COL    Draw 2-Dim with a colour table.
-Z      Used with COL or SURF, it draws the colour map.
-SURF   Draw as a surface plot (angles are set via the command angle).
-SURF1  Draw as a surface with colour levels
-SURF2  Same as SURF1 but without cell lines.
-SURF3  Same as SURF but with the contour plot (in colour) on top.
-SURF4  Draw as a surface with Gouraud shading.
-LEGO   Draw as a lego plot (angles are set via the command angle).
-LEGO1  Draw lego plot with light simulation.
-LEGO2  Draw lego plot with colour levels.
-BB     Suppress the Back Box on 3D plots.
-FB     Suppress the Front Box on 3D plots.
-CONT   Draw 2-Dim as a contour plot (15 levels).
-TEXT   Draw 2-Dim as a table.
-CHAR   Draw 2-Dim with characters (a la HBOOK).
-CYL    Cylindrical coordinates for 3D plots.
-POL    Polar coordinates for 3D plots.
-SPH    Spherical coordinates for 3D plots.
-PSD    Pseudo-rapidity/phi coordinates for 3D plots.
>Keyword
surf surface lego Cylindrical  Polar Spherical Gouraud contour
coordinate
>Guidance
Create a two dimensional histogram and fill
the bins with the values of a (two-valued) function.
.
The function UFUNC may be given in two ways:
.
- As an expression of the variables x and y in case of a simple function.
Example:
.
  PAW > FUN2 10 ABS(SIN(X**2+Y**2)) 40 -2 2 40 -2 2 CONT
.
- As a COMIS function in a text file (ftest.f for example)
The file ftest.f contains:
.
  FUNCTION FTEST(X,Y)
  FTEST=ABS(SIN(X**2+Y**2))
  END
.
  PAW > FUN2 10 FTEST.F(X,Y) 40 -2 2 40 -2 2 CONT
.
- If the extension ".f77" is used (the file still having the extension ".f")
the local fortran compiler is invoked.
.
- If the extension ".c" is used the local C compiler is invoked.
>Action PAFUNC

>Command DRAW
>Parameters
UFUNC 'Name of function' C
+
CHOPT 'Options'          C D=' '
>Keyword
GRAPHICS
>Guidance
Draw the function UFUNC
in the current ranges specified by the command:
RANGE XLOW XUP YLOW YUP ZLOW ZUP and with THETHA and PHI angles
specified by the command ANGLE THETA PHI. The number of points
to evaluate the function between XLOW, XUP YLOW, YUP, and ZLOW, ZUP
can be changed by the command POINTS NPX NPY NPZ.
.
The function UFUNC may be given in two ways:
.
- As an expression of the variables X, Y, Z in the case of a
simple function. Example:
.
  PAW >  FUN/DRAW X*Y*Z         | equivalent to :
  PAW >  FUN/DRAW X*Y*Z=0
  PAW >  FUN/DRAW X**2+Y**2+Z**2=1
  PAW >  FUN/DRAW X**2+Y**2=1-Z**2
.
- As a COMIS function in a text file (ftest.f for example)
The file ftest.f contains:
.
  FUNCTION FTEST(X,Y,Z)
  IF(X.LE.0..AND.Y.LE.0.)THEN
    FTEST=(X+0.5)**2+(Y+0.5)**2+(Z+0.5)**2-0.2
  ELSE
    FTEST=(X-0.5)**2+(Y-0.5)**2+(Z-0.5)**2-0.1
  ENDIF
  END
.
 PAW > RANGE -1 1 -1 1 -1 1  | Define the range as a cube between -1 1 in 
                             | the 3 directions
 PAW > POINTS 20 20 20       | FUN/DRAW will use 20 points in the 3 
                             | directions
 PAW > FUN/DRAW FTEST.FOR    | Draw 2 spheres centred on (-0.5,-0.5,-0.5)
                             | and (0.5,0.5,0.5) with the radius SQRT(0.2)
                             | and SQRT(0.1)
>Action PAFUNC

>Command PLOT
>Parameters
UFUNC 'Name of function' C
XLOW  'Lower limit'      R
XUP   'Upper limit'      R
+
CHOPT 'Options' C D='C'
-C Draw a smooth curve.
-S Superimpose plot on top of existing picture.
-+ Add contents of ID to last plotted histogram.
-L Connect channel contents by a line.
-P Draw the current polymarker at each channel.
-* Draw a * at each channel.
>Keyword
smooth polymarker GRAPHICS draw
>Guidance
Plot single-valued function UFUNC between XLOW and XUP.
The function UFUNC may be given in two ways:
.
-An expression of the variable x in case of a simple function.
Example: 
   FUN/PLOT   sin(x)/x  0 10
-UFUNC is the name of a COMIS function in a text file with the
name UFUNC.F or UFUNC.FOR (UNIX, VMS).
For example, if the file FTEST.F contains:
.
      FUNCTION FTEST(X)
      FTEST=SIN(X)*EXP(-0.1*X)
      END
.
Then, 
.
   FUN/PLOT FTEST.F(X) 0 10
.
will interpret the Fortran
code in the file FTEST.F and draw the function for x
between 0 and 10.
.
The number of points to evaluate the function between XLOW and XUP
can be changed by the command /FUN/POINTS. Only 1-Dim functions
are supported. For 2-Dim use FUN2.
.
This command create and fill a 1D histogram with the identifier 12345. If
this histogram already exist in memory it is deleted. 
.
The attributes (colour, line type, etc ..) used to draw the function are the 
histograms attributes: HCOL, HTYP etc ...
.
- If the extension ".f77" is used (the file still having the extension ".f")
the local fortran compiler is invoked.
.
- If the extension ".c" is used the local C compiler is invoked.
>Action PAFUNC

>Command POINTS
>Parameters
+
NPX 'Number of points on X axis' I D=20 R=2:1000
NPY 'Number of points on Y axis' I D=20 R=2:1000
NPZ 'Number of points on Z axis' I D=20 R=2:1000
>Keyword
FUN/DRAW FUN/PLOT /FUNCTION/DRAW /FUNCTION/PLOT
>Guidance
Change the number of points to be used by FUN/DRAW
and FUN/PLOT. Note that the default for NPX is 20 for 3-Dim plots
(FUN/DRAW) but it is 100 for 1-Dim plots (FUN/PLOT).
>Action PAFUNC

>Command RANGE
>Parameters
+
XLOW 'X Lower limit' R D=-1.
XUP  'X Upper limit' R D=1.
YLOW 'Y Lower limit' R D=-1.
YUP  'Y Upper limit' R D=1
ZLOW 'Z Lower limit' R D=-1.
ZUP  'Z Upper limit' R D=1.
>Keyword
limit Lower Upper FUN/DRAW /FUNCTION/DRAW
>Guidance
Change the range used by FUN/DRAW.
>Action PAFUNC

>Command ANGLE
>Parameters
+
THETA 'Angle THETA in degrees' R D=30.
PHI   'Angle PHI in degrees'   R D=30.
>Keyword
FUN/DRAW FUNCTION/DRAW HISTO/PLOT HISTOGRAM/PLOT
>Guidance
Change the angle used by FUN/DRAW and HISTO/PLOT.
>Action PAFUNC

>Name NTUDEF

>Menu NTUPLE
>Guidance
Ntuple creation and related operations.
.
An Ntuple is a set of events, where for each event the value of a number of 
variables is recorded. An Ntuple can be viewed as a table with each row 
corresponding to one event and each column corresponding to given variable. 
The interesting 
properties of the data in an Ntuple can normally be expressed as 
distributions of Ntuple variables or as correlations between two or more of 
these variables. Very often it is useful to create these distributions from
a subset of the data by imposing cuts on some of the variables. 
.
Typically, an Ntuple is made 
available to PAW by opening a direct access file; this file, as been previously 
created 
with an program using HBOOK. A storage area for an Ntuple may also be created 
directly using NTUPLE/CREATE; data may then be stored in the allocated space 
using the NTUPLE/LOOP or NTUPLE/READ commands. Other commands merge Ntuples 
into larger Ntuples, project vector functions of the Ntuple variables into 
histograms, and plot selected subsets of events. 

>Command CREATE
>Parameters
IDN     'Ntuple Identifier'           C
TITLE   'Ntuple title'                C D=' '
NVAR    'Number of variables'         I D=1    R=1:512
CHRZPA  'RZ path'                     C D=' '
NPRIME  'Primary allocation'          I D=1000
VARLIST 'Names of the NVAR variables' C  VARARG
>Keyword
Row_Wise Row Wise HFN NTUPLE/LOOP NTUPLE/READ Column_Wise Column
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
.
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
.
and then call this routine via the CALL command:
   PAW > call newnt.f
>Action PNUTIL

>Command LIST
>Keyword
dir Directory
>Guidance
List all Ntuples in the Current Directory.
Note that the command HISTO/LIST lists all histograms and Ntuples
in the Current Directory.
>Action PNUTIL

>Command PRINT
>Parameters
IDN 'Ntuple Identifier' C
>Keyword
printer plotter
>Guidance
Print a summary about Ntuple IDN.
Number of entries, variables names and limits are listed.
>Action PNUTIL

>Command HMERGE
>Parameters
OUTFILE 'Output file name' C D=' '
INFILES 'Input file names' C D=' ' VARARG
>Keyword
merge group
>Guidance
Merge HBOOK files containing histograms and/or ntuples. Ntuples are merged
and histograms with the same ID are added. The INFILES are merged into a new
file OUTFILE. If OUTFILE already exists, it is overwritten.
.
If there are histograms in PAW memory that have same
identifiers as histograms in one of the files to be merged,
then the contents of the histograms in memory are added to those of
the histograms in the file. This can be avoided by deleting the
memory histogram (using H/DEL) before issuing the HMERGE command.
.
By default HMERGE uses the automatic record length determination
to open the input files. This works for files with a record 
length smaller or equal to 8191 words. For files with a larger
record length the following syntax can be use.
.
  PAW > hmerge LRECL 16384                   | New LRECL
   Next HMERGE will use LRECL = 16384
  PAW > hmerge out.hbook in1.hbook in2.hbook | Use the new LRECL
.
To go back to the automatic record length determination mode just do:
.
  PAW > hmerge LRECL 0
   Next HMERGE will use the auto-record length detection
.
All the input files should have the same record length.
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
>Keyword
copy
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
          Real Function Dup
          Include ?
          If (X.gt.0..Or.Y.gt.0.) call hfn(2,X)
          dup=1.
          end
    Quit
    *
    nt/loop //lun1/30 dup
    hrout 2

Note that the statement 'include ?' allows to create automatically the include
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
>Keyword
crash
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
OPTION 'Options' C D=' ' R=' '
-       Alphanumeric output of the Ntuple.
-S      Graphical scan (spider plot).
-S2     Graphical scan (segments plot).
-A      Used with 'S' it displays the average spider.
VARLIS 'Names of the NVARS variables to scan' C  D=' ' VARARG
>Keyword
SPIDER UWFUNC cut cuts
>Guidance
Scan the entries of an Ntuple subject to user cuts.
Scan the variables for NEVENT events starting at IFIRST, requiring that
the events satisfy cut UWFUNC. In the case of Alphanumeric output
Up to 10 variables may be scanned, the default is to scan the first 10
variables.
.
When the option S (Spider plot) is specified, each event is presented
in a graphical form (R versus PHI plot) to give a multi dimensional view of
the event. Each variable is represented on a separate axis with a scale
ranging from the minimum to the maximum value of the variable. A line joins
all the current points on every axis where each point corresponds to
the current value of the variable. When the HCOL parameter is specified
(eg SET HCOL 1002) a fill area is drawn.
When the additional option A is specified, a spider plot of the average
value for each variable is also drawn.
When the option S2 (Segment plot) is specified, wedges are drawn along 
each axis instead of the line joining the points. 
.
NB : a minimum of three variables in VARLIS is required for any graphical 
plot. 
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
 PAW > scan 30 varlis=X:Z W
 PAW > scan 30 z>10
 PAW > scan 30 z>10 ! ! ! z abs(x) y+z x func.for
       where func.for is a COMIS function returning an expression
       of the original variables. This function func.for may be
       generated automatically by the PAW command:
 PAW > uwfunc 30 func.for
.
Note that IFIRST and NEVENT parameters are not meaningful in case of CHAINs. 
>Action PANNTU

>Command LOOP
>Parameters
IDN 'Identifier of Ntuple' C
UWFUNC 'Selection function or cut identifier' C D='1.'
+
NEVENT 'Number of events' I D=99999999
IFIRST 'First event' I D=1
>Keyword
select selection UWFUNC NTUPLE/PLOT
>Guidance
Invoke the selection function UWFUNC for each event starting at event IFIRST.
In UWFUNC, the user can fill one or several histograms previously booked.
For more information about UWFUNC, see command NTUPLE/PLOT.
.
The ntuple identifier IDN, is an integer in this command. It make no
sense to have an expression like 10.x.
.
Note that IFIRST and NEVENT parameters are not meaningful in case of CHAINs. 
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
>Keyword
channel cut cuts select selection UWFUNC graphical polymarker
HBARX
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
>Keyword
Projection UWFUNC
>Guidance
Project an Ntuple onto a 1-Dim or 2-Dim histogram,
possibly using a selection function or predefined cuts.
IDN may be given as IDN or IDN.X , IDN.Y%X (Y%X means variable Y 
of Ntuple IDN versus variable X).
For more information about UWFUNC, see command NTUPLE/PLOT.
The histogram IDH is not reset before filling. This allows
several PROJECTs from different Ntuples.
.
Note that IFIRST and NEVENT parameters are not meaningful in case of CHAINs. 
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
>Keyword
Format
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
-N      Don't draw anything, but fill the 1D or 2D histogram IDH
-C      Draw a smooth curve (1D plots).
-S      Superimpose plot on top of existing picture.
-+      Add contents of IDN to last plotted ntuple (1D plots).
-B      Bar chart format (1D plots).
-L      Connect channels contents by a line (1D, 2D and 3D plots).
-P      Draw the current polymarker at each channel or cell (1D plots).
-*      Draw a * at each channel (1D plots).
-U      Update channels modified since last call (1D plots).
-E      Compute (HBARX) and draw error bars with current marker.
-A      Axis labels and tick marks are not drawn.
-G      Draw a Gouraud shaded surface (3D plots).
-PROF   Fill a Profile histogram (mean option).
-PROFS  Fill a Profile histogram (spread option).
-PROFI  Fill a Profile histogram (integer spread option).
IDH     'Identifier of histogram to fill' I D=1000000
>Keyword
channel UWFUNC polymarker GRAPHICS draw HBARX
>Guidance
Project and plot an Ntuple as a (1-Dim or 2-Dim) histogram
with automatic binning (ID=1000000), possibly using a selection
algorithm. See parameter CHOPT in command HISTO/PLOT to have
more details on the possible OPTION.
IDN may be given as:
.
          IDN.X
          IDN.Y%X
          IDN.Y%X%Z
          IDN.Y%X%Z%T
          IDN.expression1
          IDN.expression1%expression2
          IDN.expression1%expression2%expression3
          IDN.expression1%expression2%expression3%expression4
.
Y%X means a scatter-plot Y(I) versus X(I) where I is the event number.
In this example, X and Y are the names of the variables 1 and 2
respectively.
.
expression1 is any numerical expression of the Ntuple variables.
It may include a call to a COMIS function.
.
Y%X%Z means a 3D scatter-plot Z(I) versus Y(I) versus X(I) where I is 
the event number. If option "G" is given, three Gouraud shaded surfaces
are drawn. The green one is the average.
.
Y%X%Z%T means a 3D scatter-plot Z(I) versus Y(I) versus X(I) where I is 
the event number. T is mapped on the colour map.
.
UWFUNC may have the following forms:
.
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
    the name UWFUNC.F, UWFUNC.FTN, UWFUNC.FOR, UWFUNC FORTRAN 
    (Unix, Apollo, VAX, IBM).
.
The command UWFUNC may be used to generate automatically this function.
For example if IDN=30 is an Ntuple with 3 variables per event and 10000
events, then
.
   NTUPLE/PLOT 30.X select.f
.
will process the 10000 events of the Ntuple IDN=30. For each event,
the function SELECT is called. It returns the weight of the event.
Example:
.
    FUNCTION SELECT(X)
    DIMENSION X(3)
    IF(X(1)**1+X(2)**2.LT.1.5)THEN
       SELECT=0.
    ELSE
       SELECT=1.
    ENDIF
    END
.
The file select.f
can be edited from PAW using the command EDIT. Note that if the suffix
(.F, .FTN, .FORTRAN or .FOR) is omitted, then COMIS will start from the
pre-compiled version in memory and not from the file. Results of a
selection can be saved in a MASK (See NTUPLE/MASK).
Example:
.
   NT/PLOT 30.X Z<0.4>>MNAME(4)
.
means mark bit 4 in mask MNAME for all events satisfying the condition Z<0.4
.
A MASK may also be given as input to a selection expression.
Example:
.
   NT/PLOT 30.X MNAME(4).and.Z<0.4
.
means all events satisfying bit 4 of MNAME AND Z<0.4
.
It is possible to plot expressions of the original variables.
Examples:
.
   NT/PLOT 30.SIN(X)%SQRT(Y**2+Z**2)  Z<0.4
.
plots a scatter-plot of variable U versus V for all events
satisfying the condition Z<0.4. U and V are defined as being
U=SIN(X) and V=SQRT(X**2+Y**2).
.
   NT/PLOT 30.func.f(X)%(SIN(Y)+3.)  Z<0.2.and.TEST.FTN>6
.
plots a scatter-plot of variable U versus V for all events
satisfying the condition (Z<0.2 and the result of the COMIS
function test.f >6). U and V are defined as being
U=(Result of the COMIS function func.f), V=(SIN(Y)+3.)
.
The default identifier of the histogram being filled is IDH=1000000.
At the next invocation of this command, it will be overwritten.
.
If either NEVENT or IFIRST or NUPD are negative, then the identifier
of the histogram being filled will be taken as IDH=-NEVENT or
IDH=-IFIRST or IDH=-NUPD. This facility is kept for backward compatibility
but it is strongly recommended to use the parameter IDH instead. 
.
IDH may have been created with H/CREATE.
Before filling IDH, the contents of IDH are reset if IDH already
exists. Use NTUPLE/PROJECT to cumulate several passes into IDH.
By default IDH value is 1000000. This means that the histogram
binning will be computed automatically. In particular the minimal
and maximal value of the histogramed quantity has to be computed which
implies to do an extra pass on the ntuple data.
IDH not equal to 1000000 is a convenient way to force user
binning.
.
Every NUPD events, the current status of the  histogram is displayed.
.
Note that IFIRST and NEVENT parameters are meaningless in case of CHAINs. 
>Action PANNTU

>Command CHAIN
>Parameters
+
CNAME 'Chain Name' C D=' '
ENTRY 'Chain Member(s) | -P Path' C D=' ' VARARG
>Keyword
merge group
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
.
Examples of chain (Ntuple tree) definition:
.
 CHAIN   Year93  Jan Feb March April May ...
 CHAIN   Jan     Week1 Week2 Week3 Week4
 CHAIN   Week1   file1.hbook file2.hbook ...
 CHAIN   Week2   file3.hbook file4.hbook ...
.
 CD //Jan
 NT/PLOT 10.e     ; loop over all files in chains Week1, Week2, Week3, ...
 CD //Year93      ; loop over all files in chains Jan, Feb, March, ...
 CHAIN Year93 -P /user/delphi   ; all files from chain Year93 downward will
                                  be changed to /user/delphi/file1.hbook, ...
.
 CHAIN Year93>    ; print the chain tree Year93
 CHAIN -Feb       ; delete chain Feb
 CHAIN Jan -file3.hbook   ; delete file3.hbook from chain Jan
.
Long (around 70 characters) file names (including the path) must be avoided.
They can be misunderstood and the results produced may be wrong. A warning is 
printed, when ntuple command is executed on a chain, if the file name length 
exceed the safe value. The safe file name length is printed in the warning.
.
Note that IFIRST and NEVENT parameters are not meaningful in case of CHAINs
in the ntuple commands. 
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
>Keyword
contour
>Guidance
Define the CUTID with the format $nn.
nn is an integer between 1 and 99.
This cut can then be used in subsequent commands NTUPLE/PLOT, PROJECT.
.
 OPTION='expression'  
.
allows to define the cut CUTID. For example the command:
.
 PAW > CUTS $1 X<0.8.and.Y<SQRT(X)
.
defines the cut $1. Note that CUTID=$0 means all cuts.
>Action PANCUT

>Command CSELECT
>Parameters
+
CHOPT 'Options' C D='N'
-  Comment is left adjusted to the current zone
-R Comment is right adjusted to the current zone
-C Comment is centred to the current zone
-B Comment is drawn below the top zone line
-N All subsequent NTUPLE/PLOT commands will print the selection_
   mechanism with the options specified in CHOPT.
CSIZE 'Comment size' R D=0.28
>Keyword
SELECT selection
>Guidance
To write selection mechanism as a comment on the picture.
By default, the comment is drawn left justified above the top zone line.
Example:
 CSEL          All coming NT/PLOT commands will draw a comment
               of size CSIZE=0.28cm Left justified.
 CSEL NRB 0.4  All coming NT/PLOT commands will draw a comment
               of size 0.4 cm Right justified Below the top line.
 CSEL  CB      Draw previous selection mechanism Centred Below
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
-P Code to print events is generated (Row Wise Ntuples only).
-T Names of the Ntuple variables are generated in DATA statements _
   (Row Wise Ntuples only).
>Keyword
select selection
>Guidance
To generate the FORTRAN skeleton of a selection function or the
INCLUDE file with the columns declaration.
.
A FORTRAN function is generated if the FNAME is of the form,
xxx.f, xxx.for, xxx.fortran. Otherwise an INCLUDE file is generated.
Example:
If the Row Wise Ntuple ID=30 has variable names [X,Y,Z] then:
.
   NTUPLE/UWFUNC 30 SELECT.FOR   
.
will generate the file SELECT.FOR with:
.
         REAL FUNCTION SELECT()
         REAL
        +X       ,Y       ,Z
   *
         LOGICAL         CHAIN
         CHARACTER*128   CFILE
   *
         COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
         COMMON /PAWCHC/ CFILE
   *
         COMMON/PAWIDN/IDNEVT,OBS(13),
        +X       ,Y       ,Z
   *
         SELECT=1.
         END
.
Then using the command EDIT one can modify this file which could then
look something like (IDNEVT is the event number):
.
         REAL FUNCTION SELECT()
         REAL
        +X       ,Y       ,Z
   *
         LOGICAL         CHAIN
         CHARACTER*128   CFILE
   *
         COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
         COMMON /PAWCHC/ CFILE
   *
         COMMON/PAWIDN/IDNEVT,OBS(13),
        +X       ,Y       ,Z
   *
         IF(X**2+Y**2.GT.Z**2)THEN
            SELECT=1.
         ELSE
            SELECT=0.
         ENDIF
         END
.
   NTUPLE/UWFUNC 30 SELECT.INC 
.
will generate an include file. This include file
may be referenced in a selection function in the following way:
.
         FUNCTION SELECT()
         include 'select.inc'
   *
         SELECT=1.
         IF(X.LE.Y)SELECT=0.
         END
.
Note that the command UWFUNC is not required if the SELECT function has the
following form:
         FUNCTION SELECT()
         include ?
   *
         SELECT=1.
         IF(X.LE.Y)SELECT=0.
         END
.
In this case (thanks to the statement 'include ?') the include file will be
generated automatically with the name 'comis.inc'.
.
It is possible to add input parameters to the SELECT function and to
use them in the function's body.
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
>Keyword
reduction PRINCIPAL COMPONENTS Analysis transformation
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
.
Reduction of the variables for NEVENT events starting at IFIRST
The default is to take all the 20 first variables.
.
This command creates a file: xtoxsi.f, XTOXSI.FORTRAN, xtoxsi.for or xtoxsi.ftn.
This file contains a Fortran function which computes the new variables.
These new variables can be visualised in PAW with for example:
 PAW > Ntuple/plot id.xtoxsi.ftn(1)
 PAW > Ntuple/plot id.xtoxsi.ftn(1)%xtoxsi.ftn(3)
>Action PALINT

>Command VMEM
>Parameters
+
MXSIZE    'Maximum size of dynamic memory buffer in MBytes' I  D=-1 R=-2:128
>Keyword
memory dynamic
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
SEP1 'Value separator' C D=','
SEP2 'Expression separator (on output)' C D='%'
>Keyword
Select Selection File Output
>Guidance
For the selected events the values of the expressions are printed
to the screen (by default) or to a specified file. If the expression
is non scalar (e.g. vector) the elements of the vector are separated
by a ',' (changed with SEP1). The values of the expressions are
separated by a '%' (changed with SEP2).
The output of the DUMP command is meant for consumption by other 
computer programs, for easy inspection of an ntuple the NTUPLE/SCAN command
might be more suitable.

>Action PANNTU

>Command FLAGS_QP
>Parameters
+
OPTION    'Option name'   C
VALUE     'Option value'   I
>Keyword
debug Query Processor
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
>Keyword
Open
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

>Name GRADEF

>Menu GRAPHICS
>Keyword
HIGZ HPLOT
>Guidance
Interface to the graphics packages HPLOT and HIGZ.

>Command SET
>Parameters
+
CHATT 'Attribute name' C D='SHOW'
VALUE 'Attribute value' R D=0
>Keyword
SHOW default attribute
SIZE MarGin axis TICk tic mark distance position title label Page space
Hershey KEY title Page scatter table space 
Maximum max scale Shift Offset 
SHIft BAR fill area line WIDth COLor FONT PREC PRECision
DIVision ERRor Text angle Border IGTEXT Clipping 
XSIZ YSIZ XMGL XMGR XLAB XVAL XTIC YMGL YMGU YLAB YVAL YTIC YNPG YGTI
YHTI SMGR SMGU KSIZ GSIZ TSIZ ASIZ CSIZ PSIZ VSIZ SSIZ 2SIZ XWIN YWIN
HMAX PASS CSHI BARO BARW DASH DMOD GRID DATE FILE STAT FIT HTYP BTYP
PTYP FTYP HCOL BCOL PCOL FCOL XCOL YCOL HWID BWID PWID FWID XWID YWID
TFON GFON VFON LFON CFON NDVX NDVY NDVZ   ERRX 1DEF 2DEF FAIS FASI
LTYP BASL LWID MTYP MSCF PLCI PMCI FACI TXCI TXAL CHHE TANG TXFP PICT
BORD PASS LASI LAOF Label axis TMSI Tick Tic AWLN 
BAR IGRAPH NCOL CLIP NLIN line AURZ sav save DIME Dimension
Box HPLOT
>Guidance
Set a specific HPLOT attribute.
If CHATT='SHOW', print defaults and current values for all attributes.
If CHATT='*', restore default values for all attributes.
If VALUE=0, the attribute is set to its default value.
>User_help PAHELP
>Action PAGRAF

>Command OPTION
>Parameters
+
CHOPTN 'Option name' C D='SHOW'
>Keyword
SHOW orientation Error title table cross wires STAtistics division 
div GRID DATE 
LINear LOGarithmic HPLOT
VERT VERTical HORI HORIzontal Error NEAH EAH CHA NCHA AST AST
character asterisk SOFT HARD SOFTware HARDware SQR NSQR square
TAB NTAB TABle scatter scat BOX NBOX TIC NTIC tick
cross wires STA NSTA STAtistics
FIT NFIT ZFL NZFL PTO NPTO Turn BAR NBAR chart DVXI DVXR DVX
division axis DVYI DVYR DVY GRID NGRI DATE NDAT page plotter format 
number NOPG A4 A0 A1 A2 A3 A5 A6 LINear LOGarithmic scale 
LINY LOGY LINX LOGX LIN LOG LINZ LOGZ HSTA HNST
>Guidance
Set general plotting options for HPLOT.
If CHOPTN='SHOW' print all current and default options.
If CHOPTN='*', restore all default options.
>User_help PAHELP
>Action PAGRAF

>Command METAFILE
>Parameters
+
LUN    'Logical unit number' I D=0
METAFL 'Metafile ID'         I D=0
>Keyword
HIGZ PostScript Open LaTex Tex
>Guidance
Set the metafile logical unit and metafile type.
This command controls the destination of the subsequent graphics output.
Example:
.
 LUN =-10 output only on metafile opened on unit 10;
 LUN =  0 output only on screen;
 LUN = 10 output on both screen and metafile opened on unit 10;
.
Use the command FORTRAN/FILE to open a new file, FORTRAN/CLOSE to close it.
Example:
.
  PAW > FOR/FILE 44 test.ps
  PAW > NULL 0 10 0 10
  PAW > CLOSE 44
.
Note that PAW opens the file PAW.METAFILE on the unit 10
at initialisation time.
.
 METAFL=   4 Appendix E GKS.
 METAFL=-111 HIGZ/PostScript (Portrait).
 METAFL=-112 HIGZ/PostScript (Landscape).
 METAFL=-113 HIGZ/Encapsulated PostScript.
 METAFL=-114 HIGZ/PostScript Colour (Portrait).
 METAFL=-115 HIGZ/PostScript Colour (Landscape).
 METAFL=-777 HIGZ/LaTex Encapsulated.
 METAFL=-778 HIGZ/LaTex.
.
The PostScript metafile types have the following format:
.
                   -[Format][Nx][Ny][Type]
.
Where:

[Format] Is an integer between 0 and 99 which defines the format of the
paper. For example if Format=3 the paper is in the standard
A3 format. Format=4 and Format=0 are the same and
define an A4 page.
The A0 format is selected by Format=99.
The US format Letter is selected by Format=100.
The US format Legal is selected by Format=200.
The US format Ledger is selected by Format=300.
.
[Nx, Ny] Specify respectively the number of zones on the x and y axis.
Nx and Ny are integers between 1 and 9.
.
[Type] Can be equal to:
 1: Portrait mode with a small margin at the bottom of the page.

 2: Landscape mode with a small margin at the bottom of the page.

 4: Portrait mode with a large margin at the bottom of the page.

 5: Landscape mode with a large margin at the bottom of the page.
 The large margin is useful for some PostScript printers (very
 often for the colour printers) as they need more space to grip
 the paper for mechanical reasons. Note that some PostScript
 colour printers can also use the so called 'special A4' format
 permitting the full usage of the A4 area; in this case larger
 margins are not necessary and {\tt Type}=1 or 2 can be used.

 3: Encapsulated PostScript. This Type permits the generation of
 files which can be included in other documents, for example
 in LaTeX files. Note that with this Type, Nx and Ny must always
 be equal to 1, and Format has no meaning. The size of the picture
 must be specified by the user via the SIZE command. Therefore
 the workstation type for Encapsulated PostScript is -113. For
 example if the name of an encapsulated PostScript file is
 example.eps, the inclusion of this file into a LaTeX file will
 be possible via (in the LaTeX file):
.
                    \begin{figure}
                     \epsffile{example.eps}
                     \caption{Example of Encapsulated PostScript in LaTeX.}
                     \label{EXAMPLE}
                    \end{figure}
.
With Type=1,2,4 and 5 the pictures are centred on the page, and the usable
area on paper is proportional to the dimensions of A4 format.
Examples:
.
-111 or -4111 defines an A4 page not divided.
-6322 define an A6 landscape page divided in 3 columns and 2 rows.
.
                    +-------+-------+-------+
                    |   1   |   2   |   3   |
                    +-------+-------+-------+
                    |   4   |   5   |   6   |
                    +-------+-------+-------+
.
The first picture  will be drawn  in the area 1. After each clear the screen,
the graphics output will appear in the next area in the order defined above.
If a page is filled, a new page is used with the same grid. Note that empty
pages are not printed in order to save paper.
.
Ignoring formats smaller than A12, the total number of possible different
PostScript workstation types is: 4x9x9x13+1 = 4213 !
.
Note: this command open a metafile on the workstation identifier number 2.
>Action PAGRAF

>Command WORKSTATION
>Parameters
IWKID  'Workstation ID'   I D=1 Loop
+
CHOPT  'Options'          C D='OA'
-O Open a new workstation
-C Close a workstation
-A Activate a workstation
-D Deactivate a workstation
-L Give the list of open workstations
IWTYP  'Workstation type' I D=1
>Keyword
HIGZ Open
>Guidance
To create/delete workstations (windows) or change status.
.
    IWKID > 0  Do the action specified by CHOPT on the
               workstation identified by IWKID.
    IWKID = 0  Do the action specified by CHOPT on all
               workstations.
    IWKID < 0  Do the action specified by CHOPT on the
               workstation identified by -IWKID and the
               complementary action on all the others.
.
Note: IWKID should not be equal to 2 if a metafile is active because the
command METAFILE use it already.
>Action PAWORK

>Menu MISC
>Keyword
HPLOT
>Guidance
Miscellaneous HPLOT functions.

>Command NEXT
>Keyword
Clear
>Guidance
Clear the screen.
Initialise a new HIGZ picture if option ZFL or ZFL1 has been selected.
Select the Normalisation Transformation number 1 (cm).
>Action PAGRAF

>Command CLR
>Keyword
Clear
>Guidance
Clear the screen.
>Action PAGRAF

>Command LOCATE
>Parameters
+
NTPRI 'Transformation with highest priority' C D='-1'
CHOPT 'Options' C D='R'
-R Request mode is used to locate the points (default)
-S Sample mode is used to locate the points
-I Integrate an histogram between 2 bins
-+ Use the tracking cross (default is cross-hair)
-T The output is done on the terminal.
WKID  'Workstation identifier' I D=1
>Keyword
locator
>Guidance
Locate points on the screen using the graphics cursor
and output coordinates on terminal.
Control is returned when the BREAK (right) mouse button is
clicked (or CRTL/E) or when 20 points are located.
The optional parameter NTPRI may be specified to locate
a point in the specific transformation number NTPRI.
NTPRI=-1 (default) means that all the histogram transformation
numbers (10, 20, etc.) have priority on transformation number 1.
WKID allows to define in which window the locator is performed.
.
Note: With the Motif version of PAW the locator is automatically
invoke when the mouse cursor enter the window.
>Action PAGRAF

>Command VLOCATE
>Parameters
VECX  'Vector for coordinates X' C
VECY  'Vector for coordinates Y' C
+
CHOPT 'Options' C D=' '
-   Use the cross-hair
-+  Use the tracking cross
--  Use the rubber line
-L  Connect points by a polyline
-P  Draw the current polymarker at each point
-*  Draw a * at each point
-S  Sample mode is used. Allows to see the coordinates of_
    point before clicking
-Q  Do not print the number of points entered
NTPRI 'Transformation with highest priority' I D=-1
WKID  'Workstation identifier' I D=1
>Keyword
locator polymarker
>Guidance
Locate a set of points using the graphics cursor.
Return corresponding coordinates in vectors X and Y.
If vectors X or Y do not exist, they are automatically created.
Control is returned when the point is outside picture limits
or when the BREAK (right) mouse button is clicked (or CRTL/E).
The optional parameter NTPRI may be specified to locate
a point in the specific transformation number NTPRI (see LOCATE).
WKID allows to define in which window the locator is performed.
>Action PAGRAF

>Command HMOVE
>Keyword
channel
>Guidance
Change the contents of a histogram channel using the cursor.
Position the cursor to the channel to be changed, trigger graphics
input, position the cursor to the new channel value
(a rubber band box is used to visualise the change), trigger graphics
input to fix the new value.
>Action PAGRAF

>Menu ../VIEWING
>Keyword
normalisation
>Guidance
To define Normalisation transformations.
Either automatically (ZONE and SIZE) or 'by hand' (SVP, SWN and SELNT).

>Command ZONE
>Parameters
+
NX 'Number of divisions along X' I D=1
NY 'Number of divisions along Y' I D=1
IFIRST 'First division number' I D=1
CHOPT 'Option' C D=' ' R=' ,S'
-  Define the zones for all subsequent pictures.
-S Redefine zones on current picture
>Guidance
Subdivide the picture into NX by NY zones,
starting at zone IFIRST (count along X first).
.
Note that the command ZONE doesn't define the normalisation
transformations (see SWN, SVP and SELNT). They are define
only when commands like H/PLOT, NULL etc .. are performed.
>Action PAGRAF

>Command SIZE
>Parameters
+
XSIZE 'Size along X' R D=20.
YSIZE 'Size along Y' R D=20.
>Guidance
Set the size of the picture.
On the terminal, the pictures will have the ratio YSIZE/XSIZE, and,
if a metafile is produced, pictures will be YSIZE by XSIZE cm.
This command sets the parameters for the normalisation transformation
number 1 to [0-XSIZE], [0-YSIZE].
>Action PAGRAF

>Command SVP
>Parameters
NT 'Normalisation transformation number' I
X1 'Low X  of viewport in NDC' R D=0 R=0:1
X2 'High X of viewport in NDC' R D=1 R=0:1
Y1 'Low Y  of viewport in NDC' R D=0 R=0:1
Y2 'High Y of viewport in NDC' R D=1 R=0:1
>Keyword
viewport
>Guidance
Set the viewport of the normalisation transformation NT
in the Normalized Device Coordinates (NDC). Note that the command
SELNT should be invoke in order to validate the viewport parameters.
.
This command, and also SWN, should not be used for
a common PAW usage (H/PLOT, GRAPH etc ...). Commands like
ZONE and SIZE should be used.
>Action PAGRAF

>Command SWN
>Parameters
NT 'Normalise transformation number' I
X1 'Low X  of window in WC' R D=0
X2 'High X of window in WC' R D=20
Y1 'Low Y  of window in WC' R D=0
Y2 'High Y of window in WC' R D=20
>Keyword
normalisation transformation
>Guidance
Set the window of the normalisation transformation NT
in World Coordinates (WC). Note that the command SELNT should
be invoke in order to validate the window parameters.
.
Example:
    Nul 0 1 -1 1      | Draw an empty frame (0,1)x(-1,1)
    Line 0 0 1 1      | Draw a line in (0,1)x(-1,1)
    Swn 10 0 10 0 10  | Change the coordinates to (0,10)x(0,10)
    Selnt 10          | Activate the coordinates (0,10)x(0,10)
    Line 0 0 1 1      | Draw a line in (0,10)x(0,10)
.
This command, and also SVP, should not be used for
a common PAW usage (H/PLOT, GRAPH etc ...). Commands like
ZONE and SIZE should be used.
>Action PAGRAF

>Command SELNT
>Parameters
NT 'Normalisation transformation number' I
>Keyword
normalisation transformation
>Guidance
Select a normalisation transformation number.
  If ZONE 2 2 is active , then:        If ZONE 1 1 is active, then:
 +------------------------------+    +-----------------------------+
 |                              |    |                             |
 |  +----------+   +---------+  |    |  +-----------------------+  |
 |  |          |   |         |  |    |  |                       |  |
 |  |   NT=10  |   |  NT=20  |  |    |  |                       |  |
 |  |          |   |         |  |    |  |                       |  |
 |  +----------+   +---------+  |    |  |                       |  |
 |                              |    |  |         NT=10         |  |
 |  +----------+   +---------+  |    |  |                       |  |
 |  |          |   |         |  |    |  |                       |  |
 |  |   NT=30  |   |  NT=40  |  |    |  |                       |  |
 |  |          |   |         |  |    |  |                       |  |
 |  +----------+   +---------+  |    |  |                       |  |
 |                              |    |  +-----------------------+  |
 |             NT=1             |    |             NT=1            |
 +------------------------------+    +-----------------------------+
.
Example:
    Zone 1 2          | Define 2 zones.
    Nul 0 1 -1 1      | Draw an empty frame in the first zone
    Nul 0 1 -1 1      | Draw an empty frame in the second zone
    Line 0 0 1 1      | Draw a line in second zone
    Selnt 10          | select the first zone
    Line 0 0 1 1      | Draw a line in the first zone
>Action PAGRAF

>Menu ../PRIMITIVES

>Guidance
Call HIGZ drawing primitives
>Keyword
HIGZ

>Command PLINE
>Parameters
N 'Number of points' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
>Keyword
polyline
>Guidance
Draw a polyline of N points X,Y
in the current Normalisation transformation.
The PLINE attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NUL -1 1 0 1           | Draw a frame (cf HELP NULL)
 * Create vector X and Y (cf HELP SIGMA)
    SIGMA X=ARRAY(100,-1#1)
    SIGMA Y=X*X
    SET PLCI 4             | The line colour is blue
    SET LWID 6             | The line width is 6
    SET LTYP 2             | The line type is dashed
    PLINE 100 X Y          | Draw a 100 points line
.
This command doesn't take into account OPTIONS LOGY or LOGX, use GRAPH
instead.
>Action PAGPRI

>Command 3DPLINE
>Parameters
N 'Number of points' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
Z 'Vector name for Z coordinates' C
>Keyword
polyline
>Guidance
Draw a polyline of N points X,Y,Z
in the current Normalisation transformation.
The 3DPLINE attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    3DNUL                  | Draw a frame (cf HELP 3DNULL)
    V/CREATE X(5) R 0.2 0.8 0.8 0.2 0.2
    V/CREATE Y(5) R 0.2 0.2 0.8 0.8 0.2
    V/CREATE Z1(5) R 0.2 0.2 0.2 0.2 0.2
    V/CREATE Z2(5) R 0.8 0.8 0.8 0.8 0.8
    SET PLCI 2
    3DPL 5 X Y Z2
    3DPL 5 X Y Z1
>Action PAGPRI

>Command LINE
>Parameters
X1 'X first coordinate' R
Y1 'Y first coordinate' R
X2 'X second coordinate' R
Y2 'Y second coordinate' R
>Guidance
Draw a line connecting points (X1,Y1) and (X2,Y2)
in the current Normalisation transformation. This command is kept
for backward compatibility. It has a reverse calling sequence compare
to BOX or ARROW and it doesn't take LOG scales into account. It is
recommended to use DLINE instead. The LINE attributes can be
changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NUL 0 5 0 5            | Draw a frame (cf HELP NULL)
    SET PLCI 2             | The line colour is red
    SET LWID 6             | The line width is 6
    SET LTYP 3             | The line type is dotted
    LINE 0 0 5 5           | Draw a line
>Action PAGPRI

>Command DLINE
>Parameters
X1 'X first coordinate' R
X2 'X second coordinate' R
Y1 'Y first coordinate' R
Y2 'Y second coordinate' R
>Guidance
Draw a line connecting points (X1,Y1) and (X2,Y2)
in the current Normalisation transformation taking care of
logarithmic scales. The DLINE attributes can be
changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    OPTION LOGY            | Log scale on the Y axis.
    NUL 0 5 1 100          | Draw a frame (cf HELP NULL)
    SET PLCI 2             | The line colour is red
    SET LWID 6             | The line width is 6
    SET LTYP 1             | The line type is solid
    DLINE 0 5 1 10         | Draw a line
>Action PAGPRI

>Command FAREA
>Parameters
N 'Number of points' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
>Keyword
Fill area
>Guidance
Fill the area defined by the N points X,Y
in the current Normalisation transformation.
The FAREA attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NUL -1.1 1.1 -1.1 1.1  | Draw a frame (cf HELP NULL)
 * Create vector X and Y (cf HELP SIGMA)
    SIGMA X=ARRAY(100,-3.14#3.14)
    SIGMA Y=SIN(X)*COS(X)
    SIGMA X=COS(X)
    SET FACI 2             | The fill area colour is red
    SET FAIS 1             | The fill area interior style is solid
    FAREA 100 X Y          | Draw a 100 points line
    SET FACI 1             | The fill area colour is black
    SET FAIS 0             | The fill area interior style is hollow
    FAREA 100 X Y          | Draw a 100 points line
    SET FAIS 3             | The fill area interior style is hatched
    SET FASI 245           | Defines the type of hatches
    FAREA 100 X Y          | Draw a 100 points line
.
This command doesn't take into account OPTIONS LOGY or LOGX, use GRAPH
instead.
>Action PAGPRI

>Command PMARKER
>Parameters
N 'Number of points' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
>Keyword
polymarker
>Guidance
Draw polymarkers at the N points X,Y
in the current Normalisation transformation.
The PMARKER attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NUL -3.2 3.2 -1 1      | Draw a frame (cf HELP NULL)
 * Create vector X and Y (cf HELP SIGMA)
    SIGMA X=ARRAY(100,-3.14#3.14)
    SIGMA Y=SIN(X)*COS(X)
    SET PMCI 6             | The marker colour is magenta
    SET MTYP 3             | The marker type is *
    SET MSCF 2             | The marker size is 2
    PMARKER 100 X Y        | Draw a 100 points polymarker
.
This command doesn't take into account OPTIONS LOGY or LOGX, use GRAPH
instead.
>Action PAGPRI

>Command 3DPMARKER
>Parameters
N 'Number of points' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
Z 'Vector name for Z coordinates' C
>Keyword
polymarker
>Guidance
Draw a polymarker at the N points X,Y,Z
in the current Normalisation transformation.
The 3DPMARKER attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    3DNUL                  | Draw a frame (cf HELP 3DNULL)
    V/CREATE X(5) R 0.2 0.8 0.8 0.2 0.2
    V/CREATE Y(5) R 0.2 0.2 0.8 0.8 0.2
    V/CREATE Z1(5) R 0.2 0.2 0.2 0.2 0.2
    V/CREATE Z2(5) R 0.8 0.8 0.8 0.8 0.8
    SET MTYP 3
    3DPM 5 X Y Z2
    3DPM 5 X Y Z1
>Action PAGPRI

>Command BOX
>Parameters
X1 'X coordinate of first corner' R
X2 'X coordinate of second corner' R
Y1 'Y coordinate of first corner' R
Y2 'Y coordinate of second corner' R
>Keyword
Fill area
>Guidance
Draw and fill a box
with the current fill area and line attributes.
Use the current Normalisation transformation.
This command it doesn't LOG scales. It is recommended 
to use DBOX instead.
The BOX attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 10         | Draw a frame (cf HELP NULL)
    SET FAIS 0             | Fill area interior style hollow
    BOX 1 3 1 3            | Draw a box
    SET FAIS 1             | Fill area interior style solid
    BOX 1 3 3 5            | Draw a box
    SET FAIS 3             | Fill area interior style hatched
    SET FASI 245           | Changes the type of hatches
    BOX 1 3 5 7            | Draw a box
    SET FASI 3             | Changes the type of hatches
    BOX 3 5 5 7            | Draw a box
    SET BORD 1             | The border is requested
    SET PLCI 2             | Line colour is red
    SET FASI 4             | Changes the type of hatches
    BOX 5 7 5 7            | Draw a box
>Action PAGPRI

>Command DBOX
>Parameters
X1 'X first coordinate' R
X2 'X second coordinate' R
Y1 'Y first coordinate' R
Y2 'Y second coordinate' R
>Keyword
Fill area box
>Guidance
Draw and fill a box
with the current fill area and line attributes.
Use the current Normalisation transformation
taking care of logarithmic scales.
The BOX attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    OPT LOGY ; OPT LOGX    | Use log scale
    NULL 0 10 0 10         | Draw a frame (cf HELP NULL)
    SET FAIS 0             | Fill area interior style hollow
    DBOX 1 3 1 3           | Draw a box
>Action PAGPRI

>Command FBOX
>Parameters
X1 'X coord of first corner of ext box' R
X2 'X coord of second corner of ext box' R
Y1 'Y coord of first corner of ext box' R
Y2 'Y coord of second corner of ext box' R
X3 'X coord of first corner of int box' R
X4 'X coord of second corner of int box' R
Y3 'Y coord of first corner of int box' R
Y4 'Y coord of second corner of int box' R
>Keyword
Fill area frame box
>Guidance
Draw and fill a frame (2 nested boxes)
with the current fill area and line attributes.
Use the current Normalisation transformation.
The FBOX attributes can be changed with the command SET.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 10         | Draw a frame (cf HELP NULL)
    SET FAIS 3             | Fill area interior style hatched
    SET FASI 3             | Changes the type of hatches
    SET FACI 2             | Fill are colour is red
    SET PLCI 4             | Line colour is blue
    SET LWID 8             | The line width is 8
    SET BORD 1             | The border is requested
    FBOX 1 9 1 9 3 7 3 7   | Draw a frame box
>Action PAGPRI

>Command ARROW
>Parameters
X1 'X coordinate of start point' R
X2 'X coordinate of end point' R
Y1 'Y coordinate of start point' R
Y2 'Y coordinate of end point' R
+
SIZE 'Arrow size' R D=0.4
>Guidance
Draw an arrow
Use the current Normalisation transformation.
The ARROW attributes can be changed with the command SET.
ARROW and LINE attributes are the same.
.
    (X1,Y1) ----> (X2,Y2) if SIZE>0.
    (X1,Y1) <---> (X2,Y2) if SIZE<0.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 7          | Draw a frame (cf HELP NULL)
    ARROW 1 9 1 1 .2       | Draw a simple arrow (left to right)
    ARROW 9 1 2 2 .4       | Draw a simple arrow (right to left)
    ARROW 1 9 3 3 -.8      | Draw a double arrow
    SET PLCI 2             | Arrow colour is red
    ARROW 1 9 4 4 -.8      | Draw a double arrow
    SET LWID 8             | Arrow line width is 8
    ARROW 1 9 5 5 -.8      | Draw a double arrow
    SET LTYP 3             | Arrow line type is dotted
    ARROW 1 9 6 6 -.8      | Draw a double arrow
>Action PAGPRI

>Command HELIX
>Parameters
+
X1  'X coordinate of the begin of helix' R D=0.
Y1  'Y coordinate of the begin of helix' R D=0.
X2  'X coordinate of the end of helix' R D=10.
Y2  'Y coordinate of the end of helix' R D=10.
R   'Radius of helix' R D=.3
WI  'Number of turns  ' R D=1.
PHI 'Projection angle ' R D=15.
>Guidance
Draw an helix
with the current line attributes.
Use the current Normalisation transformation.
Feynman graph: gluon phi = 30, photon phi = 0.
.
Example:
    SET * ; OPT *             | Reset the defaults
    NUL 0 10 0 10 'AB'        | Draw a frame (cf HELP NULL)
    HELIX 1 1 3 3 ! 10 !      | Draw an helix
    SET LWID 8                | Helix line width is 8
    HELIX 3 3 7 7 1  5 !      | Draw an helix
    SET PLCI 2                | Arrow colour is red
    SET LTYP 2                | Helix line type is dashed
    HELIX 7 7 10 10 .2 5 10   | Draw an helix
>Action PAFEYN

>Command ARCHELIX
>Parameters
+
X1  'X coordinate of the begin of helix' R D=0.
Y1  'Y coordinate of the begin of helix' R D=0.
X2  'X coordinate of the end of helix' R D=10.
Y2  'Y coordinate of the end of helix' R D=10.
R   'Radius of helix' R D=.3
WI  'Number of turns  ' R D=1.
PHI 'Projection angle ' R D=30.
RL  'Radius   of loop ' R D=15.
>Keyword
helix
>Guidance
Draw an archelix
with the current line attributes.
Use the current Normalisation transformation.
Feynman graph: gluon phi = 30, photon phi = 0.
.
Example:
    SET * ; OPT *               | Reset the defaults
    NUL 0 10 0 10 'AB'          | Draw a frame (cf HELP NULL)
    ARCHELIX 1 1 3 3 ! 9 ! 1    | Draw an helix
    SET LWID 8                  | Helix line width is 8
    ARCHELIX 3 3 7 7 ! 9 ! 1    | Draw an helix
    SET PLCI 2                  | Arrow colour is red
    SET LTYP 2                  | Helix line type is dashed
    ARCHELIX 7 7 10 10 ! 9 ! 3  | Draw an helix
>Action PAFEYN

>Command ARLINE
>Parameters
+
X1  'X coordinate of the begin' R D=0.
Y1  'Y coordinate of the begin' R D=0.
X2  'X coordinate of the end' R D=10.
Y2  'Y coordinate of the end' R D=10.
H   'arrow size' R D=.5
>Keyword
line arrow
>Guidance
Draw a line with arrow in middle (fermion line)
with the current line and fill area attributes.
Use the current Normalisation transformation.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 6          | Draw a frame (cf HELP NULL)
    ARLINE 1 1 9 1 .2      | Draw a arrow line (left to right)
    ARLINE 9 2 1 2 .4      | Draw a arrow line (right to left)
    SET PLCI 2             | Arrow colour is red
    SET FAIS 1             | Fill area interior style solid
    ARLINE 9 3 1 3 .4      | Draw a arrow line (right to left)
    SET LWID 8             | Arrow line width is 8
    SET FACI 4             | The fill area colour is blue
    ARLINE 9 4 1 4 .4      | Draw a arrow line (right to left)
    SET LTYP 3             | Arrow line type is dotted
    ARLINE 9 5 1 5 .4      | Draw a arrow line (right to left)
>Action PAFEYN

>Command FPOINT
>Parameters
+
X  'X      ' R D=0.
Y  'Y      ' R D=0.
R  'Radius ' R D=.5
>Keyword
Fill area vertex
>Guidance
Draw a filled point (vertex)
with the current fill area attributes.
Use the current Normalisation transformation.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 10         | Draw a frame (cf HELP NULL)
    SET FAIS 1             | Fill area interior style solid
    FPOINT 5 1 .1          | Draw a filled point
    FPOINT 5 3 .2          | Draw a filled point
    FPOINT 5 5 .3          | Draw a filled point
    SET FACI 4             | The fill area colour is blue
    FPOINT 5 7 .4          | Draw a filled point
    FPOINT 5 9 .5          | Draw a filled point
>Action PAFEYN

>Command AXIS
>Parameters
X0 'X axis origin in WC' R
X1 'X end axis in WC' R
Y0 'Y axis origin in WC' R
Y1 'Y end axis in WC' R
WMIN 'Lowest value for labels' R
WMAX 'Highest value for labels' R
NDIV 'Number of divisions' I D=510
+
CHOPT 'Options' C D=' '
-   Draw an axis with default values.
-G  Logarithmic scale, default is linear.
-B  Blank axis. Useful to superpose axis.
-U  Unlabelled axis, default is labelled.
-+  Tick marks are drawn on Positive side. (default)
--  Tick marks are drawn on the negative side.
-=  Tick marks are drawn on Equal side
-P  Labels are drawn Parallel to the axis
-O  Labels are drawn Orthogonal to the axis (Top to Down).
-0  Labels are drawn Orthogonal to the axis (Down to Top).
-R  labels are Right adjusted on tick mark.
-L  labels are Left adjusted on tick mark.
-C  labels are Centred on tick mark.
-M  In the Middle of the divisions.
-Y  Direction of labels DOWN . Default is RIGHT
-.  Dot obligatory
-T  Alphanumeric labels .
-S  Tick marks Size
-H  Labels Height
-D  Distance labels-axis
-N  No binning optimisation
-I  Integer labelling
>Keyword
logarithmic log div division label tic tick 
>Guidance
Draw an axis in the current Normalisation transformation.
            NDIV=N1 + 100*N2 + 10000*N3
N1, N2, N3 = Number of 1st, 2nd, 3rd divisions respectively, eg:.
            NDIV=0 --> no tick marks.
            NDIV=2 --> 2 divisions, one tick mark in the middle
                       of the axis.
.
Orientation of tick marks on axis: Tick marks are normally drawn
on the positive side of the axis.However, if X0=X1, then Negative .
        CHOPT='+': tick marks are drawn on Positive side. (default)
        CHOPT='-': tick marks are drawn on the negative side.
        i.e: '+-' --> tick marks are drawn on both sides of the axis.
.
Position of labels on axis: Labels are normally drawn on side
opposite to tick marks.However:
        CHOPT= '='   on Equal side
.
Orientation of labels on axis: Labels are normally drawn
parallel to the axis. However:
        if X0=X1, then Orthogonal
        if Y0=Y1, then Parallel
        CHOPT= 'P' : Parallel to the axis
        CHOPT= 'O' : Orthogonal to the axis (Top to Down).
        CHOPT= '0' : Orthogonal to the axis (Down to Top).
.
Position of labels on tick marks: Labels are centred on
tick marks. However , if X0=X1, then they are right adjusted.
        CHOPT='R': labels are Right adjusted on tick mark.
                   (default is centred)
        CHOPT='L': labels are Left adjusted on tick mark.
        CHOPT='C': labels are Centred on tick mark.
        CHOPT='M': In the Middle of the divisions.
.
Direction of labels: Default is RIGHT
        CHOPT='Y':   Down
.
Format of labels: Blank characters are stripped, and then the
label is correctly aligned. The dot,if last character of the
string, is also stripped, unless
        CHOPT='.'     Dot obligatory
.
In the following, we have some parameters, like
tick marks length and characters height (in percentage
of the length of the axis).The default values are as follows:
        Primary tick marks: 3.0 %
        Secondary tick marks: 1.5 %
        Third order tick marks: .75 %
        Characters height for labels: 2%
        Characters spacing (related to height): 40%
        Labels offset: 4.0 %
.
Type of labels: Labels are normally numeric . However, alphanumeric
labels can be drawn (see command LABEL).
        CHOPT='T':   Alphanumeric labels .
.
Intrinsic parameters: These values can be changed with the command
SET. The default value is used unless the corresponding option is
selected by CHOPT:
        CHOPT='D' The distance between the labels and the axis
                  (the offset) is given by the preceding command
                  SET with the parameter LAOF.
        CHOPT='H' The size (height) of the labels is given by the
                  preceding command SET with the parameter LASI.
        CHOPT='S' The size of the tick marks is given by the preceding
                  command SET with the parameter TMSI.
.
Axis binning optimisation: By default the axis binning is optimised .
        CHOPT='N': No binning optimisation
        CHOPT='I': Integer labelling
.
Example:
    SET * ; OPT *                          | Reset the defaults
    NUL 0 12 0 12 'A'                      | Draw a frame (cf HELP NULL)
    AXIS 1  11  1  1  0  100  510 'A'      | Axis with arrow
    AXIS 1  11  3  3  1  10000  510 'G'    | LOG axis
    LABEL 1 11 a b c d e f g h i j k       | define alphanumeric labels
    AXIS 1  11  5  5  0  12  11 'NATY'     | alphanumeric labelling
    AXIS 1  11  6  6  -100  0  510 'A'
    AXIS 11  1  7  7  -100  0  810 'A+-'   | Double side tick marks
    AXIS 1  11  8  11  0  1234567  615 'A' | exponent is required
.
Note that the command TIC provides a simpler interface to redraw axis
on the current plot. Moreover it use the NDVX, NDVY etc .. attributes.
>Action PAGPRI

>Command ARC
>Parameters
X1 'X coordinate of centre' R
Y1 'Y coordinate of centre' R
R1 'Inner radius' R
+
R2 'Outer radius' R D=-1.
PHIMIN 'Minimum angle' R D=0.
PHIMAX 'Maximum angle' R D=360.
>Keyword
polyline circle
>Guidance
Draw an arc of circle
with the current fill area and line attributes.
Use the current Normalisation transformation.
If R1 is not equal to R2 the area between the two arcs of radius
R1 and R2 is filled according to the current fill area attributes.
The border is never drawn unless the interior style is hollow or
the command SET BORD 1 has been called. If R1 is equal to R2 a
polyline is drawn.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 20 0 20 'AB'    | Draw a frame (cf HELP NULL)
    SET PLCI 2             | Line colour is red
    SET LWID 6             | Line width is 6
    ARC  5  5 4 4 ! !      | Draw an circle
    ARC  5 15 4 4 30 260   | Draw an arc of circle
    SET FAIS 3             | Fill area with hatches
    SET FASI 3             | Type of hatches
    ARC 15 15 1 4 ! !      | Draw an arc
    SET BORD 1             | Border is requested
    ARC 15  5 1 4 30 !     | Draw an arc
>Action PAGPRI

>Command ELLIPSE
>Parameters
XC       'X coord of centre'             R
YC       'Y coord of centre'             R
RX       'X radius of ellipse'           R
+
RY       'Y radius of ellipse'           R D=0.
PHIMIN   'Minimum angle (degrees)'       R D=0.
PHIMAX   'Maximum angle (degrees)'       R D=360.
THETA    'Rotation of axes of (degrees)' R D=0.
>Guidance
Draws an ellipse in the current normalisation transformation.
The parameter THETA rotates the ellipse major and minor axes
(RX and RY) relative to the coordinates by the given angle.
The a filled area is used, so the ellipse may be filled by
changing the appropriate SET parameters.
.
Example:
    SET * ; OPT *          | Reset the defaults
    NULL 0 10 0 10 'AB'    | Draw a frame (cf HELP NULL)
    SET PLCI 2             | Line colour is red
    SET LWID 6             | Line width is 6
    SET BORD 1             | Border on
    SET FAIS 1             | Filled area
    SET FACI 3             | Filled area are green
    ELLIPSE  5 5 3 5
>Action PAGPRI

>Command PIE
>Parameters
X0 'X coordinate of centre of the pie' R
Y0 'Y coordinate of centre of the pie' R
RADIUS 'Radius of the pie chart' R
N 'Number of values' I
VALUES 'Vector name for N values' C
+
CHOPT 'Options' C D=' '
-  Draw a Pie Chart with default values.
-C Colours array is present.
-L Alphanumeric labels are required.
-O Offset array is present.
-N The label of each slice will be the corresponding numeric value in_
   array VALUES.
-P The label of each slice will be in expressed in percentage.
-S Style array is present.
-H Force the labels size to be the current character height. Without_
   this option the labels size is computed automatically.
-R Draw the labels aligned on the radius of each slice.
IAO 'Name of vector with offsets' C D=' '
IAS 'Name of vector with styles' C D=' '
IAC 'Name of vector with colours' C D=' '
>Keyword
chart
>Guidance
Draw a pie chart
in the current Normalisation transformation.
.
Example:
    SET * ; OPT *                                 | Reset the defaults
    NULL 0 20 0 20 'AB'                           | Draw a frame
    LABEL 1 5 'Lab1' 'Lab2' 'Lab3' 'Lab4' 'Lab5'  | define labels
 * Initialise vectors
    V/CRE VWS(5) R 28.3 18.6 16.9 13.5 22.7
    V/CRE OFFSET(5)  R 2*0. 2*20. 0.
    V/CRE COLOUR(5)  R 2 3 4 5 6
    SET FAIS 1                                    | Fill solid
    SET BORD 1                                    | Draw the border
    PIE 10. 10. 7.  5 VWS 'L' OFFSET ! COLOUR     | Draw the pie chart
>Action PAGPRI

>Command TEXT
>Parameters
X 'X coordinate' R
Y 'Y coordinate' R
TEXT 'Text to be drawn' C
SIZE 'Text size' R D=0.3
+
ANGLE 'Comment angle' R D=0
CHOPT 'Justification option' C D='L'
-L Text is Left justified.
-C Text is Centred.
-R Text is Right justified.
>Keyword
PASS TXCI IGTEXT CSHI
>Guidance
Draw text at position X,Y
in the current normalisation transformation using the software font
IGTEXT. SIZE is always given in centimetres.
A boldface effect can be obtained using the parameters PASS and
CSHI of the command SET. The text colour can be changed by SET TXCI.
.
Example:
    SET * ; OPT *                                   | Reset the defaults
    NULL 0 10 0 10                                  | Draw a frame
    TEXT 5 1 'Left justified' .5 0. L
    TEXT 5 2 'Centred' .5 0. C
    TEXT 5 3 'Right justified' .5 0. R
    TEXT 5 4 '-- 30 degrees' .5 30. L
    TEXT 5 4 '-- 60 degrees' .5 60. L
    TEXT 5 4 '-- 90 degrees' .5 90. L
    TEXT 5 4 '-- 120 degrees' .5 120. L
    TEXT 5 4 '-- 150 degrees' .5 150. L
    TEXT 5 8 'Some Greek ... [a, b, c, d]' .5 0. C
    Set PASS 7                                      | Number of passes
    TEXT 5 9 'Bold TEXT' .5 0. C
>Action PAGPRI

>Command ITX
>Parameters
X 'X coordinate' R
Y 'Y coordinate' R
TEXT 'Text to be drawn' C
>Keyword
TXFP CHHE TXCI TXAL TANG text
>Guidance
Draw text at position X,Y in the current Normalisation transformation,
using the current font parameters.
The font and the precision can be changed by SET TXFP.
The character size can be changed by SET CHHE.
The text colour can be changed by SET TXCI.
The text orientation can be changed with SET TXAL.
The text angle can be changed by SET TANG.
.
Example:
    SET * ; OPT *                         | Reset the defaults
    NULL 0 10 0 6                         | Draw a frame
    SET TXFP -20                          | Times bold
    SET CHHE .5                           | Text size 0.5 cm
    SET TXAL 10                           | Horizontal align. Left
    ITX 5 1 'Left justified'
    SET TXAL 20                           | Horizontal align. Center
    ITX 5 2 'Centred'
    SET TXAL 30                           | Horizontal align. Right
    ITX 5 3 'Right justified'
    SET TXAL 12                           | Vertical align. Top
    ITX .2 4 'Top justified'
    SET TXAL 13                           | Vertical align. Middle
    ITX .2 5 'Middle justified'
    SET TXAL  0                           | Default align.
    SET TANG 30                           | Angle 30 degrees
    ITX 5 4 '-- 30 degrees --'
>Action PAGPRI

>Command LABELS
>Parameters
LABNUM 'Label identifier' I D=1 R=1:9
NLABS 'Number of labels' I D=0 R=0:50
CHLABS 'List of labels' C D=' ' VARARG
>Keyword
PIE AXIS NDVX NDVY
>Guidance
Define a list of alphanumeric labels
to be used by subsequent commands such as PIE and AXIS. The position of
the labels on the axis may be changed with SET NDVX (NDVY).
.
Example:
    SET * ; OPT *                 | Reset the defaults
    ZONE 1 3
    LABEL 1 3 AAAAA BBBBB CCCCC   | Define labels
    SET NDVX 3.15                 | 3 div, lab id 1, 5=center on bin
    NULL 0 10 0 1                 | Draw a frame
    SET NDVX 3.11                 | 3 div, lab id 1, 1=center on tick
    NULL 0 10 0 1                 | Draw a frame
    SET NDVX 3.18                 | 3 div, lab id 1, 8=bottom -> up
    NULL 0 10 0 1                 | Draw a frame
.
A full description of the possible alignments is given in the PAW
manual (see NDVX in the index).
>Action PAGPRI

>Command PAVE
>Parameters
X1 'X bottom left corner of box' R
X2 'X top right corner of box' R
Y1 'Y bottom left corner of box' R
Y2 'Y top right corner of box' R
+
DZ 'Box width' R D=0.4
ISBOX 'Box style' I D=0
ISFRAM 'Frame style' I D=5
CHOPT 'Option' C D='TR'
-TR  Top and Right frame are drawn
-TL  Top and Left frame
-BR  Bottom and Right frame
-BL  Bottom and Left frame
-L   Left frame only
-R   Right frame only
-T-  Top frame only pointing left
-B-  Bottom frame only pointing left
-S   Shadow mode
-K   Key mode
>Keyword
polyline block box
>Guidance
Draw a paving-block (box with 3D effect).
ISBOX (ISFRAM) may be 1000+ICOLOR where ICOLOR is the colour index
of the box (frame), otherwise the style index.
If ISBOX (ISFRAM) = 0, only the box contour is drawn with the current
polyline attributes.
.
Example:
    SET * ; OPT *                         | Reset the defaults
    NULL 0 10 0 10                        | Draw a frame
    PAVE 1 4 1 4 ! ! 1001 CHOPT=TRS
    PAVE 5 9 1 4 ! ! 1001 CHOPT=BLS
    PAVE 1 4 5 9 ! !    3 CHOPT=TR
    PAVE 5 9 5 9 ! !    3 CHOPT=BL
>Action PAGPRI

>Command HIST
>Parameters
N 'Number of values' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
+
CHOPT 'Options' C D='AHW'
-A  X and Y axes are drawn (default).
-H  An histogram is drawn as a contour (default).
-W  The Window/Viewport parameters are automatically computed_
    from the X and Y values (default).
-R  The histogram is Rotated, i.e. the values in X are used_
    for the ordinate and the values in Y for the abscissa_
    (default is the contrary)._
    If option R is selected (and option 'N' is not selected),_
    the user must give:_
       2 values for Y (Y(1)=YMIN and Y(2)=YMAX)_
       N values for X, one for each bin._
    Otherwise the user must give:_
       N values for Y, one for each bin._
       2 values for X (X(1)=XMIN and X(2)=XMAX)_
    If option 'N' is selected see below.
-N  Non equidistant bins (default is equidistant)._
    The arrays X and Y must be dimensioned as follows:_
    If option R is not selected (default) then give:_
       (N+1) values for X (limits of bins)._
       N values for Y, one for each bin._
    Otherwise give:_
       (N+1) values for Y (limits of bins)._
       N values for X, one for each bin.
-F  The area delimited by the histogram is filled according_
    to the fill area interior style and the fill area style_
    index or colour index._
    Contour is not drawn unless CHOPT='H' is also selected.
-C  A Smooth curve is drawn across points at the centre of_
    each bin of the histogram.
-L  A straight Line is drawn across points at the centre of_
    each bin of the histogram.
-*  A star is plotted at the center of each bin of the_
    histogram.
-P  Idem as '*' but with the current marker.
-B  A Bar chart with equidistant bins is drawn as fill areas._
    (Contours are drawn). The bar origin and the bar_
    width can be controlled by the routine SET using the_
    options BARO and BARW respectively.
>Keyword
histogram array vector contour
>Guidance
Draw an histogram defined by arrays X and Y.
The number of components needed in vectors X and/or in Y may be
dependent upon the value of CHOPT (see options 'R' and 'N').
By default if the option 'N' is not given, X(1) contains the X
minimum value and X(2) the X maximum value (the others are
ignored as the example show it).
To set Log scales in X and/or Y, use OPT LOGX/LOGY.
Note that when an option is specified, it is also necessary to
specify the options 'W' or 'HW' in order to start a new zone
or/and draw the axes.
.
Example
    SET * ; OPT *                         | Reset the defaults
    Zone 1 2
 * This command needs vectors
    V/CREATE Y(10) r 1 2 3 4 5 5 4 3 2 1
    V/CREATE X(11) r 1 2 4 6 8 10 15 16 20 21 30
    HIST 10 X Y 'WH'                      | Equidistant bins
    HIST 10 X Y 'HWN'                     | Non Equidistant bins
>Action PAGPRI

>Command GRAPH
>Parameters
N 'Number of values' I
X 'Vector name for X coordinates' C
Y 'Vector name for Y coordinates' C
+
CHOPT 'Options' C D='ALW'
-A  X and Y axes are drawn (default).
-L  Every point is connected with a straight line. (default)
-W  The Window/Viewport parameters are automatically computed_
    from the X and Y values (default).
-C  The values in Y are plotted in the form of a smooth curve._
    A Spline approximation algorithm is used.
-F  A fill area is drawn. If the option 'CF' is used the contour_
    of the fill area is smooth. The border of the fill area_
    is drawn if the command SET BORD 1 has been typed. The fill_
    area type may be changed via the SET parameters FASI and FASI
-R  The graph is Rotated, i.e. the values in X are used_
    for the ordinate and the values in Y for the abscissa_
    (default is the contrary).
-B  A Bar chart with equidistant bins is drawn as fill areas._
    (Contours are drawn). The bar origin and the bar_
    width can be controlled by the routine SET using the_
    options BARO and BARW respectively.
-*  A star is plotted at every point.
-P  A marker is plotted at every point,_
    according to current marker type and polymarker colour index.
>Keyword
curve
>Guidance
Draw a curve through a set of points.
X and Y are real vectors.
To set Log scales in X and/or Y, use OPT LOGX/LOGY.
Note that when an option is specified, it is also
necessary to specify the options 'AW' or 'ALW' in order to start a new
zone or/and draw the axes.
.
Example
    SET * ; OPT *                           | Reset the defaults
    ZONE 1 2
 * This command needs vectors
    V/CREATE Y(10) r 1 2 3 4 5 5 4 3 2 1
    V/CREATE X(11) r 1 2 4 6 8 10 15 16 20 21
    GRAPH 10 X Y 'WC*L'                     | Draw an 'open' graph
    SET FAIS 3                              | Interior style: hatched
    SET FASI 245                            | Define hatches type
    SET BORD 1                              | Border requested
    NULL 0 22 0 6                           | define new scales
    GRAPH 10 X Y 'CF*'                      | Draw an 'closed' graph
>Action PAGPRI

>Menu ../ATTRIBUTES
>Keyword
HIGZ
>Guidance
Change HIGZ attributes.

>Command COLOR_TABLE
>Parameters
ICOL   'Colour Index'      I D=1
+
RED    'Weight of red'    R D=0. R=0.:1.
GREEN  'Weight of green'  R D=0. R=0.:1.
BLUE   'Weight of blue'   R D=0. R=0.:1.
>Guidance
Define the colour ICOL.
>Action PAGKSA

>Command PALETTE
>Parameters
PALNB 'Palette number'                    I D=0 R=0:9
+
NEL   'Number of elements in the palette' I D=0 R=0:50
LIST  'List of the palette elements'      I D=0
>Keyword
color colour
>Guidance
Define a palette of attributes.
The palette number is used in the command SET. The command
SET HCOL 0.1 defines the palette number 1 as colour indices
used by the command LEGO in case of stacked lego plots and
plotting of SURFACE with options 1 or 2, LEGO with option 2
and CONTOUR with option 3.
.
By default the palettes are initialised with 6 elements:
2,3,4,5,6,7.
.
If the number of elements (NEL) is equal to 0 (default), the
palette is filled automatically according to the number of
colours defined with the command SET NCOL:
.
a) If NCOL is smaller or equal to 8, the palette is filled with a
subset of the 8 basic colours.
Example:
.
    PAW > SET NCOL 8        | Define the number of colours
    PAW > PALETTE 1         | The palette 1 is filled with
                            | 8 elements: 0,5,7,3,6,2,4,1
    PAW > SET NCOL 4        | Define the number of colours
    PAW > PALETTE 1         | The palette 1 is filled with
                            | 4 elements: 0,5,7,3
.
b) If NCOL is greater than 8, the palette is filled
with colours varying continuously from blue to red. This is
called a 'geographical palette'.
Example:
.
    PAW > SET NCOL 16     | Define the number of colours
    PAW > PALETTE 1       | Fill palette 1 with 8 elements
                          | (8,9,10,11,12,13,14,15) varying
                          | continuously from blue to red
.
Note that after the command SET NCOL, the colour indices from
8 to NCOL are set with gray levels. The command PALETTE 1
reset the same indices with a 'geographical palette' varying
continuously from blue to red.
>Action PAGKSA


>Menu ../HPLOT
>Keyword
HPLOT
>Guidance
Draw various HPLOT objects (symbols, errors, key, etc.).

>Command SYMBOLS
>Parameters
X 'Vector of X coordinates' C
Y 'Vector of Y coordinates' C
N 'Number of points' I D=1
+
ISYMB 'Symbol number' I D=24
SSIZE 'Symbol size' R D=0.28
>Guidance
Draw the same symbol at several points x,y
in the current normalisation transformation.
>Action PAGHPL

>Command ERRORS
>Parameters
X  'Vector of X coordinates' C
Y  'Vector of Y coordinates' C
EX 'Vector of X error bars'  C
EY 'Vector of Y error bars'  C
N  'Number of points'        I D=1
+
ISYMB 'Symbol number' I D=24
SSIZE 'Symbol size'   R D=0.28
CHOPT 'Options'       C D=' '
-  Coordinates are expressed in histogram coordinates_
   (of the last drawn histogram). Error bars are drawn.
-C Coordinates are expressed in centimetres.
-W A new window is defined and axis are drawn.
-1 Draw small lines at the end of the error bars.
-2 Draw error rectangles.
-3 Draw a filled area through the end points of the vertical error bars.
-4 Draw a smoothed filled area through the end points_
   of the vertical error bars.
-0 Turn off the symbols clipping.
>Guidance
Draw (according to the CHOPT value) a series of points using
a symbol and error bars in horizontal and vertical direction
in the current normalisation transformation.
.
By default, the symbols are not drawn if they are on the
edges of the plot: the option '0' allows to turn off this
symbols clipping.
.
With Option 1, the size of the tick marks at the end of the error
bars is equal to the marker size and can be changed with SET KSIZ.
.
If ISYMB = 0 or SSIZE = 0. no symbol is drawn.
.
Note that the options can be cumulated.
>Action PAGHPL

>Command AERRORS
>Parameters
X   'Vector of X coordinates'      C
Y   'Vector of Y coordinates'      C
EXL 'Vector of X error bars (Low)' C
EXU 'Vector of X error bars (Up)'  C
EYL 'Vector of Y error bars (Low)' C
EYU 'Vector of Y error bars (Up)'  C
N   'Number of points'             I D=1
+
ISYMB 'Symbol number' I D=24
SSIZE 'Symbol size'   R D=0.28
CHOPT 'Options'       C D=' '
-  Coordinates are expressed in histogram coordinates_
   (of the last drawn histogram). Error bars are drawn.
-C Coordinates are expressed in centimetres.
-W A new window is defined and axis are drawn.
-1 Draw small lines at the end of the error bars.
-2 Draw error rectangles.
-3 Draw a filled area through the end points of the vertical error bars.
-4 Draw a smoothed filled area through the end points_
   of the vertical error bars.
-0 Turn off the symbols clipping.
>Keyword
error errors
>Guidance
Draw (according to the CHOPT value) a series of points using
a symbol and asymmetric error bars in horizontal and vertical
direction in the current normalisation transformation.
.
By default, the symbols are not drawn if they are on the
edges of the plot: the option '0' allows to turn off this
symbols clipping.
.
With Option 1, the size of the tick marks at the end of the error
bars is equal to the marker size and can be changed with SET KSIZ.
.
If ISYMB = 0 or SSIZE = 0. no symbol is drawn.
.
Note that the options can be cumulated.
>Action PAGHPL

>Command KEY
>Parameters
X     'X coordinate of comment' R
Y     'Y coordinate of comment' R
+
IATT  'Attribute value'         I D=24
TEXT  'Legend'                  C D=' '
DX    'Box width'               R D=1.
CHOPT 'Options'                 C D=' '
-   IATT is a marker type
-F  IATT is a fill area colour index
-H  IATT is a hatches type
-L  IATT is a line type
-W  IATT is a line width
>Keyword
legend
>Guidance
Draw one legend and its explanation at a point x,y
in the current normalisation transformation.
.
The legend can be:
 - A marker type (default)
 - A filled box (CHOPT=F), in this case IATT is a colour
   and DX is the width of the box.
 - A hatched  box (CHOPT=H), in this case IATT is a hatches type
   and DX is the width of the box.
 - A line (CHOPT=L), in this case IATT is a line type
   and DX is the length of the line.
 - A line (CHOPT=W), in this case IATT is a line width
   and DX is the length of the line.
.
Example
    SET * ; OPT *                     | Reset the defaults
    NUL 0 10 0 8 A                    | Draw a frame
    KEY 5 2 ! 'Key 1'                 | Key with marker
    KEY 5 3 2 'Key 2' ! F             | Key with filled box
    SET FACI 3                        | Change colour for next key
    key 5 4 2 'Key 3' 2 H             | Key with hatches. DX is modified
    key 5 5 2 'Key 4' ! L             | Key with line type
    SET PLCI 4                        | Change colour for next key
    SET CSIZ .4                       | Change key size
    KEY 5 6 8 'Key 5' 1.5 W           | Key with line width
>Action PAGHPL

>Command TICKS
>Parameters
+
CHOPT 'Options' C D=' '
-   Tick marks are drawn on the edges of the picture
-X  Cross-wire drawn perpendicular to the X-axis
-Y  Cross-wire drawn perpendicular to the Y-axis
-A  Value drawn Above cross-wire
-B  Value drawn Below cross-wire
-L  Value drawn Left of cross-wire
-R  Value drawn Right of cross-wire
XVAL 'Intersection on the X-axis' R D=1.E30
YVAL 'Intersection on the Y-axis' R D=1.E30
>Keyword
cross wires
>Guidance
Draw 'cross-wires' on a picture,
optionally with tick marks and values.
Cross-wires are lines perpendicular to the X and/or Y axis.
.
The values of XVAL are always histogram coordinates.
.
The tick marks will be drawn on both side of the cross
wire, unless the cross-wires are requested on the boundary
of the box surrounding the histogram (i.e. at the extreme
limits of the drawn histogram). In this case tick marks will
only be drawn inside the box.
.
The options 'A' and 'B' (for Above and Below)
refer only to the cross-wire perpendicular to the Y axis.
In each case only one cross-wire will be drawn.
.
Similarly 'L' and 'R' (Left and Right) refer only
to the cross-wires perpendicular to the X-axis.
.
It is possible to redefine the length of tick marks on the X or Y
axis with SET XTIC or SET YTIC.
.
The position of the axis values may be changed
with SET XVAL or SET YVAL.
.
The Number of divisions can be changed with SET NDVX and SET NDVY.
.
This command combines with the command NUL is a easy way to redraw
axis on the current plot.
.
Example:
.
    SET * ; OPT *                     | Reset the defaults
    Nul 0 1 0 1                       | draw an empty frame with axis
    Set ndvy 5                        | Change number of Y divisions
    Nul 0 10 0 10 ABS                 | Redefine the scales
    Tic XR 5 !                        | Axis in the new coordinates
>Action PAGHPL

>Command ATITLE
>Parameters
+
XTIT  'X Axis title'          C D=' '
YTIT  'Y Axis title'          C D=' '
ZTIT  'Z Axis title'          C D=' '
IALGN 'Axis titles alignment' I D=0
CHOPT 'Options'               C D=' '
-  Axis title are drawn on the left and on the bottom of the plot.
-R Y axis title is drawn on the right of the plot.
-T X axis title is drawn on the top of the plot.
>Keyword
title Axis
>Guidance
Draw axis titles on the axes of the present plot zone.
The parameter IALGN defined where the title is aligned i.e:
on the beginning, the middle or at the end of the axis. The
alignment parameter has 3 digits (one for each axis): xyz
where x, y and z may have independently the following values:
.
 1: Beginning of the axis
 2: Middle of the axis
 3: End of the axis (0 is equivalent to 3)
.
Example:
.
    NUL 0 10 0 10
    NUL 0 100 0 100 S
    ATITLE 'End of axis' 'Middle of axis on the right' ! 320 R
    ATITLE 'Beginning of axis' 'End of axis' ! 130
    ATITLE 'Middle of axis on the top' 'Beginning of axis' ! 210 T

>Action PAGHPL

>Command GRID
>Guidance
Draw a grid in cm. The grid appearance is controlled with
the line and text attributes (cf command SET).
>Action PAGHPL

>Command NULL
>Parameters
+
XMIN  'Low range in X'  R D=0.
XMAX  'High range in X' R D=1.
YMIN  'Low range in Y'  R D=0.
YMAX  'High range in Y' R D=1.
CHOPT 'Options'         C D=' '
-  Draw a frame box only.
-S Redefine the scale for the current zone.
-A Axis labels and tick marks are not drawn.
-B The box is not drawn.
>Keyword
frame box clear
>Guidance
Define an empty 2D space of coordinates.
If XMIN, XMAX, etc. are given, draw a frame box
with the window coordinates set to XMIN, XMAX, YMIN, YMAX. Axis
labels and tick marks are drawn by default.
>Action PAGHPL

>Command 3DNULL
>Parameters
+
XMIN  'Low range in X'         R D=0.
XMAX  'High range in X'        R D=1.
YMIN  'Low range in Y'         R D=0.
YMAX  'High range in Y'        R D=1.
ZMIN  'Low range in Z'         R D=0.
ZMAX  'High range in Z'        R D=1.
THETA 'Angle THETA in degrees' R D=30.
PHI   'Angle PHI in degrees'   R D=30.
CHOPT 'Options'                C D='WFBA'
-  Redefine the scale for the current zone.
-B Draw the back box
-F Draw the front box
-A Draw the axis
-W Start a new window
>Guidance
Define an empty 3D space of coordinates.
If XMIN, XMAX, etc. are given, draw a frame box
with the window coordinates set to XMIN, XMAX, YMIN, YMAX. Axis
labels, tick marks and 3D box around the plot are drawn by default.
>Action PAGHPL


>Name PICDEF

>Menu PICTURE
>Keyword
HIGZ GRAPHICS
>Guidance
Creation and manipulation of HIGZ pictures.

>Command FILE
>Parameters
LUN 'Logical unit number' I R=1:128
FNAME 'File name' C
+
LRECL 'Record length in words' I D=1024
CHOPT 'Options' C D=' '
-   Existing file is opened.
-N  A new file is opened.
-U  Existing file is modified.
-A  Automatic saving.
>Keyword
HIGZ Open
>Guidance
Open a HIGZ direct access picture file.
If CHOPT='AU' or 'AN', pictures will be automatically saved
on the direct access file. This automatic saving facility
can be switched off using SET AURZ 0.
>Action PAPICT

>Command LIST
>Guidance
List all the HIGZ pictures currently stored in memory.
>Action PAPICT

>Command CREATE
>Parameters
PNAME 'Picture name' C Loop
>Guidance
Create a new picture, named PNAME, in memory.
Note that all commands which start a new picture (clear workstation)
automatically create pictures named PICT1, PICT2, etc.
if the command OPTION ZFL or OPTION ZFL1 has been executed.
>Action PAPICT

>Command DELETE
>Parameters
PNAME 'Picture name' C D=' ' Loop
>Keyword
remove rm
>Guidance
Delete the picture PNAME from memory.
PNAME='*' means all pictures.
>Action PAPICT

>Command SCRATCH
>Parameters
PNAME 'Picture name' C D=' ' Loop
+
ICYCLE 'Cycle number ' I D=9999
>Guidance
Delete the picture PNAME from current directory on disk.
>Action PAPICT

>Command PLOT
>Parameters
+
PNAME 'Picture name' C D=' ' Loop
>Keyword
draw GRAPHICS
>Guidance
Plot the picture PNAME.
PNAME=' ' means the current picture.
PNAME='*' means all pictures.
>Action PAPICT

>Command MODIFY
>Parameters
+
PNAME 'Picture name' C D=' '
CHOPT 'Options' C D=' '
-S  Software characters are used for the text in menus.
-A  The option shadow is used.
>Guidance
Edit the picture PNAME.
PNAME=' ' means the current picture.
This command is only available on workstations.
>Action PAPICT

>Command MERGE
>Parameters
PNAME 'Picture name' C
+
X  'X coordinates (NDC) where to draw PNAME' R D=0
Y  'Y coordinates (NDC) where to draw PNAME' R D=0
SCALE 'Scale factor' R D=1.
CHOPT 'Options' C D=' '
-   Merge the picture PNAME with the current picture.
-D  Picture PNAME is displayed during merging.
>Keyword
group
>Guidance
Add the picture PNAME to the current picture.
>Action PAPICT

>Command COPY
>Parameters
PNAME1 'Picture name' C
PNAME2 'New picture name' C Loop
>Guidance
Copy a picture.
>Action PAPICT

>Command RENAME
>Parameters
PNAME1 'Old picture name' C
PNAME2 'New picture name' C
>Keyword
move mv
>Guidance
Rename a picture.
>Action PAPICT

>Command PRINT
>Parameters
+
FILE 'File name' C D=' '
WIDTH 'Image width (in pixels)' I D=0
HEIGHT 'Image height (in pixels)' I D=0
>Keyword
PostScript Tex LaTex
printer plotter
>Guidance
Print the current picture.
The current picture is transformed into a printable file.
The file type is defined according to the extension of
the file name i.e.
.
 FILE = filename.ps    A PostScript file is generated (-111)
 FILE = filename.eps   A Encapsulated PostScript file
                       is generated (-113)
 FILE = filename.tex   A LaTex file is generated (-778)
 FILE = filename.gif   A gif file is generated
.
Do HELP META for details about the metafile types.
Note that a new picture is automatically created for each new
plot if the OPTION ZFL1 is on.
.
For gif files, the picture in memory is not used. The gif file is
a direct "hard-copy" of the window number 1 content. The WIDTH and 
HEIGHT parameters allow to scale the gif output file.
.
If FILE=HIGZPRINTER or FILE=' ' the PostScript file paw.ps (-111)
is generated and the operating system command defined by the environment
variable HIGZPRINTER is executed.
.
The environment variable HIGZPRINTER should be defined as follow:
.
On UNIX systems:
.
       setenv HIGZPRINTER 'lp -dprinter_name paw.ps'
.
or
       export HIGZPRINTER='lp -dprinter_name paw.ps'
.
On VAX/VMS systems:
.
       HIGZPRINTER == 'XPRINT paw.ps /PRINTER=printer_name'
.
Note that if the environment variable HIGZPRINTER is not defined
the file paw.ps is created but not printed.
>Action PAPICT

>Command LOAD
>Parameters
X     'X position' R D=0.
Y     'Y position' R D=0.
FILE  'File name'  C D=' '
+
WKID  'Workstation identifier' I D=1
>Keyword
GIF
>Guidance
Load the picture named FILE at the position X,Y in the
current normalisation transformation. X,Y is the top
left corner of the picture (like in X11). FILE should 
be a GIF file.
>Action PAPICT

>Command IZOUT
>Parameters
+
PNAME 'Picture name' C D=' ' Loop
>Keyword
Write
>Guidance
Write the picture PNAME to a direct access picture file
(see command PICTURE/FILE).
PNAME=' ' means the current picture.
PNAME='*' means all pictures.
>Action PAPICT

>Command IZIN
>Parameters
PNAME 'Picture name' C Loop
+
ICYCLE 'Cycle number ' I D=9999
>Keyword
memory Read
>Guidance
Read picture into memory from a direct access picture file.
(see command PICTURE/FILE).
PNAME='*' means all pictures.
>Action PAPICT

>Command IZPICT
>Parameters
PNAME 'Picture name' C
+
CHOPT 'Options' C D='M'
-M Make a new picture in memory with name PNAME._
   An empty structure is created in memory and becomes_
   the current picture. If PNAME = ' ', the picture is_
   automatically named as PICTnnn, where the starting_
   value of nnn is either 0 (default), or the value assigned by_
   SET to the parameter PICT.
-D Display the picture PNAME in memory.
-S Scratch the picture PNAME from memory. If PNAME = ' '_
   the current picture is scratched.
-N The picture following the current picture in memory becomes_
   the current picture. If the current picture is the_
   last one in memory, the first picture in memory becomes_
   the current picture.
-L Give the list of the pictures in memory,_
   following the sequence of their storage in memory.
-F The First picture in memory becomes the current picture.
-P Print the picture data structure. Useful to debug programs.
-C Set Current picture. All calls to HIGZ graphic functions_
   are stored in the current structure according to the option_
   selected be IGZSET.
>Guidance
Perform various operations on a picture.
PNAME=' ' means the current picture.
PNAME='*' means all pictures.
>Action PAPICT

>Command SWITCH
>Parameters
+
CHOPT 'Options' C D='G'
-G  graphics output only.
-Z  Graphics primitives stored in ZEBRA memory only.
>Keyword
ZEBRA GZ
>Guidance
Set the graphics switch to control plotting output
to terminal (G) and/or picture in memory (Z).
>Action PAPICT

>Command IGSET
>Parameters
+
CHATT 'Attribute name' C D='SHOW'
VALUE 'Attribute value' R D=0.
>Keyword
attribute
>Guidance
Set a HIGZ attribute.
If CHATT='SHOW' print default and current values for all attributes.
If CHATT='*' restore default values for all attributes.
.
Note that command SET is also calling IGSET so SET can be used
instead of IGSET.
>User_help PAHELP
>Action PAPICT

>Name OBSDEF
>Menu OBSOLETE
>Guidance
Obsolete commands

>Command MASK
>Guidance
Obsolete command use the commands:

 MASK/FILE          Open a MASK file.
 MASK/CLOSE         Close a MASK file.
 MASK/LIST          List the MASK files currently open.
 MASK/RESET         Reset on bit in a mask file.

Note that the mask files generated by this (now obsolete) command are
incompatible with the new Ntuple commands. Just generate again the
mask files once:

 MASK/FILE mask_name N
 NT/LOOP idn selection>>mask_name(i)
 NT/LOOP idn selection>>mask_name(j)
 NT/LOOP idn selection>>mask_name(k)
 etc ...
>Action PAOBS

>Menu NTUPLE
>Command MERGE
>Guidance
Obsolete command use HMERGE.
>Action PAOBS


>Menu ../GRAPHICS
>Menu ATTRIBUTES
>Command SMK
>Action PAOBS

>Command SLN
>Action PAOBS

>Command SFAIS
>Action PAOBS

>Command SFASI
>Action PAOBS

>Command SFACI
>Action PAOBS

>Command SPLCI
>Action PAOBS

>Command SPMCI
>Action PAOBS

>Command STXCI
>Action PAOBS

>Command STXFP
>Action PAOBS

>Command SCHH
>Action PAOBS

>Command SLWSC
>Action PAOBS


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
>Keyword
Open
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
>Keyword
Open
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
>Keyword
dir directory
>Guidance
Create a new RZ directory below the current directory.
>Action PAZRZ

>Command DDIR
>Parameters
CHDIR 'Directory name' C
>Keyword
dir directory
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
>Keyword
dir directory disk
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
>Keyword
dir directory
>Guidance
Change the current working directory (CWD).
IF CHPATH is given make it the new CWD.
Otherwise, print the pathname of the CWD.
.
Example:
.
  CD dir1         | make DIR1 the new CWD
  CD //file1/dir2 | make //FILE1/DIR2 the new CWD
  CD              | print the name of the CWD
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
>Keyword
statistics space
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
>Keyword
Open
>Guidance
Open an FZ sequential formatted or unformatted file.
>Action PAZFZ

>Command TOFZ
>Parameters
LUN 'Logical unit number of FZ file' I R=1:128
+
CHOPT 'Options' C D=' '
>Keyword
Copy 
>Guidance
Copy the current directory tree onto an FZ file.
>Action PAZFZ

>Command FRFZ
>Parameters
LUN 'Logical unit number of FZ file' I R=1:128
+
CHOPT 'Options' C D=' '
>Keyword
Copy 
>Guidance
Copy the FZ file into the current directory tree.
>Action PAZFZ

>Command TOALPHA
>Parameters
FNAME 'Name of the FZ text file' C
>Keyword
Copy 
>Guidance
Copy the current directory tree onto a FZ file.
An alphanumeric format is used.
The file FNAME can be exchanged between different machines.
>Action PAZFZ

>Command FRALPHA
>Parameters
FNAME 'Name of the FZ text file' C
>Keyword
Copy 
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
>Keyword
zebra bank structure
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
>Keyword
structure
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
>Keyword
structure
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
Output the parameters characterising the store, followed by a
list of all divisions and all link areas associated with the store in
question.
>Action PAZDZ

>Name FORDEF

>Menu FORTRAN
>Guidance
Interface to MINUIT, COMIS, SIGMA and FORTRAN Input/Output.

>Command HMINUIT
>Keyword
MINUIT
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
.
Normal PAW commands can be executed from MINUIT if they
are prefixed with a "+" sign.
.
The Minuit command MNC is extented with a 3rd argument.
If the 3rd argument is "C" a graphical contour is drawn
and the contour points are saved in vectors XFIT,YFIT.
>Action PAFORT

>Command COMIS
>Keyword
interpreter
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
>Keyword
Execute
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

>Command UNITS
>Parameters
>Guidance
List all Input/Output logical units currently open.  The files attached to 
them are also shown.  
>Action PAFORT

>Command LOOP
>Parameters
NTIMES 'Number of calls' I D=1
UROUT  'User routine' C
>Keyword
call
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
>Keyword
Open
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
.
1- Using the KUIP $SIGMA function. Example:
.
  PAW > Vector/Create x(10) r 1 2 3 4 5 6 7 8 9 10
  PAW > Graph 10 x $sigma(sqrt(x))
.
2- Using the SIGMA command. Example:
.
  PAW > sigma x=array(10,1#10)
  PAW > sigma y=sqrt(x)
  PAW > Graph 10 x y
.
3- Using the APPLication command. Example:
.
  PAW > APPLication SIGMA
  SIGMA > x=array(10,1#10)
  SIGMA > y=sqrt(x)
  SIGMA > exit
  PAW > Graph 10 x y
>Action PASIGM

>Name NETDEF

>Menu NETWORK
>Guidance
To access files on remote computers.
To send messages to a remote process (ZEBRA server)

>Command RLOGIN
>Parameters
HOST 'Host name' C D=' '
>Keyword
communication remote HOST
>Guidance
Start a communication with a remote machine HOST.
Current Directory will be changed to //HOST.
>Action RLOGIN

>Command RSHELL
>Parameters
MESSAGE 'Message to remote host' C D=' '
>Keyword
SHELL communication remote HOST
>Guidance
Send MESSAGE to current remote host.
Note that the Current Directory must be //HOST (see RLOGIN).
Some PAW commands (Histo/Plot, Histo/List) can communicate
directly with HOST.
>Action RSHELL

>Menu PIAF
>Keyword
server
>Guidance
To establish and control the connection to the Piaf server.
The Parallel Interactive Analysis Facility (Piaf) is a cluster of
5 high-performance HP workstations.
.
A locally running PAW session (client) connected to the Piaf server
can access Hbook RZ files stored on the server side in a transparent way.
Commands with high CPU or I/O requirements, e.g. NT/PLOT and NT/PROJECT
are processed by the server and only the resulting histograms etc. are
sent back to the client.
.
In order to use the Piaf server the PAW client must have been compiled
with the communications option CZ using TCP/IP as transport protocol.

>Command CONNECT
>Parameters
SERVER 'Server name'    C D='piaf'
NODE   'Front-end node' C 
>Keyword
connection
>Guidance
Establish a connection to the Piaf server.
Subsequent HISTO/FILE commands can refer to files on the server
using path names '//piaf/file.hbook'.
.
Example:
 
      PAW > CONNECT piaf cerncs2b       | Ethernet node
      PAW > CONNECT piaf f-cerncs2-f    | FDDI node


>Action RLOGIN

>Command STAGE
>Parameters
SOURCE 'Source file identifier' C
+
TARGET 'Target file name' C D=' '
OPTION 'Options' C D=' '
-N  NoWait. Submit the request to the staging system and return immediately.

>Guidance
Stage an Ntuple file on the Piaf server.
The source file identifier can be the name of a local file on the client
system, a Fatmen path, or a tape identifier.
If the target file name is not specified it is constructed from the
source identifier.
.
Unless the option N is used the STAGE command waits until the staging
is completed and the file is ready to be used.
>Action PAPIAF

>Command GET
>Parameters
REMOTE 'Remote file name' C
+
LOCAL  'Local file name' C D=' '
FORMAT 'Text or binary' C D=RZ  Option
-T      Text file.
-RZ     Zebra RZ file in exchange format.
-BIN    Binary file with record length given by RECL.
RECL   'Record length in bytes' I D=0 R=0:

>Keyword
Copy
>Guidance
Copy a file from the Piaf server to the client system.
If not specified the local file name will be same as the remote file name.

RECL needs to be specified only for BIN format.

For IBM only: A text file with RECL=0 is written in V-format.
Otherwise it is written in F-format with the given LRECL.

>Action PAPIAF


>Command PUT
>Parameters
LOCAL  'Local file name' C
+
REMOTE 'Remote file name' C D=' '
FORMAT 'Text or binary' C D=RZ  Option
-T      Text file.
-RZ     Zebra RZ file in exchange format.
-BIN    Binary file.

>Keyword
Copy
>Guidance
Copy a file from the client system to the Piaf server.
If not specified the remote file name will be same as the local file name.

Note for VMS: Avoid text files with variable record length.
Use Stream_LF format instead.

>Action PAPIAF

>Command LS
>Parameters
+
FILES 'File pattern' C D=' '
>Keyword
List
>Guidance
List files stored on the Piaf server.
>Action PAPIAF

>Command CAT
>Parameters
FILE 'File name' C
>Keyword
write
>Guidance
Print a Piaf file on the terminal.
>Action PAPIAF

>Command RM
>Parameters
FILE 'File name' C
>Keyword
Delete remove
>Guidance
Delete a Piaf file.
>Action PAPIAF

>Command MV
>Parameters
FROM 'Old file name' C
TO   'New file name' C
>Keyword
Rename move
>Guidance
Rename a Piaf file.
>Action PAPIAF

>Command CP
>Parameters
FROM 'Old file name' C
TO   'New file name' C
>Keyword
Copy
>Guidance
Copy a Piaf file to a new file.
>Action PAPIAF

>Command PWD
>Keyword
dir directory
>Guidance
Show current Piaf working directory.
>Action PAPIAF

>Command MKDIR
>Parameters
DIR 'Directory name' C
>Keyword
dir directory
>Guidance
Create a new directory on Piaf.
>Action PAPIAF

>Command RMDIR
>Parameters
DIR 'Directory name' C
>Keyword
dir directory
>Guidance
Delete a directory on Piaf.
>Action PAPIAF

>Command MESSAGE
>Parameters
MESS 'Message' C
>Keyword
Send
>Guidance
Send a message to Piaf.
>Action PAPIAF

>Command STATUS
>Guidance
Inquire the status of the Piaf server.
>Action PAPIAF

>Command MODE
>Parameters
+
OPTION 'Processing mode' C D=?
-?    Inquire the current mode.
-SEQ  Set sequential processing mode.
-PAR  Set parallel processing mode.
>Guidance
Inquire or change the processing mode of the Piaf server.
In parallel mode the Piaf server uses slave servers to process
Ntuple requests on all available machines in parallel.
.
With certain types of COMIS selection functions,
e.g. when reading from an external file for each event,
parallel processing is not possible.
The Piaf server should be switched to sequential mode,
i.e. the master server alone processes the Ntuple request.
>Action PAPIAF

>Command LOGLEVEL
>Parameters
LEVEL 'Log level' I D=0
>Keyword
level diagnostic
>Guidance
Set the level of diagnostic output from the Piaf server.
>Action PAPIAF

>Command DISCONNECT
>Keyword
connect connection
>Guidance
Close the connection to the Piaf server.
>Action PAPIAF

>Name PVEDEF

>Menu VECTOR

>Command DRAW
>Parameters
VNAME 'Vector name' C
+
ID 'Histogram Identifier' C D='12345'
CHOPT 'Options' C D=' '
-  Draw an histogram.
-C Draw a smooth curve.
-S Superimpose plot on top of existing picture.
-+ Add contents of ID to last plotted histogram.
-B Select Bar chart format.
-L Connect channels contents by a line.
-P Draw the current polymarker at each channel.
-* Draw a * at each channel.
>Keyword
GRAPHICS
>Guidance
Draw vector VNAME (real) interpreting it as a histogram.
Optionally save the contents in histogram ID.
>Action PAVECT

>Command HFILL
>Parameters
VNAME 'Vector name' C
ID 'Histogram Identifier' C
>Keyword
Fill
>Guidance
Fill the existing histogram ID with vector VNAME (real) .
Note that the command VECTOR/PLOT can automatically book, fill and plot
the contents of a vector.
>Action PAVECT

>Command PLOT
>Parameters
VNAME 'Vector name' C
+
ID 'Histogram Identifier' C D='12345'
CHOPT 'Options' C D=' '
-  Draw an histogram.
-C Draw a smooth curve.
-S Superimpose plot on top of existing picture.
-+ Add contents of ID to last plotted histogram.
-B Select Bar chart format.
-L Connect channels contents by a line.
-P Draw the current polymarker at each channel.
-* Draw a * at each channel.
>Keyword
draw GRAPHICS
>Guidance
Each element of VNAME (real)  is used to fill an histogram
which is automatically booked with 100 channels and then plotted.
If VNAME has the form VNAME1%VNAME2 then a scatter-plot of vector VNAME1
versus VNAME2 is plotted. If ID is given different of 12345, then
a 2-Dim histogram is created with 40 bins by 40 bins and filled.
One can use the command VECTOR/HFILL to fill an already existing
histogram.
When option 'S' is used, the limits of the current plot are used to create 
the 1D histogram.
>Action PAVECT

>Command FIT
>Parameters
X 'Vector of X coordinates' C
Y 'Vector of Y coordinates' C
EY 'Vector of errors on Y' C D='?'
FUNC 'Function name' C
+
CHOPT 'Character options' C D=' '
-   Do the fit, plot the result and print the parameters.
-0  Do not plot the result of the fit. By default the fitted _
    function is drawn unless the option 'N' below is specified.
-S  Superimpose plot on top of existing picture.
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
    of the contents or from HPAKE/HBARX (PUT/ERRORS).
-M  The interactive Minuit is invoked.
-E  Performs a better Error evaluation (MIGRAD + HESSE + MINOS).
-Z  FUNC is the user fitting model
-NN Neural Network fitting.
NP 'Number of parameters' I D=0 R=0:20
PAR 'Vector of parameters' C
STEP 'Vector of steps size' C
PMIN 'Vector of lower bounds' C
PMAX 'Vector of upper bounds' C
ERRPAR 'Vector of errors on parameters' C
>Keyword
HBARX HPAKE
>Guidance
Fit a user defined function to the points defined by the two vectors X and Y 
and the vector of associated errors EY.
See command HISTOGRAM/FIT for explanation of parameters.
Note that if option 'W' is specified or EY='?' (default),
the array EY is ignored. Option 'L' is not available.
.
When option 'Z' is given, FUNC is the user fitting model.
FUNC is a subroutine with the calling sequence:
      SUBROUTINE FUNC(N,X,Y,EY,NPAR,IFLAG,NPFITS)
where
      - X(N),Y(N),EY(N) are the input vectors,
      - NPAR the number of parameters
      - NPFITS is an output parameter = Number of points used in the fit
The user must declare the 
      COMMON /HCFITD/FITPAD(24),FITFUN 
in FUNC
.
Some plotting options available in the command HISTOGRAM/PLOT can be also used.
.
When 'NN' is given as the two first letters of the fitting model,
a Multi-Layer Perceptron like function is used to do the fit. The
syntax of the command becomes:
.
 VEC/FIT X Y EY NNi[,j] [CHOPT NEPOCH] 
.
Where: i[,j] number of neurons in the single hidden layer (if only i given) 
or in the two hidden layer if i,j are given. NEPOCH is the number of training 
epochs (default = 100).
If the option '+' is given, the network continues to minimise starting from the
previous weights. If the option 'I' is given, different random weights are used.
Thus, repeated 
.
vec/fit x y ey NN4 ! 400 
.
commands all give the same result, whereas repeated
.
vec/fit x y ey NN4 I 400.
.
commands all lead to a different result because of different initial weights.
>Action PAFITV

>Menu /KUIP
>Command BUGREPORT
>Parameters
+
CHOPT 'Options' C D='B'
-B      Send a bug report
-C      Send a comment, suggestion, etc.
>Keyword
report comment suggestion
>Guidance
Email a bug report or comment to the PAW team.
The local editor is invoked with a template to be
filled out.
After the template has been edited, version information
about PAW and the operating system is appended.
The user is asked for a confirmation before the report is send.
.
In Paw++ this command can be accessed via the 'Help' menu of the
'Executive Window' or the 'Main Browser' (menu item 'Mail Paw++ Developers').
.
This command is implemented only on UNIX, VMS and VM systems.
.
If the environment variable PAWSUPPORT is defined it is used as
email address.
>Action bugreprt

>Command VERSION
>Guidance
Print the version string for PAW and the underlying packages.
>Keyword
PAW HBOOK HPLOT HIGZ SIGMA
>Action pavercmd
