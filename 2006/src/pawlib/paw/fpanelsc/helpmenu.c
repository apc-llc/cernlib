/*
 * $Id: helpmenu.c,v 1.2 1996/10/17 14:38:37 couet Exp $
 *
 * $Log: helpmenu.c,v $
 * Revision 1.2  1996/10/17 14:38:37  couet
 * - Wrong format in the help
 *
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 19/04/95  14.35.37  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for Help menu and the help text.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <X11/cursorfont.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef UxParent
#undef mainctxt
#include "fpanelsh/fileselpanel.h"
#undef UxParent
#undef mainctxt
#include "fpanelsh/contpanel.h"
#undef UxParent
#undef mainctxt
#include "fpanelsh/scanpanel.h"
#undef UxParent
#undef mainctxt
#include "fpanelsh/vectpanel.h"
#undef UxParent
#undef mainctxt
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif

/******************************************************************************/
static char minuit_help_text[] = "\
+--------+---------------------------------------------------------+------+\n\
| MINUIT |                 CERN Program Library                    | D506 |\n\
+--------+---------------------------------------------------------+------+\n\
Author(s): F. James, M. Roos                              Library: MATHLIB\n\
Submitter: F. James                                  Submitted: 03.05.1967\n\
Language:  Fortran                                     Revised: 01.03.1989\n\
\n\
                   Function Minimization and Error Analysis\n\
                   -----------------------------------------\n\
\n\
The MINUIT package performs minimization and analysis of the shape of a\n\
multi-parameter function. It is intended to be used on Chisquare or\n\
likelihood functions for fitting data and finding parameter errors and\n\
correlations. The more important options offered are:\n\
\n\
\n\
*  Variable metric (Fletcher) minimization\n\
*  Monte Carlo minimization\n\
*  Simplex (Nelder and Mead) minimization\n\
*  Parabolic error analysis (error matrix)\n\
*  MINOS (non-linear) error analysis\n\
*  Contour plotting\n\
*  Fixing and restoring parameters\n\
*  Global minimization\n\
\n\
See Long Write-up and supplements Function Minimization and Interpretation\n\
of Errors for details.\n"
;


/***********************************************************************
***********************************************************************/
static char panel_help_text[] = "\
                           Fit Panel\n\
                           ---------\n\
\n\
The Histogram Fit and Vector Fit Panels allow interactive access to\n\
practically all the features of Minuit. A panel consist of the\n\
following areas listed from top to bottom.\n\
    o Pull-down menu bar\n\
    o Fit object (histogram or vector pair) area\n\
    o Fit function and fit model area\n\
    o Minimization status area\n\
    o Fit function parameter area\n\
    o Minuit settings area\n\
\n\
Several subpanels can be opened through pull-down menus, namely.\n\
    o File selection subpanel\n\
    o Contour subpanel\n\
    o Scan subpanel\n\
    o Transfer subpanel\n\
\n\
In addition to standard Motif menus, buttons, and sliders, the panel\n\
and its subpanels have numerous text fields. Some of these fields are\n\
used for display only and can not be edited by the user. Text fields\n\
which can be edited by the user behave as following. A text field is\n\
selected for input when the pointer is placed inside it. No action is\n\
taken when the user edits the field. Text input field is activated\n\
when the pointer leaves the field or when the \"Enter\" key have been\n\
pressed. The content of the field is first checked for correct syntax\n\
and the appropriate action is taken only if the field contains a valid\n\
value. Otherwise, an error message is popped-up and the field is\n\
marked as invalid. To indicate an invalid value in the input text\n\
field or a value which is not current in an output text field (such as\n\
an error estimate) red boundary is put around the field.\n\
\n\
For more information about different parts of the panel use context\n\
help.\n"
;

/**********************************************************************/
static char error_help_text[] = "\
Context help error: no valid\n\
widget is pointed at.\n"
;


/***********************************************************************
             Pull-down menus
***********************************************************************/

/**********************************************************************/
static char filemenu_help_text[] = "\
              File menu\n\
              ---------\n\
\n\
The File menu performs the following functions\n\
of generic nature.\n\
\n\
Save... - open the File Selection Panel to select\n\
        a file to save the fit function parameters.\n\
        This is done via the Minuit \"save\" command.\n\
\n\
Input... - open the File Selection Panel to select\n\
        a file to restore previously saved fit\n\
        function parameters and/or to read Minuit\n\
        commands.\n\
\n\
Store all - store internally the current fit\n\
        function parameter values.\n\
\n\
Recall stored - set the fit function parameter\n\
        values to the values previously stored\n\
        with the \"Store all\" menu item.\n\
\n\
Recall starting - reset the fit function parameter\n\
        values to their values at the panel start.\n\
\n\
Exit - exit the panel.\n"
;

/**********************************************************************/
static char minimenu_help_text[] = "\
                 Minimize menu\n\
                 -------------\n\
\n\
Through the Minimize menu various minimizing functions\n\
of Minuit can be executed.\n\
\n\
Migrad - The most efficient and complete method\n\
        recommended for general functions.\n\
\n\
Minimize - Causes minimization of the function by the\n\
        method of Migrad, but switches to the Simplex\n\
        method if Migrad fails to converge.\n\
\n\
Seek - Causes a Monte-Carlo minimization of the\n\
        function, by choosing random values of the\n\
        variable parameters, chosen uniformly over a\n\
        hypercube centered at the current best value.\n\
        The hypercube relative size depends on the\n\
        value of \"Devs\" in the settings area of the\n\
        panel.\n\
\n\
Simplex - Performs a function minimization using the\n\
        simplex method of Nelder and Mead.\n\
\n\
Improve - If a previous minimization has converged,\n\
        and the current values of the parameters\n\
        therefore correspond to a local minimum of the\n\
        function, this command requests a search for\n\
        additional distinct local minima.\n"
;

/**********************************************************************/
static char examenu_help_text[] = "\
                  Examine menu\n\
                  ------------\n\
\n\
The minimums found by Minuit can be examined via the\n\
Examine menu.\n\
\n\
Contour... - Open the Contour subpanel to plot the\n\
        contour lines of the fit model with respect to\n\
        two parameters chosen on the subpanel.\n\
\n\
Hesse - Instructs Minuit to calculate the Hessian or\n\
        error matrix. The resulting matrix is printed\n\
        in the output area.\n\
\n\
Minos - Causes Minos error analysis to be performed\n\
        for all variable parameters. The errors are\n\
        displayed in the Eminus and Eplus fields of\n\
        the parameter lines, if these fields are on\n\
        (see View menu help).\n\
\n\
Correlations - Calculates the parameter correlations\n\
        from the error matrix and prints them in the\n\
        output area.\n\
\n\
Covariance - Prints the covariance matrix in the output\n\
        area.\n\
\n\
Eigenvalues - Calculates the eigenvalues and prints\n\
        them in the output area.\n"
;

/**********************************************************************/
static char scanmenu_help_text[] = "\
                 Scan menu\n\
                 ---------\n\
\n\
The Scan menu functions visualize the behavior\n\
of the fit model around the current point in\n\
the parameter space\n\
\n\
1-D scan... - Open the Scan subpanel to plot the\n\
        graph of the fit model with respect to\n\
        a parameter chosen on the subpanel.\n\
\n\
2-D scan... - Open the Scan subpanel to plot the\n\
        colorfill representation of the fit model\n\
        with respect to two parameters chosen on\n\
        the subpanel (not yet implemented).\n"
;

/**********************************************************************/
static char transmenu_help_text[] = "\
             Transfer menu\n\
             -------------\n\
\n\
The Transfer menu serves to exchange data\n\
between the Fit Panel and KUIP interpreter.\n\
\n\
PAW vectors... - Open Transfer subpanel to\n\
        copy Fit Panel vectors into KUIP\n\
        vectors specified on the subpanel.\n\
        If a KUIP vector with the specified\n\
        name does not exist it is created\n"
;

/**********************************************************************/
static char viewmenu_help_text[] = "\
                 View menu\n\
                 ---------\n\
\n\
The View menu consists of toggle buttons to control\n\
the appearance of the parameter lines. Corresponding\n\
text fields appear in and disappear from the\n\
parameter lines when a button is toggled.\n\
\n\
Limits - Input text fields for lower and upper\n\
        limits of parameters.\n\
\n\
Steps - Input text fields for parameter steps.\n\
\n\
Eparab - Display text fields for the parabolic\n\
        errors.\n\
\n\
Eminos - Display text fields for the Minos errors.\n\
\n\
Globcc - Display text fields for the global\n\
        correlation coefficients.\n"
;


/***********************************************************************
             Fit object line
***********************************************************************/

/**********************************************************************/
static char histId_help_text[] = "\
Histogram identifier input field\n\
--------------------------------\n\
\n\
The identifier of the current\n\
histogram is specified in this\n\
field. The number of bins of the\n\
current histogram is displayed\n\
in parentheses next to the input\n\
field.\n"
;

/**********************************************************************/
static char hrange_help_text[] = "\
Histogram range input field\n\
---------------------------\n\
\n\
The histogram range is specified\n\
in this field. Integer and\n\
floating point ranges are\n\
supported, like in PAW commands.\n\
An empty range limit is assumed\n\
to be 1 for the lower limit and\n\
the number of bins for the upper\n\
limit.\n"
;

/**********************************************************************/
static char xvect_help_text[] = "\
 X name input field\n\
 ------------------\n\
\n\
X is an array defining x\n\
coordinates of the\n\
points to be fitted.\n"
;

/**********************************************************************/
static char yvect_help_text[] = "\
 Y name input field\n\
 ------------------\n\
\n\
Y is an array defining y\n\
coordinates of the\n\
points to be fitted.\n"
;

/**********************************************************************/
static char eyvect_help_text[] = "\
EY name input field\n\
-------------------\n\
\n\
EY is an array defining\n\
associated errors (weights)\n\
in Y for points to be fitted.\n\
If EY field is empty all the\n\
weights are taken to be equal\n\
to 1.\n"
;

/**********************************************************************/
static char optn_help_text[] = "\
        Fit options input field\n\
        -----------------------\n\
\n\
Fit options are specified in this field.\n\
Options D, W, and U are supported for the\n\
Histogram Fit Panel, options D and U only for\n\
the Vector Fit Panel (in the latter case the\n\
weights are explicitly specified by the vector\n\
EY).\n\
\n\
D - The user is assumed to compute\n\
    derivatives analytically using the routine\n\
    HDERIV. By default, derivatives are computed\n\
    numerically.\n\
\n\
W - Set weights equal to 1. Default weights\n\
    are taken from the square root of the\n\
    contents or from HPAKE/HBARX (PUT/ERRORS).\n\
    If the L option is given (Log Likelihood),\n\
    bins with errors=0 are excluded from the fit.\n\
\n\
U - User function value is taken from\n\
    /HCFITD/FITPAD(24),FITFUN\n"
;


/***********************************************************************
             Fit function and fit model line
***********************************************************************/

/**********************************************************************/
static char ffunc_help_text[] = "\
          Fit function input field\n\
          ------------------------\n\
\n\
A fit function can be defined as one of the following.\n\
\n\
A- The name of a file which contains the user defined\n\
   function to be minimized. Function name and file name\n\
   must be the same. For example file FUNC.FOR is:\n\
       FUNCTION FUNC(X)\n\
       COMMON/PAWPAR/PAR(2)\n\
       FUNC=PAR(1)*X +PAR(2)*EXP(-X)\n\
       END\n\
\n\
   When the option U is given, the file FUNC.FOR should\n\
   look like:\n\
       FUNCTION FUNC(X)\n\
       DOUBLE PRECISION FITPAD(24),FITFUN\n\
       COMMON/HCFITD/FITPAD,FITFUN\n\
       FITFUN=FITPAD(1)*X+FITPAD(2)*EXP(-X)\n\
       FUNC=FITFUN\n\
       END\n\
\n\
B- One of the following keywords (1-Dim only):\n\
   G : Func=par(1)*exp(-0.5*((x-par(2))/par(3))**2)\n\
   E : Func=exp(par(1)+par(2)*x)\n\
   Pn: Func=par(1)+par(2)*x+...  +par(n+1)*x**n\n\
\n\
C- A combination of keywords in B with operators + or *.\n\
   Examples: p4+g, p2*g+g\n\
   Note that in this case, the order of parameters\n\
   corresponds to the order of the basic functions.\n\
   For example, in the first case above, par(1:5) apply\n\
   to the polynomial of degree 4 and par(6:8) to the\n\
   gaussian while in the second case par(1:3) apply to the\n\
   polynomial of degree 2, par(4:6) to the first gaussian\n\
   and par(7:9) to the second gaussian. Blanks are not\n\
   allowed in the expression.\n"
;

/**********************************************************************/
static char npar_help_text[] = "\
Number of parameters input field\n\
--------------------------------\n\
\n\
Input field for the number of fit\n\
function parameters. If the fit\n\
function is one of the predefined\n\
functions, or an expression composed\n\
of predefined functions, the number\n\
of parameters is determined\n\
automatically and can not be\n\
modified by the user. If, on\n\
the other hand, a user fit function\n\
is specified, it is a user\n\
responsibility to specify the\n\
correct number of parameters.\n"
;

/**********************************************************************/
static char fmodel_help_text[] = "\
     Fit model input field\n\
     ---------------------\n\
\n\
The name of the fit model is\n\
specified in this field. The fit\n\
model defines how the distance\n\
between the fit object (histogram\n\
or vectors) is computed. Currently,\n\
only chisquare (chis2) and  Log \n\
Likelihood (logL) fit models are \n\
supported for histograms, and chisquare \n\
only for vectors.\n"
;


/***********************************************************************
             Fit status line
***********************************************************************/

/**********************************************************************/
static char fval_help_text[] = "\
   Fval display field\n\
   ------------------\n\
\n\
Fval is the current value\n\
of the fit model.\n"
;

/**********************************************************************/
static char fmin_help_text[] = "\
   Fmin display field\n\
   ------------------\n\
\n\
Fmin is the best value\n\
of the fit model found\n\
so far by Minuit\n"
;

/**********************************************************************/
static char fedm_help_text[] = "\
   Fedm display field\n\
   ------------------\n\
\n\
Fedm is the estimated\n\
distance between Fmin\n\
and the true minimum\n"
;


/***********************************************************************
             Minuit settings line
***********************************************************************/

/**********************************************************************/
static char strategy_help_text[] = "\
        Strategy option menu\n\
        --------------------\n\
\n\
Strategy option menu sets the strategy\n\
to be used in calculating first and\n\
second derivatives and in certain\n\
minimization methods. In general,\n\
lower values of the strategy mean\n\
fewer function calls and high values\n\
mean more reliable minimization.\n\
Currently allowed values are 0, 1,\n\
and 2.\n"
;

/**********************************************************************/
static char printout_help_text[] = "\
        Printout option menu\n\
        --------------------\n\
\n\
Printout option menu sets the print\n\
level, determining how much output\n\
Minuit will print in the output area.\n\
The values are as following.\n\
\n\
-1 - No output, except when explicitly\n\
    requested (see Examine menu help).\n\
\n\
0 - Minimum output (no starting values\n\
    or intermediate results).\n\
\n\
1 - Default value, normal output.\n\
\n\
2 - Additional output giving\n\
    intermediate results.\n\
\n\
2 - Maximum output, showing progress\n\
    of minimization.\n"
;

/**********************************************************************/
static char update_help_text[] = "\
            Update mode menu\n\
            ----------------\n\
\n\
Update mode menu defines how the graphical\n\
window is updated when fit function parameters\n\
are modified. Graphical window updating\n\
involves heavy overhead and should be set to\n\
Delayed if the performance is not\n\
satisfactory.\n\
\n\
Immediate - Update the graphics every time a\n\
        parameter is modified by scale\n\
        dragging, text input, menu functions.\n\
\n\
Delayed - Update the graphics every time a\n\
        parameter is modified, except during\n\
        the scale dragging. The graphics is\n\
        updated only when the mouse button\n\
        used for scale dragging is released.\n\
\n\
Disabled - Graphics update is disabled.\n"
;

/**********************************************************************/
static char errordef_help_text[] = "\
       Errordef input field\n\
       --------------------\n\
\n\
Value defining the scaling of parameter\n\
errors (sensitivity) is specified in this\n\
field. The Errordef value allows the user to\n\
define what he means by \"one error\" in terms\n\
of change in the fit model value which should\n\
be caused by changing one parameter by one\n\
\"error\". If the fit model is the usual\n\
chisquare function, then the Errordef value\n\
should be set to 1.0 if the user wants the\n\
usual one-standard-deviation errors. If the\n\
fit model is a negative-log-likelihood\n\
function, then the one-standard-deviation\n\
value for Errordef is 0.5. The Errordef value\n\
is used to calculate all the parameter errors,\n\
including the fit model contours.\n"
;

/**********************************************************************/
static char tolerance_help_text[] = "\
       Tolerance input field\n\
       ---------------------\n\
\n\
Tolerance value specifies the required\n\
tolerance on the fit model value at the\n\
minimum. The minimization will stop\n\
when the estimated vertical distance to\n\
the minimum (EDM) is less than\n\
0.001*Tolerance*Errordef.\n"
;

/**********************************************************************/
static char devs_help_text[] = "\
          Devs input field\n\
          ----------------\n\
\n\
Devs value specifies the relative size\n\
of the search hypercube for\n\
Monte-Carlo minimization by the\n\
Minuit method \"Seek\". The hypercube\n\
has sides parallel to the coordinates.\n\
Its extension in each dimension is\n\
equal to the current estimate of the\n\
parabolic parameter errors multiplied\n\
by the value of \"Devs\".\n"
;

/***********************************************************************
             Parameter lines
***********************************************************************/

/**********************************************************************/
static char parzone_help_text[] = "\
          Parameter scroll window\n\
          -----------------------\n\
\n\
The parameter scroll window contains the header\n\
line and a parameter line for every parameter\n\
of the fit function. A parameter line consist\n\
of widgets which identify the parameter, allow\n\
to change the parameter value, limits, and\n\
steps, and several optional text display fields\n\
for estimates associated with the parameter.\n\
The presence of the optional text display\n\
fields in the parameter lines is controlled by\n\
the \"View\" pull-down menu.\n"
;

/**********************************************************************/
static char parnum_help_text[] = "\
    Parameter number label\n\
    ----------------------\n\
\n\
Parameter number label displays\n\
the consecutive number of the\n\
corresponding parameter. This\n\
number can not be changed by\n\
the user\n"
;

/**********************************************************************/
static char parname_help_text[] = "\
    Parameter name input field\n\
    --------------------------\n\
\n\
Parameter name text field displays\n\
the parameter name. Parameter name\n\
can be changed by editing the field.\n\
Valid name is a sequence of letters\n\
and digits starting from a letter.\n"
;

/**********************************************************************/
static char parscale_help_text[] = "\
              Parameter scale\n\
              ---------------\n\
\n\
The scale is used to change the parameter value.\n\
The change is reflected immediately in the parameter\n\
value text field. If the parameter value is outside\n\
of the scale interval, or the scale interval is\n\
invalid, the scale is disabled. The scale interval\n\
can be adjusted using the adjusting device which\n\
opens/closes when the adjusting button located to\n\
the right of the scale is activated.\n\
\n\
The scale can be used by dragging the slider with\n\
the left button pressed, or by clicking the left or\n\
the middle button outside of the slider. When the\n\
left button is clicked outside of the slider, it\n\
moves to the direction of the pointer by one tenth\n\
of the scale length. When the center button is\n\
pressed, the slider moves to the location of the\n\
pointer. Whether the graphics window is updated\n\
immediately when the slider is dragged, or only when\n\
the button is released, is defined by an update mode\n\
set by the \"Update\" option menu in the settings\n\
area.\n"
;

/**********************************************************************/
static char paradjbut_help_text[] = "\
       Adjusting button\n\
       ----------------\n\
\n\
Adjusting button opens/closes\n\
the scale interval adjusting\n\
device.\n"
;


/**********************************************************************/
static char parvalue_help_text[] = "\
          Parameter value input field\n\
          ---------------------------\n\
\n\
Parameter value input text field displays the current\n\
parameter value. The value can be changed by editing\n\
the field. If the new value is outside of the scale\n\
interval, the scale is disabled.\n\
\n\
The parameter value pop-up menu appears when the\n\
pointer is anywhere on the parameter line and the right\n\
mouse button is pressed. For the value of an individual\n\
parameter, this menu performs functions similar to\n\
functions that the \"File\" menu performs for all the\n\
parameter values. The menu also has display fields for\n\
the current, stored, and starting values.\n\
\n\
Recall current - if a syntactic error was found in the\n\
        parameter value input text field, write the\n\
        current parameter value back into it.\n\
\n\
Store - store internally the current fit function\n\
        parameter value.\n\
\n\
Recall stored - set the fit function parameter value\n\
        to the value previously stored with the\n\
        \"Store\" menu item.\n\
\n\
Recall starting - reset the parameter value to its\n\
        value at the panel start.\n"
;

/**********************************************************************/
static char parfixtoggle_help_text[] = "\
        Fix/Release toggle\n\
        ------------------\n\
\n\
Fix/Release toggle removes/returns\n\
the parameter from/to the Minuit set\n\
of variable parameters. The parameter\n\
is fixed in the sense that Minuit\n\
will not attempt to change its value.\n\
The user, however, can change the\n\
value of a fixed parameter as usual.\n"
;

/**********************************************************************/
static char parlimits_help_text[] = "\
  Parameter limit input fields\n\
  ----------------------------\n\
\n\
The lower and upper limits of the\n\
parameter are specified in these\n\
fields. If both fields are made\n\
empty or zero, the limits are\n\
removed and vice versa.\n"
;

/**********************************************************************/
static char parstep_help_text[] = "\
Parameter step input field\n\
--------------------------\n\
\n\
The starting step size of the\n\
parameter is specified in this\n\
field.\n"
;

/**********************************************************************/
static char pareparab_help_text[] = "\
  Eparab display field\n\
  --------------------\n\
\n\
Display text field for the\n\
parabolic error which is\n\
computed by the Minuit\n\
minimization procedures.\n"
;

/**********************************************************************/
static char parminos_help_text[] = "\
Minos error display fields\n\
--------------------------\n\
\n\
Display text fields for the\n\
Minos errors which are\n\
computed by the \"Minos\"\n\
function of the \"Examine\"\n\
pull-down menu.\n"
;

/**********************************************************************/
static char parglobcc_help_text[] = "\
    Globcc display field\n\
    --------------------\n\
\n\
Display text field for the global\n\
correlation coefficient which is\n\
computed by Minuit procedures\n\
\"Migrad\", \"Hesse\", and\n\
\"Minos\". This is a number\n\
between zero and one which gives\n\
the correlation between the\n\
parameter and that linear\n\
combination of all other\n\
parameters which is most strongly\n\
correlated with the parameter in\n\
question.\n"
;

/**********************************************************************/
static char paradjdev_help_text[] = "\
     Scale interval adjusting device\n\
     -------------------------------\n\
\n\
The scale interval adjusting device consists of\n\
text input fields for scale minimum and maximum\n\
and a pair of arrow buttons for each text field.\n\
The scale interval can be changed either by\n\
editing the text fields or using the arrow\n\
buttons.\n\
\n\
Activating an arrow button pointing left\n\
decreases the corresponding value. Activating an\n\
arrow button pointing right increases the\n\
corresponding value. The modified value is\n\
chosen so that it is as round as possible, the\n\
scale interval relative length change is from\n\
1/10 to 1/3, and, if the parameter is bounded,\n\
it remains inside the limits. If the mouse\n\
button which activated an arrow button remains\n\
pressed, the arrow button is activated\n\
repeatedly several times per second till the\n\
button is released.\n\
\n\
If, as a result of the user input, the current\n\
parameter value fells outside of the scale\n\
interval, the scale is disabled and vice versa.\n"
;

/***********************************************************************
             Popup subpanels.
 Note: The help for these subpanels can be also called through "Help"
     buttons on the subpanels, therefore the help text strings are
     made "extern" instead of "static".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
***********************************************************************/

/***********************************************************************
             File Selection Panel
***********************************************************************/

/**********************************************************************/
char fp_filesel_help_text[] = "\
     File selection panel\n\
     --------------------\n\
\n\
File selection panel allows the user\n\
to specify file name and directory\n\
for the commands \"Save...\" and\n\
\"Input...\" from the \"File\" menu.\n\
After a file name has been selected\n\
click \"OK\" button to execute the\n\
command for which the panel have\n\
been opened.\n"
;

/***********************************************************************
             Contour Panel
***********************************************************************/

/**********************************************************************/
char fp_contpanel_help_text[] = "\
                     Contour panel\n\
                     -------------\n\
\n\
Contour panel provides interactive access to Minuit command\n\
\"MNContour\". A contour calculated by \"MNContour\" is\n\
dynamic, in the sense that it represents the minimum of the\n\
fit model with respect to all the parameters other than the\n\
two selected on the panel. In statistical terms, this means\n\
that \"MNContour\" takes account of the correlations\n\
between the two parameters being plotted, and all the other\n\
variable parameters, using a procedure analogous to that of\n\
\"Minos\". If this feature is not wanted, then the other\n\
parameters must be fixed before calling \"MNContour\".\n\
To draw more than one contour it is necessary to set\n\
\"Errordef\" on the main panel to the appropriate value,\n\
set \"Overlay\" toggle on, and activate \"Compute\" button\n\
for each contour desired.\n\
\n\
\"Compute\" button - Compute the contour coordinates and plot\n\
        it in the PAW++ graphical window.\n\
\n\
\"Overlay\" toggle - If on, each consecutive plot is\n\
        overlaid over the previous ones.\n\
\n\
\"Num1\", \"Num2\" input fields - Specify the numbers of a\n\
        pair of parameters in respect to which the contour\n\
        is to be plotted.\n\
\n\
\"Npt\" input field - Specifies the number of points (>4)\n\
        to be computed on the contour.\n\
\n\
\"Nfnd\" display field - Shows the number of points actually\n\
        found on the contour.\n\
\n\
\"Save in PAW vectors\" button - Save X and Y coordinates of\n\
        the previously computed contour in KUIP vectors\n\
        specified in \"X\", \"Y\" text input fields. If a\n\
        vector does not exist, it is created with the\n\
        appropriate length. If a vector exists but is too short\n\
        or has a wrong type, an error message is given.\n\
\n\
\"X\", \"Y\" input fields - Specify the names of KUIP vectors\n\
        in which X and Y coordinates of a contour are saved.\n\
\n\
\"Close\" button - Close the panel.\n\
\n"
;

/***********************************************************************
             Scan Panel
***********************************************************************/

/**********************************************************************/
char fp_scanpanel_help_text[] = "\
                     Scan panel\n\
                     ----------\n\
\n\
The fit model dependence on the specified parameter is\n\
computed and plotted, while all the remaining parameters\n\
are fixed at the current value. This panel may be useful\n\
for debugging the fit model or finding a reasonable\n\
starting point for minimization.\n\
\n\
\"Compute\" button - Compute the fit model at \"Npt\"\n\
        values of parameter \"Num\", equispaced along\n\
        the \"From\", \"To\" interval and plot the\n\
        resulting graph in the PAW++ graphical window.\n\
\n\
\"Num\" input field - Specifies the number of a\n\
        parameter in respect to which the fit model is\n\
        to be plotted.\n\
\n\
\"Npt\" input field - Specifies the number of points\n\
        to be computed along the specified interval.\n\
\n\
\"From\", \"To\" input fields - Specify the interval\n\
        of the parameter value along which the fit\n\
        model is plotted. When the panel is opened or\n\
        the parameter number is changed, the interval\n\
        is initialized to be identical to the scale\n\
        interval of the parameter currently specified\n\
        on the panel.\n\
\n\
\"Close\" button - Close the panel.\n\
\n"
;

/***********************************************************************
             Transfer Panel
***********************************************************************/

/**********************************************************************/
char fp_transpanel_help_text[] = "\
              Transfer panel\n\
              --------------\n\
\n\
Copy internal Fit Panel vectors into KUIP vectors.\n\
If a vector with specified name does not exist, a\n\
new REAL vector with the appropriate length is\n\
created. If a vector exists but has a wrong type\n\
or is too short to hold the corresponding panel\n\
vector, an error message is given. If Fit Panel is\n\
invoked through \"histogram/fit\" or \"vector/fit\"\n\
command, the corresponding internal Fit Panel\n\
vectors are initialized by the KUIP vectors given\n\
as command arguments. The names of these KUIP\n\
vectors will appear in the appropriate vector name\n\
input fields on the Transfer Panel.\n\
\n\
\"Save in PAW vectors\" button - Save panel vectors\n\
        in KUIP vectors specified in the text input\n\
        fields below.\n\
\n\
\"Parameters\" input field - Specifies a KUIP vector\n\
        to copy the vector of fit model parameters.\n\
\n\
\"Steps\" input field - Specifies a KUIP vector\n\
        to copy the vector of starting step sizes.\n\
\n\
\"Lower bounds\", \"Upper bounds\" input fields -\n\
        - Specify KUIP vectors to copy the vectors\n\
        storing parameter limits.\n\
\n\
\"Errors\" input field - Specifies a KUIP vector to\n\
        copy the vector of parabolic errors.\n\
\n\
\"Close\" button - Close the panel.\n\
\n"
;


/***********************************************************************
***********************************************************************/
static void km_help(char *help_cmd)
{
    km_display_sctext ("Help", help_cmd, 0, 0, 0);
}

/***********************************************************************
***********************************************************************/
static void setContextHelpMode(
Widget w,
XtPointer client_data,
XEvent *event)
{
    static Cursor context_cursor = 0;
    int iline;
    Widget cw,pw,aw;

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    XFlush(XtDisplay(mainPanelShell));

    if ( context_cursor == 0 )
        context_cursor =
            XCreateFontCursor(XtDisplay(mainPanelShell), XC_question_arrow);

    cw = XmTrackingLocate( mainPanelShell, context_cursor, FALSE);
    pw = XtParent(cw); /* parental widget */
    aw = cw; /* ancestral widget */
    while (
        aw!=NULL && aw!=mainForm && aw!=parForm &&
        aw!=fileSelPanel &&
        aw!=contourPanel &&
        aw!=scanPanel &&
        aw!=vectPanel)
    {
        aw = XtParent(aw);
    }

    /***** find the part for which help was requested and display it *****/
    /*********************************************************************/

    /***** Invalid widget *****/
    if (cw==NULL) { /* it is not a widget */
        km_help(error_help_text);
    }

    /***** Pull-down menus *****/
    else if (cw==fileMenu_top_b) {
        km_help(filemenu_help_text);
    }
    else if (cw==minimizeMenu_top_b) {
        km_help(minimenu_help_text);
    }
    else if (cw==examineMenu_top_b) {
        km_help(examenu_help_text);
    }
    else if (cw==scanMenu_top_b) {
        km_help(scanmenu_help_text);
    }
    else if (cw==transMenu_top_b) {
        km_help(transmenu_help_text);
    }
    else if (cw==viewMenu_top_b) {
        km_help(viewmenu_help_text);
    }

    /***** Fit object line *****/
    else if (cw==histoIdLabel || cw==histoIdText || cw==nbinLabel) {
        km_help(histId_help_text);
    }
    else if (cw==rangeLabel || cw==rangeText) {
        km_help(hrange_help_text);
    }
    else if (cw==xvectLabel || cw==xvectText) {
        km_help(xvect_help_text);
    }
    else if (cw==yvectLabel || cw==yvectText) {
        km_help(yvect_help_text);
    }
    else if (cw==eyvectLabel || cw==eyvectText) {
        km_help(eyvect_help_text);
    }
    else if (cw==optnLabel || cw==optnText) {
        km_help(optn_help_text);
    }

    /***** Fit function and fit model line *****/
    else if (cw==ffuncLabel || cw==ffuncText) {
        km_help(ffunc_help_text);
    }
    else if (cw==nparLabel || cw==nparText) {
        km_help(npar_help_text);
    }
    else if (cw==fmodelLabel || cw==fmodelText) {
        km_help(fmodel_help_text);
    }

    /***** Fit status line *****/
    else if (cw==fvalLabel || cw==fvalText) {
        km_help(fval_help_text);
    }
    else if (cw==fminLabel || cw==fminText) {
        km_help(fmin_help_text);
    }
    else if (cw==fedmLabel || cw==fedmText) {
        km_help(fedm_help_text);
    }

    /***** Minuit settings line *****/
    else if (pw==strategyMenu) {
        km_help(strategy_help_text);
    }
    else if (pw==printoutMenu) {
        km_help(printout_help_text);
    }
    else if (pw==updateModeMenu) {
        km_help(update_help_text);
    }
    else if (cw==errordefLabel || cw==errordefText) {
        km_help(errordef_help_text);
    }
    else if (cw==toleranceLabel || cw==toleranceText) {
        km_help(tolerance_help_text);
    }
    else if (cw==devsLabel || cw==devsText) {
        km_help(devs_help_text);
    }
    else if (cw == parZoneLabel) {
        km_help(parzone_help_text);
    }
    /***** No valid widget on main panel *****/
    else if (cw==mainForm || aw==mainForm) {
        km_help(error_help_text);
    }
    /***** Invalid ancestor widget *****/
    else if (aw==NULL) { /* it is not a widget */
        km_help(error_help_text);
    }

    /***** Parameter lines *****/
    else if (aw == parForm) {
        int nocw = 0; /* count number of lines in which cw is not found */
        for (iline=0; iline<npar; iline++) {
            UxParLineFormContext =
                (_UxCparLineForm *) UxGetContext(parLines[iline]);
            if (cw == parNumberValueLabel) {
                km_help(parnum_help_text);
            }
            else if (cw == parNameText) {
                km_help(parname_help_text);
            }
            else if (pw == parScale) {
                km_help(parscale_help_text);
            }
            else if (cw == parScaleAdjustButton) {
                km_help(paradjbut_help_text);
            }
            else if (cw == parValueText) {
                km_help(parvalue_help_text);
            }
            else if (cw == parFixToggle) {
                km_help(parfixtoggle_help_text);
            }
            else if (cw==parLowLimitText || cw==parUppLimitText) {
                km_help(parlimits_help_text);
            }
            else if (cw == parStepText) {
                km_help(parstep_help_text);
            }
            else if (cw == parEparabText) {
                km_help(pareparab_help_text);
            }
            else if (cw==parEminusText || cw==parEplusText) {
                km_help(parminos_help_text);
            }
            else if (cw == parGlobccText) {
                km_help(parglobcc_help_text);
            }
            /* adjusting device */
            else if (cw==parScaleMinMaxForm || pw==parScaleMinMaxForm) {
                km_help(paradjdev_help_text);
            }
            else {
                nocw++;
            }
        }
        /***** no cw is found among line widgets *****/
        if (nocw == npar) {
            km_help(parzone_help_text);
        }
    }
    /***** popup panels *****/
    else if (aw == fileSelPanel) {
        km_help(fp_filesel_help_text);
    }
    else if (aw == contourPanel) {
        km_help(fp_contpanel_help_text);
    }
    else if (aw == scanPanel) {
        km_help(fp_scanpanel_help_text);
    }
    else if (aw == vectPanel) {
        km_help(fp_transpanel_help_text);
    }
    else {  /***** control should never come here *****/
        km_help(error_help_text);
    }
}

/***********************************************************************
 Register callbacks for Help menu
***********************************************************************/
void helpmenuSetClbs(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    XtAddCallback( helpMenu_context, XmNactivateCallback,
            (XtCallbackProc) setContextHelpMode,
            (XtPointer) UxMainPanelShellContext );

    XtAddCallback( helpMenu_minuit, XmNactivateCallback,
            (XtCallbackProc) km_help_cb,
            (XtPointer) minuit_help_text);

    XtAddCallback( helpMenu_panel, XmNactivateCallback,
            (XtCallbackProc) km_help_cb,
            (XtPointer) panel_help_text);
}
