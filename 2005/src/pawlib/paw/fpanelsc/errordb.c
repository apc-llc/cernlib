/*
 * $Id: errordb.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: errordb.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Error-reporting module.
 =====================> Author: G. Kozlovsky, 1994 <===========================
********************************************************************************

     Considerations about the organization of error-reporting in a
     program
     -------------------------------------------------------------

 Generating and printing error messages in the application program
 immediately after an error condition is detected is not a good idea
 for the following reasons:
     o The same strings with messages and other message-generating code
       have to be repeated many times
     o The error may have been detected in a more generic subroutine
       (e.g.  reading a floating point number from a text string), but
       the message would be better given in more specific terms (e.g it
       should include the name of the parameter which value has been
       found invalid), and therefore should be postponed till the
       control returns to a more specific routine which has all the
       necessary information
     o The details of error reporting may vary depending on the
       information received after the error condition was first
       detected
 Because of the above considerations it is clear that there is a need
 in a separate error database module. There are two approaches which
 immediately come to mind.
     1. Create a generic error-reporting function with a variable number
        of arguments and a header file with the definition of error codes.
        The error-reporting function will have the error code as its
        first parameter and will treat the rest of the parameters
        depending on the error code. The disadvantage of this approach
        is that we turn off the C function arguments control and that
        the error codes must be global for all application. In addition,
        we will have to put a switch at every point in the program
        where we want to report an error and can expect several possible
        error conditions which each require different set of arguments.
     2. Create a specific function for every error condition. With this
        approach we have to put a switch at every point where we want to
        report an error and can expect several possible error conditions
        to arise. It would be also difficult to re-use the same code to
        treat together several similar error conditions which differ in
        some details.
 We chose here an intermediate approach which consist of having several
 error-reporting functions with fixed number of parametters, each function
 processing several error conditions which are similar, and can be
 expected to arise as a result of a certain action.

 Another design decision is on how to transfer the error codes between
 different functions of the program. One approach would be to have
 functions in which an error condition arises return the corresponding
 error code. The shortcoming of this approach are:
  o It may be that several different error conditions can arise
    during the execution of a function
  o The procedure which should signal the error may be several calls
    removed from the procedure where the error condition arises.
 Therefore, we chose to have several external variables in which an
 error condition is reported.

***********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"

#ifdef __cplusplus
}  /* extern "C" */
#endif


int currErrorCode;


/***********************************************************************
***********************************************************************/
void textFloatError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];

    switch (cc) {
    case TEXT_OK: case TEXT_EMPTY: case TEXT_NOCHANGE:
    break;
    case TEXT_INVALID:
        km_warn_user(panelShell,"Invalid floating point number","");
    break;
    case TEXT_INVDAFTER:
        km_warn_user(panelShell,
            "Invalid token after floating point number","");
    break;
    case TEXT_INVDEMPTY:
        sprintf(errmsg,"Empty field is invalid for %s\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    case TEXT_INVDNONPOS:
        sprintf(errmsg,"Non-positive value is invalid for %s\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void textIntError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];

    switch (cc) {
    case TEXT_OK: case TEXT_EMPTY: case TEXT_NOCHANGE:
    break;
    case TEXT_INVALID:
        km_warn_user(panelShell,"Invalid integer","");
    break;
    case TEXT_INVDAFTER:
        km_warn_user(panelShell,
            "Invalid token after integer","");
    break;
    case TEXT_INVDEMPTY:
        sprintf(errmsg,"Empty field is invalid for %s\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    case TEXT_INVDNONPOS:
        sprintf(errmsg,"Non-positive value is invalid for %s\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void textNameError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];

    switch (cc) {
    case TEXT_OK: case TEXT_EMPTY: case TEXT_NOCHANGE:
    break;
    case TEXT_INVALID:
        sprintf(errmsg,"Invalid %s identifier\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    case TEXT_INVDAFTER:
        sprintf(errmsg,"Invalid token after %s identifier\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    case TEXT_INVDEMPTY:
        sprintf(errmsg,"Empty field is invalid for %s identifier\n",valname);
        km_warn_user(panelShell, errmsg,"");
    break;
    case TEXT_INVDNONPOS:
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void textRangeError(RangeReadCode cc, int idim, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];
    char *dimname = "XYZ";

    switch (cc) {
    case RANGE_OK:
    break;
    case RANGE_EMPTY:
        km_warn_user(panelShell,
            "Empty histogram range field is invalid","");
    break;
    case RANGE_INVDLOW:
        sprintf(errmsg,"Invalid histogram range lower limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDLOWSEM:
        sprintf(errmsg,"Out of bounds histogram range lower limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDUPP:
        sprintf(errmsg,"Invalid histogram range upper limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDUPPSEM:
        sprintf(errmsg,"Out of bounds histogram range upper limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDORDER:
        sprintf(errmsg,"Lower limit greater than upper limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDLOWUPPSEP:
        sprintf(errmsg,"Invalid separator between lower and upper limit in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg, "");
    break;
    case RANGE_INVDDIMSEP:
        sprintf(errmsg,"Invalid separator after histogram range in %c",
            dimname[idim]);
        km_warn_user(panelShell, errmsg,"");
    break;
    case RANGE_INVDAFTER:
        km_warn_user(panelShell,
            "Invalid token after histogram range","");
    break;
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void parLimitsError(LimitsErrorCode cc,
    char *limname, int iparline, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];

    switch (cc) {
    case LIMITS_NOERR:
    break;
    case LIMITS_OVERLAP:
        km_warn_user(panelShell, "Limits are invalid","");
    break;
    case LIMITS_PARAMETEROUT:
        km_warn_user(panelShell, "Parameter is out of limits","");
    break;
    case LIMITS_LOWEMPTY: case LIMITS_UPPEMPTY:
        km_warn_user(panelShell, "One limit is blank","");
    break;
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void parScaleError(ScaleErrorCode cc, int iparline, Widget panelShell)
{
    char errmsg[MAX_ERRMSGLEN];

    switch (cc) {
    case SCALE_NOERR:
    break;
    case SCALE_PARAMETEROUT:
        km_warn_user(panelShell,
                    "Parameter is outside of scale interval","");
    break;
    case SCALE_OVERLAP:
        km_warn_user(panelShell,
                    "Scale interval is invalid","");
    break;
    case SCALE_OUTLIMITS:
        km_warn_user(panelShell,
            "Scale interval outside of parameter limits","");
    break;
    default:;
        internalError();
    }
}

/***********************************************************************
***********************************************************************/
void _internalError(char *fname, int line)
{
    printf("%s:%d: Internal errror, notify program developers\n",fname,line);
    exit(1);
}
