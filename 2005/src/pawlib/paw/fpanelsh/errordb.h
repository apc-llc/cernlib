/*
 * $Id: errordb.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: errordb.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.34.35  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/

/*******************************************************************************
 Error codes and error-reporting functions. Each group of codes
 is placed together with the prototype of the function which treats
 them.
=====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/

#define MAX_ERRMSGLEN 100

extern int currErrorCode; /* current error code must always be placed here */

/****** generic return codes *****/
typedef enum _GenRetCode {OK_RET, ERROR_RET} GenRetCode;

/****** return codes for text field reading function ******/
typedef enum _TextReadCode {TEXT_OK, TEXT_EMPTY, TEXT_INVALID, TEXT_INVDAFTER,
    TEXT_INVDEMPTY, TEXT_INVDNONPOS, TEXT_NOCHANGE
} TextReadCode;

void textFloatError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell);
void textIntError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell);
void textNameError(TextReadCode cc,
    char *valname, int iparline, Widget panelShell);

/****** return codes for histo range text field reading function ******/
typedef enum _RangeReadCode {RANGE_OK, RANGE_EMPTY,
    RANGE_INVDLOW, RANGE_INVDLOWSEM, RANGE_INVDUPP, RANGE_INVDUPPSEM,
    RANGE_INVDORDER, RANGE_INVDLOWUPPSEP, RANGE_INVDDIMSEP,
    RANGE_INVDAFTER, RANGE_NOCHANGE
} RangeReadCode;

void textRangeError(RangeReadCode cc, int idim, Widget panelShell);

/****** errors of the limits ****
 limits are absent if they are both specified as zero or both fields are
 empty; limits are invalid if lower limit >= than upper limit, or one of
 the fields is empty
******/
typedef enum _LimitsErrorCode {
    LIMITS_NOERR, LIMITS_OVERLAP, LIMITS_PARAMETEROUT,
    LIMITS_LOWEMPTY, LIMITS_UPPEMPTY
} LimitsErrorCode;
void parLimitsError(LimitsErrorCode cc,
    char *parname, int iparline, Widget panelShell);

/***** parameter scale error code *****/
typedef enum _ScaleErrorCode {
    SCALE_NOERR, SCALE_PARAMETEROUT, SCALE_OVERLAP, SCALE_OUTLIMITS
} ScaleErrorCode;

void parScaleError(ScaleErrorCode cc, int iparline, Widget panelShell);


void _internalError(char *fname, int line);
#define internalError() _internalError(__FILE__,__LINE__)

