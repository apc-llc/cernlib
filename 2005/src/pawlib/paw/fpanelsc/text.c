/*
 * $Id: text.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: text.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.42  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Generic functions for reading and analysing data from text fields and for
 writing into text fields.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

#include "hmotif/uxxt.h"
#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS
#include "fpanelsh/lexer.h"


static Pixel textTopShadowColor = 0;
static Pixel textBottomShadowColor = 0;

/***********************************************************************
 Initialize colors for text field marking.
***********************************************************************/
void utl_initMarkText(Widget text)
{
    XtVaGetValues(text,
        XmNbottomShadowColor, &textBottomShadowColor,
        XmNtopShadowColor, &textTopShadowColor, NULL);

}

/***********************************************************************
 Mark text field as invalid by putting a red frame around it.
***********************************************************************/
void utl_markTextInvalid(Widget text)
{
    XtVaSetValues(text,
        RES_CONVERT( XmNbottomShadowColor, "Red" ),
        RES_CONVERT( XmNtopShadowColor, "Red" ), NULL);
}

/***********************************************************************
 Remove red frame marking text field as invalid.
***********************************************************************/
void utl_markTextValid(Widget text)
{
    XtVaSetValues(text,
        XmNbottomShadowColor, textBottomShadowColor,
        XmNtopShadowColor, textTopShadowColor, NULL);
}

/***********************************************************************
DESCRIPTION:
    Read a floating point value from a text widget with syntax error cheking.
    The value is read only if it have been changed since the last reading
    as indicated by status. If a syntax error have been found, the
    text field is visually marked and a warning message is popped up.
    An empty field is not considered an error by this function, and
    0.0 is returned as the value. If the value has been succesfully read,
    it is checked by a user-supplied function valueok, and if rejected,
    the error is signaled as before with a user-supplied message given
    in the argument msg.

ARGUMENTS:
    text - text widget
    *status - current status of the field on input, new status on
        output (enum textstatus)
    *dval - the read value is returned if there was no syntax error and
        the field is not empty
    valueok - a function which given a pointer to the value read, returns
        1 if the application accepts it and 0 if not; if the field is
        empty, the pointer to the value is NULL

EXTERNALS: None
***********************************************************************/
void textReadFloat(Widget text, TextStatusInfo *status,
    double *dp, int (*valueok)(double dval))
{
    String textstr;
    int sign = 1;
    double dval;

    if (status->change != TEXTSTAT_CHANGED) {
        currErrorCode = TEXT_NOCHANGE;
        return;
    }
    currErrorCode = TEXT_OK;

    XtVaGetValues(text,XmNvalue, &textstr,NULL);

    /****** read and analyze the field *****/
    lx_load(textstr);
    lx_next();
    if (lx_token.type == NEWLINE_T) { /* empty field */
        dval = 0.0;
        currErrorCode = TEXT_EMPTY;
        goto ok_proc;
    }

    if (lx_token.type == OPER_T+'+') lx_next();
    else if (lx_token.type == OPER_T+'-') {sign = -1; lx_next();}
    if (lx_token.type == DOUBLE_T) {
        dval = sign*lx_token.val.d;
        currErrorCode = TEXT_OK;
    }
    else if (lx_token.type == INT_T) {
        dval = sign*(double)lx_token.val.i;
        currErrorCode = TEXT_OK;
    }
    else {
        currErrorCode = TEXT_INVALID;
        goto endproc;
    }

    lx_next();
    if (lx_token.type != NEWLINE_T) {
        currErrorCode = TEXT_INVDAFTER;
        goto endproc;
    }

    /***** perform user check *****/
ok_proc:
    valueok(dval); /* changes error code according to user preference */

    /***** return the number or signal the error *****/
endproc:
    status->change = TEXTSTAT_NOCHANGE;
    switch (currErrorCode) {
    case TEXT_OK:
        if (status->stat==TEXTSTAT_SYNTERROR) utl_markTextValid(text);
        *dp = dval;
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
    break;
    case TEXT_EMPTY:
        if (status->stat==TEXTSTAT_SYNTERROR) utl_markTextValid(text);
        *dp = dval;
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_EMPTY;
    break;
    case TEXT_INVALID:
    case TEXT_INVDAFTER:
    case TEXT_INVDEMPTY:
    case TEXT_INVDNONPOS:
        if (!(status->stat&TEXTSTAT_ANYERROR)) utl_markTextInvalid(text);
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_SYNTERROR;
    break;
    case TEXT_NOCHANGE:
    default:
        internalError();
    }
}

/***********************************************************************
 Functions to check a value read by textReadFloat.
***********************************************************************/

/***** empty is marked as a different state *****/
int anyFloatOrEmptyOK(double dval) {return(1);}

/***** empty field is interpreted as 0 *****/
int anyFloatEmptyIs0OK(double dval)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_OK;
        return(0);
    }
    else {
        return(1);
    }
}

int nonEmptyFloatOK(double dval)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_INVDEMPTY;
        return(0);
    }
    else {
        return(1);
    }
}

int positiveFloatOK(double dval)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_INVDEMPTY;
        return(0);
    }
    else if (dval > 0.0) {
        return(1);
    }
    else {
        currErrorCode=TEXT_INVDNONPOS;
        return(0);
    }
}


/***********************************************************************
DESCRIPTION:
    Read an integer value from a text widget with syntax error cheking.
    The value is read only if it have been changed since the last reading
    as indicated by status. If a syntax error have been found, the
    text field is visually marked and a warning message is popped up.

ARGUMENTS:
    text - text widget
    *status - current status of the field on input, new status on
        output (enum textstatus)
    *ival - the read value is returned if there was no syntax error and
        the field is not empty
    valueok - a function which given a pointer to the value read, returns
        1 if the application accepts it and 0 if not; if the field is
        empty, the pointer to the value is NULL

EXTERNALS: None
***********************************************************************/
void textReadInt(Widget text, TextStatusInfo *status,
    int *ip, int (*valueok)(int dval))
{
    String textstr;
    int sign = 1;
    int ival;

    if (status->change != TEXTSTAT_CHANGED) {
        currErrorCode = TEXT_NOCHANGE;
        return;
    }
    currErrorCode = TEXT_OK;

    XtVaGetValues(text,XmNvalue, &textstr,NULL);

    /****** read and analyze the field *****/
    lx_load(textstr);
    lx_next();
    if (lx_token.type == NEWLINE_T) { /* empty field */
        ival = 0;
        currErrorCode = TEXT_EMPTY;
        goto ok_proc;
    }

    if (lx_token.type == OPER_T+'+') lx_next();
    else if (lx_token.type == OPER_T+'-') {sign = -1; lx_next();}
    if (lx_token.type == INT_T) {
        ival = sign*lx_token.val.i;
        currErrorCode = TEXT_OK;
    }
    else {
        currErrorCode = TEXT_INVALID;
        goto endproc;
    }

    lx_next();
    if (lx_token.type != NEWLINE_T) {
        currErrorCode = TEXT_INVDAFTER;
        goto endproc;
    }

    /***** perform user check *****/
ok_proc:
    valueok(ival); /* changes error code according to user preference */

    /***** return the number or signal the error *****/
endproc:
    status->change = TEXTSTAT_NOCHANGE;
    switch (currErrorCode) {
    case TEXT_OK:
        if (status->stat==TEXTSTAT_SYNTERROR) utl_markTextValid(text);
        *ip = ival;
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
    break;
    case TEXT_EMPTY:
        if (status->stat==TEXTSTAT_SYNTERROR) utl_markTextValid(text);
        *ip = ival;
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_EMPTY;
    break;
    case TEXT_INVALID:
    case TEXT_INVDAFTER:
    case TEXT_INVDEMPTY:
    case TEXT_INVDNONPOS:
        if (!(status->stat&TEXTSTAT_ANYERROR)) utl_markTextInvalid(text);
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_SYNTERROR;
    break;
    case TEXT_NOCHANGE:
    default:
        internalError();
    }
}

/***********************************************************************
 Functions to check a value read by textReadInt.
***********************************************************************/

/***** empty is marked as a different state *****/
int anyIntOrEmptyOK(int ival) {return(1);}

/***** empty field is interpreted as 0 *****/
int anyIntEmptyIs0OK(int ival)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_OK;
        return(0);
    }
    else {
        return(1);
    }
}

int nonEmptyIntOK(int ival)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_INVDEMPTY;
        return(0);
    }
    else {
        return(1);
    }
}

int positiveIntOK(int ival)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_INVDEMPTY;
        return(0);
    }
    else if (ival > 0) {
        return(1);
    }
    else {
        currErrorCode=TEXT_INVDNONPOS;
        return(0);
    }
}

/***********************************************************************
DESCRIPTION:
    Read a name from a text widget with syntax error cheking.
    The value is read only if it have been changed since the last reading
    as indicated by status. If a syntax error have been found, the
    text field is visually marked and a warning message is popped up.
    An empty field is not considered an error by this function.

ARGUMENTS:
    text - text widget
    *status - current status of the field on input, new status on
        output (enum textstatus)
    name - the read value is copied if there was no syntax error and
        the field is not empty
    valueok - a function which given a pointer to the value read, returns
        1 if the application accepts it and 0 if not; if the field is
        empty, the pointer to the value is NULL

EXTERNALS: None
***********************************************************************/
void textReadName(Widget text, TextStatusInfo *status,
    char *name, int (*valueok)(char *name,int n))
{
    String textstr;
    TOKLOC nameloc;

    if (status->change != TEXTSTAT_CHANGED) {
        currErrorCode = TEXT_NOCHANGE;
        return;
    }
    currErrorCode = TEXT_OK;

    XtVaGetValues(text,XmNvalue, &textstr,NULL);

    /****** read and analyze the field *****/
    lx_load(textstr);
    lx_next();
    nameloc = lx_loc;
    if (lx_token.type == NEWLINE_T) { /* empty field */
        currErrorCode = TEXT_EMPTY;
        goto ok_proc;
    }

    if (lx_token.type == IDENT_T) {
        currErrorCode = TEXT_OK;
    }
    else {
        currErrorCode = TEXT_INVALID;
        goto endproc;
    }

    lx_next();
    if (lx_token.type != NEWLINE_T) {
        currErrorCode = TEXT_INVDAFTER;
    }

    /***** perform user check *****/
ok_proc:
    /* changes error code according to user preference */
    valueok(textstr+nameloc.pos,nameloc.inputlen);

    /***** return the name or signal the error *****/
endproc:
    status->change = TEXTSTAT_NOCHANGE;
    switch (currErrorCode) {
    case TEXT_OK:
    case TEXT_EMPTY:
        if (status->stat==TEXTSTAT_SYNTERROR) utl_markTextValid(text);
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
        strncpy(name,textstr+nameloc.pos,nameloc.inputlen);
        name[nameloc.inputlen] = '\0';
    break;
    case TEXT_INVALID:
    case TEXT_INVDEMPTY:
    case TEXT_INVDAFTER:
        if (!(status->stat&TEXTSTAT_ANYERROR)) utl_markTextInvalid(text);
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_SYNTERROR;
    break;
    case TEXT_NOCHANGE:
    default:
        internalError();
    }
}

/***********************************************************************
 Functions to check a value read by textReadName.
***********************************************************************/

/***** empty is marked as a different state *****/
int anyNameOrEmptyOK(char *name, int n) {return(1);}

int nonEmptyNameOK(char *name, int n)
{
    if (currErrorCode==TEXT_EMPTY) {
        currErrorCode=TEXT_INVDEMPTY;
        return(0);
    }
    else {
        return(1);
    }
}

/***********************************************************************
DESCRIPTION:
    Read a string from a text widget.
    The value is read only if it have been changed since the last reading
    as indicated by status.
    An empty field is not considered an error by this function.

ARGUMENTS:
    text - text widget
    *status - current status of the field on input, new status on
        output (enum textstatus)
    str - the read string value

RETURN: a termination code is returned (enum textreadret)

EXTERNALS: None
***********************************************************************/
void textReadString(Widget text, TextStatusInfo *status, char *str)
{
    String textstr;

    if (status->change != TEXTSTAT_CHANGED) {
        currErrorCode = TEXT_NOCHANGE;
        return;
    }
    currErrorCode = TEXT_OK;

    XtVaGetValues(text,XmNvalue, &textstr,NULL);

    status->change = TEXTSTAT_NOCHANGE;
    status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
    strcpy(str,textstr);
}

/***********************************************************************
DESCRIPTION:
    Convert a floating point value into a string and write it into a
    text widget. If the widget field is marked as invalid, mark it as
    valid.

ARGUMENTS: self-explicable
***********************************************************************/
void textWriteFloat(Widget text, TextStatusInfo *status, double dval)
{
    char textstr[50];

    sprintf(textstr,"%g",dval);
    XmTextSetString(text,textstr);
    if (status != NULL) {
        if (status->stat == TEXTSTAT_SYNTERROR) {
            utl_markTextValid(text);
        }
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
        status->change = TEXTSTAT_NOCHANGE;
    }
}

/***********************************************************************
DESCRIPTION:
    Convert an integer value into a string and write it into a
    text widget. If the widget field is marked as invalid, mark it as
    valid.

ARGUMENTS: self-explicable
***********************************************************************/
void textWriteInt(Widget text, TextStatusInfo *status, int ival)
{
    char textstr[50];

    sprintf(textstr,"%d",ival);
    XmTextSetString(text,textstr);
    if (status != NULL) {
        if (status->stat == TEXTSTAT_SYNTERROR) {
            utl_markTextValid(text);
        }
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
        status->change = TEXTSTAT_NOCHANGE;
    }
}

/***********************************************************************
DESCRIPTION:
    Write empty string into a text widget. If the widget field is marked
    as invalid, mark it as valid.

ARGUMENTS: self-explicable
***********************************************************************/
void textWriteEmpty(Widget text, TextStatusInfo *status)
{
    XmTextSetString(text,"");
    if (status != NULL) {
        if (status->stat == TEXTSTAT_SYNTERROR) {
            utl_markTextValid(text);
        }
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_EMPTY;
        status->change = TEXTSTAT_NOCHANGE;
    }
}

/***********************************************************************
DESCRIPTION:
    Convert a floating point value into a string and write it into a
    label widget.

ARGUMENTS: self-explicable
***********************************************************************/
void labelWriteFloat(Widget label, double dval)
{
    char textstr[50];

    sprintf(textstr,"%g",dval);
    XtVaSetValues(label,
        RES_CONVERT(XmNlabelString, textstr), NULL);
}

/***********************************************************************
DESCRIPTION:
    Write string into a text widget. If the widget field is marked as
    invalid, mark it as valid.

ARGUMENTS: self-explicable
***********************************************************************/
void textWriteName(Widget text, TextStatusInfo *status, char *tstr)
{

    XmTextSetString(text,tstr);
    if (status != NULL) {
        if (status->stat == TEXTSTAT_SYNTERROR) {
            utl_markTextValid(text);
        }
        status->stat = (status->stat&TEXTSTAT_SEMANTERROR)|TEXTSTAT_READOK;
        status->change = TEXTSTAT_NOCHANGE;
    }
}

/***********************************************************************
DESCRIPTION:
    If the text field does not have a syntax error, mark it with
    semantic error. Syntax error has precedence over semantic error.

ARGUMENTS: self-explicable
***********************************************************************/
void textSetSemantError(Widget text, TextStatusInfo *status)
{
    if (!(status->stat&TEXTSTAT_ANYERROR)) {
        utl_markTextInvalid(text);
    }
    status->stat |= TEXTSTAT_SEMANTERROR;
}

/***********************************************************************
DESCRIPTION:
    If the text field does have a semantic error, remove
    semantic error. Syntax error has precedence over semantic error.

ARGUMENTS: self-explicable
***********************************************************************/
void textRemoveSemantError(Widget text, TextStatusInfo *status)
{
    if ((status->stat&TEXTSTAT_ANYERROR) == TEXTSTAT_SEMANTERROR) {
        utl_markTextValid(text);
    }
    status->stat &= ~TEXTSTAT_SEMANTERROR;
}

/***********************************************************************
 When text field have been edited, set .change field in TextStatusInfo
 structure to TEXTSTAT_CHANGED. This is used to prevent re-reading
 text fields which have not been chahged, every time the text field
 looses focus.
***********************************************************************/
void clb_textChanged(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    ((TextStatusInfo *)client_data)->change = TEXTSTAT_CHANGED;
}
