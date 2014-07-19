/*
 * $Id: about.c,v 1.1.1.1 1996/03/01 11:38:53 mclareni Exp $
 *
 * $Log: about.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:53  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.07/02 16/06/95  10.46.51  by  O.Couet*/
/*-- Author :    Fons Rademakers   03/03/93*/

#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>


#include "hmotif/pawm.h"

/*KEEP,VERSQQ.*/
#define VERSQQ " 2.07/21"
#define IVERSQ  20721
/*KEND.*/
/*KEEP,DATEQQ.*/
#define IDATQQ 960214
/*KEND.*/

typedef struct {
   XtIntervalId     id;
   int              which;
   int              num_pix;
   Pixmap           pix[10];
   Widget           dialog;
   XtAppContext     app;
} TimeOutClientData;


/***********************************************************************
 *                                                                     *
 *   Remove (unmanage) about dialog.                                   *
 *                                                                     *
 ***********************************************************************/
static void close_about_cb(Widget w, TimeOutClientData *data,
                    XmAnyCallbackStruct *call_data)
{
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) ABOUT argument popup.                               *
 *                                                                     *
 ***********************************************************************/
void show_aboutDialog(Widget parent)
{
}
