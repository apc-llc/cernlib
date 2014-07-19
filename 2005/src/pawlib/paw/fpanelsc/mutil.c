/*
 * $Id: mutil.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: mutil.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Motif-related utilities used in the program.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/fplocal.h"

#ifdef __cplusplus
}  /* extern "C" */
#endif


/***********************************************************************
 Query widget preferred geometry in its native units
 (XtQueryGeometry always returns geometry in pixels)
***********************************************************************/
static void utlQueryGeometry(
Widget w,
Position *x,
Position *y,
Dimension *width,
Dimension *height)
{
    unsigned char unit = XmPIXELS;
    XtWidgetGeometry preferred;

    XtVaGetValues(w,XmNunitType,&unit,NULL);
    XtQueryGeometry(w,NULL,&preferred);

    if (x !=NULL)
        *x = XmConvertUnits(w,XmHORIZONTAL,XmPIXELS,preferred.x,unit);
    if (width !=NULL)
    *width = XmConvertUnits(w,XmHORIZONTAL,XmPIXELS,preferred.width,unit);
    if (y !=NULL)
    *y = XmConvertUnits(w,XmVERTICAL,XmPIXELS,preferred.y,unit);
    if (height !=NULL)
    *height = XmConvertUnits(w,XmVERTICAL,XmPIXELS,preferred.height,unit);
}

/***********************************************************************
 Query widget preferred dimensions in its native units
 (XtQueryGeometry always returns geometry in pixels)
***********************************************************************/
void utlQueryDimensions(
Widget w,
Dimension *width,
Dimension *height)
{
    unsigned char unit = XmPIXELS;
    XtWidgetGeometry preferred;

    XtVaGetValues(w,XmNunitType,&unit,NULL);
    XtQueryGeometry(w,NULL,&preferred);

    if (width !=NULL)
    *width = XmConvertUnits(w,XmHORIZONTAL,XmPIXELS,preferred.width,unit);
    if (height !=NULL)
    *height = XmConvertUnits(w,XmVERTICAL,XmPIXELS,preferred.height,unit);
}
