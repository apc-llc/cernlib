/*
 * $Id: caption.h,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: caption.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*
 * Copyright(c) 1992 Bell Communications Research, Inc. (Bellcore)
 *                        All rights reserved
 * Permission to use, copy, modify and distribute this material for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Bellcore not be used in advertising
 * or publicity pertaining to this material without the specific,
 * prior written permission of an authorized representative of
 * Bellcore.
 *
 * BELLCORE MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES, EX-
 * PRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR ANY PARTICULAR PURPOSE, AND THE WARRANTY AGAINST IN-
 * FRINGEMENT OF PATENTS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  THE
 * SOFTWARE IS PROVIDED "AS IS", AND IN NO EVENT SHALL BELLCORE OR
 * ANY OF ITS AFFILIATES BE LIABLE FOR ANY DAMAGES, INCLUDING ANY
 * LOST PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES RELAT-
 * ING TO THE SOFTWARE.
 *
 * CaptionWidget Author: Andrew Wason, Bellcore, aw@bae.bellcore.com
 */

#ifndef _Xbae_Caption_h
#define _Xbae_Caption_h

/*
 *  "@(#)Caption.h      1.5 7/8/92"
 */

/*
 * Caption Widget public include file
 */

#include <Xm/Xm.h>
#include <X11/Core.h>


/* Resources:
 * Name                 Class           RepType         Default Value
 * ----                 -----           -------         -------------
 * fontList             FontList        FontList        dynamic
 * labelAlignment       LabelAlignment  LabelAlignment  AlignmentCenter
 * labelOffset          LabelOffset     int             0
 * labelPixmap          LabelPixmap     PrimForegroundPixmap
 *                                                      XmUNSPECIFIED_PIXMAP
 * labelPosition        LabelPosition   LabelPosition   PositionLeft
 * labelString          XmString        XmString        widget name
 * labelTextAlignment   Alignment       Alignment       XmALIGNMENT_CENTER
 * labelType            LabelType       LabelType       XmSTRING
 */

/*
 * New resource constants
 */
#define XmNlabelPosition "labelPosition"
#define XmCLabelPosition "LabelPosition"
#define XmNlabelAlignment "labelAlignment"
#define XmCLabelAlignment "LabelAlignment"
#define XmNlabelTextAlignment "labelTextAlignment"
#define XmNlabelOffset "labelOffset"
#define XmCLabelOffset "LabelOffset"

#define XmRLabelPosition "LabelPosition"
#define XmRLabelAlignment "LabelAlignment"

/* Class record constants */

extern WidgetClass xbaeCaptionWidgetClass;

typedef struct _XbaeCaptionClassRec *XbaeCaptionWidgetClass;
typedef struct _XbaeCaptionRec *XbaeCaptionWidget;

/*
 * Type for XmNlabelPosition resource
 */
typedef enum _XbaeLabelPosition {
    XbaePositionLeft,
    XbaePositionRight,
    XbaePositionTop,
    XbaePositionBottom
} XbaeLabelPosition;

/*
 * Type for XmNlabelAlignment resource
 */
typedef enum _XbaeLabelAlignment {
    XbaeAlignmentTopOrLeft,
    XbaeAlignmentCenter,
    XbaeAlignmentBottomOrRight
} XbaeLabelAlignment;

#endif /* _Xbae_Caption_h */
/* DON'T ADD STUFF AFTER THIS #endif */

