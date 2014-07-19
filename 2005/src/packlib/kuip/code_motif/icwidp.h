/*
 * $Id: icwidp.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: icwidp.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/* IconWidgetP.h */

/*
 * Copyright 1991 Addison-Wesley Publishing Company
 *                (see the file "COPYRIGHTS" for details)
 */

#ifndef _IconWidgetP_h
#define _IconWidgetP_h

/* #include "IconWidget.h" */
#include "icwid.h"
#include <Xm/XmP.h>

/*  Icon class structure  */

typedef struct _IconWidgetClassPart
{
   caddr_t extension;
} IconWidgetClassPart;


/*  Full class record declaration for Icon class  */

typedef struct _IconWidgetClassRec
{
   CoreClassPart        	core_class;
   XmPrimitiveClassPart 	primitive_class;
   XmLabelClassPart             label_class;
   IconWidgetClassPart     	icon_class;
} IconWidgetClassRec;

extern IconWidgetClassRec iconWidgetClassRec;


/*  The icon widget instance record  */

typedef struct _IconWidgetPart
{
    short            offset;
    short            width,
                     height;
    unsigned char    shadow_type;
    Boolean 	     armed;
    XtCallbackList   activate_callback;
    XtCallbackList   arm_callback;
    XtCallbackList   disarm_callback;
} IconWidgetPart;


/*  Full instance record declaration  */

typedef struct _IconWidgetRec
{
   CorePart	   	core;
   XmPrimitivePart	primitive;
   XmLabelPart          label;
   IconWidgetPart       icon;
} IconWidgetRec;

#ifndef XtSpecificationRelease
#define XtCallCallbackList(w,cb,cd)  XtWidgetCallCallbacks(cb,cd)
#endif

#endif /* _IconWidgetP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
