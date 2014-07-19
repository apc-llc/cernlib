/*
 * $Id: icwid.h,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: icwid.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */

/* IconWidget.h */

/*
 * Copyright 1991 Addison-Wesley Publishing Company
 *                (see the file "COPYRIGHTS" for details)
 */

#ifndef _IconWidget_h
#define _IconWidget_h

#ifndef NeedFunctionPrototypes
#define NeedFunctionPrototypes 0
#endif

#include <Xm/Xm.h>

#define XmNiconOffset     "iconOffset"
#define XmCIconOffset     "IconOffset"

extern WidgetClass iconWidgetClass;

typedef struct _IconWidgetClassRec * IconWidgetClass;
typedef struct _IconWidgetRec      * IconWidget;

Widget CreateIconLabel (
#if NeedFunctionPrototypes
    Widget       /* parent   */,
    char*        /* name     */,
    ArgList      /* arglist  */,
    Cardinal     /* argcount */,
    char*        /* pixmap   */
#endif
);

Widget CreateIconButton (
#if NeedFunctionPrototypes
    Widget       /* parent   */,
    char*        /* name     */,
    ArgList      /* arglist  */,
    Cardinal     /* argcount */,
    char*        /* pixmap   */
#endif
);

Widget CreateIconWidget (
#if NeedFunctionPrototypes
    Widget       /* parent   */,
    char*        /* name     */,
    ArgList      /* arglist  */,
    Cardinal     /* argcount */
#endif
);

#endif /* _IconWidget_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
