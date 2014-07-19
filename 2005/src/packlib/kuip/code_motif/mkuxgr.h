/*
 * $Id: mkuxgr.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxgr.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipGraphics.h
       This header file is included by kuipGraphics.c

*******************************************************************************/

#ifndef        _KUIPGRAPHICS_INCLUDED
#define        _KUIPGRAPHICS_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure.
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef        struct
{
        Widget        UxkuipGraphics;
        Widget        UxkuipGraphicsFrame;
        Widget        UxgraphicsForm;
} _UxCkuipGraphics;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipGraphics        *UxKuipGraphicsContext;
#define kuipGraphics            UxKuipGraphicsContext->UxkuipGraphics
#define kuipGraphicsFrame       UxKuipGraphicsContext->UxkuipGraphicsFrame
#define graphicsForm            UxKuipGraphicsContext->UxgraphicsForm

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_graphicsDraw;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipGraphics();

#endif        /* _KUIPGRAPHICS_INCLUDED */
