/*
 * $Id: mkuxpb.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxpb.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/*******************************************************************************
       PalButton.h
       This header file is included by PalButton.c

*******************************************************************************/

#ifndef       _PALBUTTON_INCLUDED
#define       _PALBUTTON_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Label.h>
#include <Xm/ArrowB.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>

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

typedef       struct
{
       Widget       UxPalButton;
       Widget       UxPNBframe1;
       Widget       UxPNBform1;
       Widget       UxarrowButtonB;
       Widget       Uxkm_palLabelB;
       swidget       UxPalParent;
} _UxCPalButton;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCPalButton           *UxPalButtonContext;
#define PalButton               UxPalButtonContext->UxPalButton
#define PNBframe1               UxPalButtonContext->UxPNBframe1
#define PNBform1                UxPalButtonContext->UxPNBform1
#define arrowButtonB            UxPalButtonContext->UxarrowButtonB
#define km_palLabelB            UxPalButtonContext->Uxkm_palLabelB
#define PalParent               UxPalButtonContext->UxPalParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget       create_PalButton();

#endif       /* _PALBUTTON_INCLUDED */
