/*
 * $Id: mkuxs1.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxs1.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipScroll1.h
       This header file is included by kuipScroll1.c

*******************************************************************************/

#ifndef        _KUIPSCROLL1_INCLUDED
#define        _KUIPSCROLL1_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
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

typedef        struct
{
        Widget        UxkuipScroll1;
        Widget        UxscrollSep1;
        Widget        UxScWindow1;
        Widget        UxscrollText1;
        swidget        Uxscroll1Parent;
} _UxCkuipScroll1;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipScroll1         *UxKuipScroll1Context;
#define kuipScroll1             UxKuipScroll1Context->UxkuipScroll1
#define scrollSep1              UxKuipScroll1Context->UxscrollSep1
#define ScWindow1               UxKuipScroll1Context->UxScWindow1
#define scrollText1             UxKuipScroll1Context->UxscrollText1
#define scroll1Parent           UxKuipScroll1Context->Uxscroll1Parent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_scrollOK1;
extern Widget        km_scrollCL1;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipScroll1();

#endif        /* _KUIPSCROLL1_INCLUDED */
