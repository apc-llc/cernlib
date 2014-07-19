/*
 * $Id: mkuxol.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxol.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipOptList.h
       This header file is included by kuipOptList.c

*******************************************************************************/

#ifndef        _KUIPOPTLIST_INCLUDED
#define        _KUIPOPTLIST_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
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
        Widget        UxkuipOptList;
        Widget        UxOptListSep;
        Widget        UxOKButton;
        Widget        UxCCLButton;
        Widget        UxscrolledWindow1;
        Widget        UxOptList;
} _UxCkuipOptList;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipOptList         *UxKuipOptListContext;
#define kuipOptList             UxKuipOptListContext->UxkuipOptList
#define OptListSep              UxKuipOptListContext->UxOptListSep
#define OKButton                UxKuipOptListContext->UxOKButton
#define CCLButton               UxKuipOptListContext->UxCCLButton
#define scrolledWindow1         UxKuipOptListContext->UxscrolledWindow1
#define OptList                 UxKuipOptListContext->UxOptList

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipOptList();

#endif        /* _KUIPOPTLIST_INCLUDED */
