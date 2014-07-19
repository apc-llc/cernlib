/*
 * $Id: mkuxlihlp.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxlihlp.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/*******************************************************************************
       kuipListHelp.h
       This header file is included by kuipListHelp.c

*******************************************************************************/

#ifndef        _KUIPLISTHELP_INCLUDED
#define        _KUIPLISTHELP_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Text.h>
#include <Xm/SelectioB.h>
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
        Widget        UxkuipListHelp;
        swidget        UxlistHelpParent;
} _UxCkuipListHelp;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipListHelp        *UxKuipListHelpContext;
#define kuipListHelp            UxKuipListHelpContext->UxkuipListHelp
#define listHelpParent          UxKuipListHelpContext->UxlistHelpParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_listSBoxHelp;
extern Widget        km_listText;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipListHelp();

#endif        /* _KUIPLISTHELP_INCLUDED */
