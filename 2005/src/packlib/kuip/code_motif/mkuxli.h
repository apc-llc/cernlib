/*
 * $Id: mkuxli.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxli.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipList.h
       This header file is included by kuipList.c

*******************************************************************************/

#ifndef        _KUIPLIST_INCLUDED
#define        _KUIPLIST_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

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
        Widget        UxkuipList;
        swidget        UxlistParent;
} _UxCkuipList;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipList            *UxKuipListContext;
#define kuipList                UxKuipListContext->UxkuipList
#define listParent              UxKuipListContext->UxlistParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_listSBox;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipList();

#endif        /* _KUIPLIST_INCLUDED */
