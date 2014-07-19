/*
 * $Id: mkuxse.h,v 1.1.1.1 1996/03/08 15:33:13 mclareni Exp $
 *
 * $Log: mkuxse.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:13  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipSelect.h
       This header file is included by kuipSelect.c

*******************************************************************************/

#ifndef        _KUIPSELECT_INCLUDED
#define        _KUIPSELECT_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/PushBG.h>
#include <Xm/MessageB.h>
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
        Widget        UxkuipSelect;
        swidget        UxUxParent;
} _UxCkuipSelect;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipSelect          *UxKuipSelectContext;
#define kuipSelect              UxKuipSelectContext->UxkuipSelect
#define UxParent                UxKuipSelectContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_messageBox;
extern Widget        km_goButton;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipSelect();

#endif        /* _KUIPSELECT_INCLUDED */
