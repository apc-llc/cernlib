/*
 * $Id: closefbdialog.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: closefbdialog.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        closeFbDialog.h
        (Generated from interface file closeFbDialog.i)
        This header file is included by closeFbDialog.c

*******************************************************************************/

#ifndef _CLOSEFBDIALOG_INCLUDED
#define _CLOSEFBDIALOG_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/SelectioB.h>

/*******************************************************************************
        The definition of the context structure:
        If you create multiple instances of your interface, the context
        structure ensures that your callbacks use the variables for the
        correct instance.

        For each swidget in the interface, each argument to the Interface
        function, and each variable in the Instance Specific section of the
        Declarations Editor, there is an entry in the context structure.
        and a #define.  The #define makes the variable name refer to the
        corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxcloseFbDialog;
        swidget Uxclose_parent;
} _UxCcloseFbDialog;

#define closeFbDialog           UxCloseFbDialogContext->UxcloseFbDialog
#define close_parent            UxCloseFbDialogContext->Uxclose_parent

static _UxCcloseFbDialog        *UxCloseFbDialogContext;


#endif  /* _CLOSEFBDIALOG_INCLUDED */
