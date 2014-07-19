/*
 * $Id: fileselpanel.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: fileselpanel.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.35.27  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       fileselpanel.h
       This header file is included by fileselpanel.c

*******************************************************************************/

#ifndef _FILESELPANEL_INCLUDED
#define _FILESELPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/FileSB.h>
#include <X11/Shell.h>

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

typedef struct
{
        Widget  UxfileSelectionShell;
        Widget  UxfileSelectionBox;
        void    *Uxmainctxt;
        swidget UxUxParent;
} _UxCfileSelectionShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCfileSelectionShell  *UxFileSelectionShellContext;
#define fileSelectionShell      \
UxFileSelectionShellContext->UxfileSelectionShell
#define fileSelectionBox        UxFileSelectionShellContext->UxfileSelectionBox
#define mainctxt                UxFileSelectionShellContext->Uxmainctxt
#define UxParent                UxFileSelectionShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_fileSelectionPanel( swidget _UxUxParent );

#endif  /* _FILESELPANEL_INCLUDED */
