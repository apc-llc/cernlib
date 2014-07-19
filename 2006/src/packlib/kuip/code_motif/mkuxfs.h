/*
 * $Id: mkuxfs.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxfs.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipFilSel.h
       This header file is included by kuipFilSel.c

*******************************************************************************/

#ifndef       _KUIPFILSEL_INCLUDED
#define       _KUIPFILSEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/FileSB.h>
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
       Widget       UxkuipFilSel;
       Widget       Uxkm_FilSel;
       swidget       UxfileParent;
} _UxCkuipFilSel;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipFilSel          *UxKuipFilSelContext;
#define kuipFilSel              UxKuipFilSelContext->UxkuipFilSel
#define km_FilSel               UxKuipFilSelContext->Uxkm_FilSel
#define fileParent              UxKuipFilSelContext->UxfileParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget       create_kuipFilSel();

#endif       /* _KUIPFILSEL_INCLUDED */
