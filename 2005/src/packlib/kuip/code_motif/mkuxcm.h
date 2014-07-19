/*
 * $Id: mkuxcm.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxcm.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipCommand.h
       This header file is included by kuipCommand.c

*******************************************************************************/

#ifndef        _KUIPCOMMAND_INCLUDED
#define        _KUIPCOMMAND_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

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
        Widget        UxkuipCommandP;
        Widget        UxcommandSep;
} _UxCkuipCommandP;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipCommandP        *UxKuipCommandPContext;
#define kuipCommandP            UxKuipCommandPContext->UxkuipCommandP
#define commandSep              UxKuipCommandPContext->UxcommandSep

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget        km_cmdOK;
extern Widget        km_cmdEX;
extern Widget        km_cmdRE;
extern Widget        km_cmdCA;
extern Widget        km_cmdHE;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipCommand();

#endif        /* _KUIPCOMMAND_INCLUDED */
