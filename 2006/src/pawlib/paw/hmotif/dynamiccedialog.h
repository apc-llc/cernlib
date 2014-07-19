/*
 * $Id: dynamiccedialog.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: dynamiccedialog.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        dynamicCeDialog.h
        (Generated from interface file dynamicCeDialog.i)
        This header file is included by dynamicCeDialog.c

*******************************************************************************/

#ifndef _DYNAMICCEDIALOG_INCLUDED
#define _DYNAMICCEDIALOG_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/Scale.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/Form.h>

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
        Widget  UxdynamicCeDialog;
        Widget  UxDyForm2;
        Widget  UxDyLabel1;
        Widget  UxpercentDyText;
        Widget  UxapplyDyButton;
        Widget  UxresetDyButton;
        Widget  UxcloseDyButton;
        Widget  UxDySeparator3;
        Widget  UxDyFrame2;
        Widget  UxDyRowColumn1;
        Widget  UxpercentDyToggle;
        Widget  UxabsoluteDyToggle;
        Widget  UxdynamicDyScale;
        Widget  UxcutDyLabel;
        swidget Uxdyn_parent;
} _UxCdynamicCeDialog;

#define dynamicCeDialog         UxDynamicCeDialogContext->UxdynamicCeDialog
#define DyForm2                 UxDynamicCeDialogContext->UxDyForm2
#define DyLabel1                UxDynamicCeDialogContext->UxDyLabel1
#define percentDyText           UxDynamicCeDialogContext->UxpercentDyText
#define applyDyButton           UxDynamicCeDialogContext->UxapplyDyButton
#define resetDyButton           UxDynamicCeDialogContext->UxresetDyButton
#define closeDyButton           UxDynamicCeDialogContext->UxcloseDyButton
#define DySeparator3            UxDynamicCeDialogContext->UxDySeparator3
#define DyFrame2                UxDynamicCeDialogContext->UxDyFrame2
#define DyRowColumn1            UxDynamicCeDialogContext->UxDyRowColumn1
#define percentDyToggle         UxDynamicCeDialogContext->UxpercentDyToggle
#define absoluteDyToggle        UxDynamicCeDialogContext->UxabsoluteDyToggle
#define dynamicDyScale          UxDynamicCeDialogContext->UxdynamicDyScale
#define cutDyLabel              UxDynamicCeDialogContext->UxcutDyLabel
#define dyn_parent              UxDynamicCeDialogContext->Uxdyn_parent

static _UxCdynamicCeDialog      *UxDynamicCeDialogContext;


#endif  /* _DYNAMICCEDIALOG_INCLUDED */
