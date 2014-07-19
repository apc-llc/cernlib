/*
 * $Id: cuteditor.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: cuteditor.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        cutEditor.h
        (Generated from interface file cutEditor.i)
        This header file is included by cutEditor.c

*******************************************************************************/

#ifndef _CUTEDITOR_INCLUDED
#define _CUTEDITOR_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>

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
        Widget  UxcutEditor;
        Widget  Uxmenu1;
        Widget  Uxmenu1_p1;
        Widget  UxopenCeButton;
        Widget  UxsaveCeButton;
        Widget  UxsaveasCeButton;
        Widget  UxcloseCeButton;
        Widget  Uxmenu1_top_b1;
        Widget  Uxmenu1_p3;
        Widget  UxaddBeforeCeButton;
        Widget  UxaddAfterCeButton;
        Widget  Uxmenu1_p3_b7;
        Widget  UxaddBeforeOpenCeButton;
        Widget  UxaddAfterOpenCeButton;
        Widget  UxaddBeforeCloseCeButton;
        Widget  UxaddAfterCloseCeButton;
        Widget  Uxmenu1_p3_b8;
        Widget  UxdeleteCeButton;
        Widget  UxdeleteAllCeButton;
        Widget  Uxmenu1_top_b3;
        Widget  Uxmenu1_p2;
        Widget  UxtreeCeButton;
        Widget  UxdynCeButton;
        Widget  Uxmenu1_p2_b4;
        Widget  UxindentCeButton;
        Widget  Uxmenu1_p2_b6;
        Widget  UxactivateCeButton;
        Widget  UxdeactivateCeButton;
        Widget  Uxmenu1_top_b2;
        Widget  Uxframe1;
        Widget  Uxform1;
        Widget  Uxseparator1;
        Widget  Uxclose2CeButton;
        Widget  Uxseparator2;
        Widget  UxscrolledCeWindow;
        Widget  UxcutsCeForm;
        Widget  UxfullCeLabel;
        Widget  UxapplyCeButton;
        Widget  UxapplyPlotCeButton;
} _UxCcutEditor;

#define cutEditor               UxCutEditorContext->UxcutEditor
#define menu1                   UxCutEditorContext->Uxmenu1
#define menu1_p1                UxCutEditorContext->Uxmenu1_p1
#define openCeButton            UxCutEditorContext->UxopenCeButton
#define saveCeButton            UxCutEditorContext->UxsaveCeButton
#define saveasCeButton          UxCutEditorContext->UxsaveasCeButton
#define closeCeButton           UxCutEditorContext->UxcloseCeButton
#define menu1_top_b1            UxCutEditorContext->Uxmenu1_top_b1
#define menu1_p3                UxCutEditorContext->Uxmenu1_p3
#define addBeforeCeButton       UxCutEditorContext->UxaddBeforeCeButton
#define addAfterCeButton        UxCutEditorContext->UxaddAfterCeButton
#define menu1_p3_b7             UxCutEditorContext->Uxmenu1_p3_b7
#define addBeforeOpenCeButton   UxCutEditorContext->UxaddBeforeOpenCeButton
#define addAfterOpenCeButton    UxCutEditorContext->UxaddAfterOpenCeButton
#define addBeforeCloseCeButton  UxCutEditorContext->UxaddBeforeCloseCeButton
#define addAfterCloseCeButton   UxCutEditorContext->UxaddAfterCloseCeButton
#define menu1_p3_b8             UxCutEditorContext->Uxmenu1_p3_b8
#define deleteCeButton          UxCutEditorContext->UxdeleteCeButton
#define deleteAllCeButton       UxCutEditorContext->UxdeleteAllCeButton
#define menu1_top_b3            UxCutEditorContext->Uxmenu1_top_b3
#define menu1_p2                UxCutEditorContext->Uxmenu1_p2
#define treeCeButton            UxCutEditorContext->UxtreeCeButton
#define dynCeButton             UxCutEditorContext->UxdynCeButton
#define menu1_p2_b4             UxCutEditorContext->Uxmenu1_p2_b4
#define indentCeButton          UxCutEditorContext->UxindentCeButton
#define menu1_p2_b6             UxCutEditorContext->Uxmenu1_p2_b6
#define activateCeButton        UxCutEditorContext->UxactivateCeButton
#define deactivateCeButton      UxCutEditorContext->UxdeactivateCeButton
#define menu1_top_b2            UxCutEditorContext->Uxmenu1_top_b2
#define frame1                  UxCutEditorContext->Uxframe1
#define form1                   UxCutEditorContext->Uxform1
#define separator1              UxCutEditorContext->Uxseparator1
#define close2CeButton          UxCutEditorContext->Uxclose2CeButton
#define separator2              UxCutEditorContext->Uxseparator2
#define scrolledCeWindow        UxCutEditorContext->UxscrolledCeWindow
#define cutsCeForm              UxCutEditorContext->UxcutsCeForm
#define fullCeLabel             UxCutEditorContext->UxfullCeLabel
#define applyCeButton           UxCutEditorContext->UxapplyCeButton
#define applyPlotCeButton       UxCutEditorContext->UxapplyPlotCeButton

static _UxCcutEditor    *UxCutEditorContext;


#endif  /* _CUTEDITOR_INCLUDED */
