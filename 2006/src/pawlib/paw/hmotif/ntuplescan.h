/*
 * $Id: ntuplescan.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: ntuplescan.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        ntupleScan.h
        (Generated from interface file ntupleScan.i)
        This header file is included by ntupleScan.c

*******************************************************************************/

#ifndef _NTUPLESCAN_INCLUDED
#define _NTUPLESCAN_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
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
        Widget  UxntupleScan;
        Widget  UxseparatorGadget3;
        Widget  UxseparatorGadget4;
        Widget  UxcloseScButton;
        Widget  UxhelpScButton;
        Widget  Uxform6;
        Widget  UxlabelGadget1;
        Widget  UxfirstScText;
        Widget  UxlabelGadget2;
        Widget  UxnrowScText;
        Widget  UxnextScButton;
        Widget  UxpreviousScButton;
        Widget  UxscanScButton;
        Widget  UxseparatorGadget5;
        Widget  UxcolumnsScLabel;
        Widget  UxtitleScLabel;
} _UxCntupleScan;

#define ntupleScan              UxNtupleScanContext->UxntupleScan
#define separatorGadget3        UxNtupleScanContext->UxseparatorGadget3
#define separatorGadget4        UxNtupleScanContext->UxseparatorGadget4
#define closeScButton           UxNtupleScanContext->UxcloseScButton
#define helpScButton            UxNtupleScanContext->UxhelpScButton
#define form6                   UxNtupleScanContext->Uxform6
#define labelGadget1            UxNtupleScanContext->UxlabelGadget1
#define firstScText             UxNtupleScanContext->UxfirstScText
#define labelGadget2            UxNtupleScanContext->UxlabelGadget2
#define nrowScText              UxNtupleScanContext->UxnrowScText
#define nextScButton            UxNtupleScanContext->UxnextScButton
#define previousScButton        UxNtupleScanContext->UxpreviousScButton
#define scanScButton            UxNtupleScanContext->UxscanScButton
#define separatorGadget5        UxNtupleScanContext->UxseparatorGadget5
#define columnsScLabel          UxNtupleScanContext->UxcolumnsScLabel
#define titleScLabel            UxNtupleScanContext->UxtitleScLabel

static _UxCntupleScan   *UxNtupleScanContext;


#endif  /* _NTUPLESCAN_INCLUDED */
