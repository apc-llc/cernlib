/*
 * $Id: openfbdialog.h,v 1.1.1.1 1996/03/01 11:39:13 mclareni Exp $
 *
 * $Log: openfbdialog.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:13  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        openFbDialog.h
        (Generated from interface file openFbDialog.i)
        This header file is included by openFbDialog.c

*******************************************************************************/

#ifndef _OPENFBDIALOG_INCLUDED
#define _OPENFBDIALOG_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/FileSB.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
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
        Widget  UxopenFbDialog;
        Widget  Uxframe1;
        Widget  UxrowColumn1;
        Widget  UxfileTogRo;
        Widget  UxfileTogUp;
        Widget  UxfileTogNew;
        Widget  UxseparatorGadget1;
        Widget  UxfileSelBox;
        Widget  Uxform1;
        Widget  UxrecordLength;
        Widget  UxscrolledWindow2;
        Widget  UxrecordLengthList;
        Widget  UxlabelGadget2;
        Widget  UxscrolledWindow3;
        Widget  UxfileUnits;
        Widget  UxlabelGadget1;
        swidget Uxfile_parent;
} _UxCopenFbDialog;

#define openFbDialog            UxOpenFbDialogContext->UxopenFbDialog
#define frame1                  UxOpenFbDialogContext->Uxframe1
#define rowColumn1              UxOpenFbDialogContext->UxrowColumn1
#define fileTogRo               UxOpenFbDialogContext->UxfileTogRo
#define fileTogUp               UxOpenFbDialogContext->UxfileTogUp
#define fileTogNew              UxOpenFbDialogContext->UxfileTogNew
#define separatorGadget1        UxOpenFbDialogContext->UxseparatorGadget1
#define fileSelBox              UxOpenFbDialogContext->UxfileSelBox
#define form1                   UxOpenFbDialogContext->Uxform1
#define recordLength            UxOpenFbDialogContext->UxrecordLength
#define scrolledWindow2         UxOpenFbDialogContext->UxscrolledWindow2
#define recordLengthList        UxOpenFbDialogContext->UxrecordLengthList
#define labelGadget2            UxOpenFbDialogContext->UxlabelGadget2
#define scrolledWindow3         UxOpenFbDialogContext->UxscrolledWindow3
#define fileUnits               UxOpenFbDialogContext->UxfileUnits
#define labelGadget1            UxOpenFbDialogContext->UxlabelGadget1
#define file_parent             UxOpenFbDialogContext->Uxfile_parent

static _UxCopenFbDialog *UxOpenFbDialogContext;


#endif  /* _OPENFBDIALOG_INCLUDED */
