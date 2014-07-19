/*
 * $Id: columnselection.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: columnselection.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.05/01 20/03/94  20.32.19  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       columnSelection.c
       (Generated from interface file columnSelection.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
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

typedef struct
{
        Widget  UxcolumnSelection;
        Widget  UxscrolledWindow2;
        Widget  UxscrolledWindow3;
        Widget  Uxlabel1;
        swidget Uxselection_parent;
} _UxCcolumnSelection;

static _UxCcolumnSelection     *UxColumnSelectionContext;
#define columnSelection         UxColumnSelectionContext->UxcolumnSelection
#define scrolledWindow2         UxColumnSelectionContext->UxscrolledWindow2
#define scrolledWindow3         UxColumnSelectionContext->UxscrolledWindow3
#define label1                  UxColumnSelectionContext->Uxlabel1
#define selection_parent        UxColumnSelectionContext->Uxselection_parent


Widget  cancelNtSButton;
Widget  scanNtSButton;
Widget  var1NtSList;
Widget  var2NtSList;
Widget  exprNtSText;
Widget  copyNtSButton;
Widget  copyAllNtSButton;
Widget  deleteNtSButton;
Widget  resetNtSButton;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_columnSelection( swidget _Uxselection_parent );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_columnSelection()
{
        Widget          _UxParent;


        /* Creation of columnSelection */
        _UxParent = selection_parent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "columnSelection_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 530,
                        XmNheight, 370,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "columnSelection",
                        NULL );

        columnSelection = XtVaCreateWidget( "columnSelection",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 530,
                        XmNheight, 370,
                        XmNunitType, XmPIXELS,
                        XmNautoUnmanage, FALSE,
                     RES_CONVERT( XmNdialogTitle, "Column Selection Dialog" ),
                        NULL );
        UxPutContext( columnSelection, (char *) UxColumnSelectionContext );


        /* Creation of cancelNtSButton */
        cancelNtSButton = XtVaCreateManagedWidget( "cancelNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        RES_CONVERT( XmNlabelString, "Cancel" ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNwidth, 110,
                        XmNleftOffset, -60,
                        XmNleftPosition, 50,
                        XmNnavigationType, XmTAB_GROUP,
                        NULL );
        UxPutContext( cancelNtSButton, (char *) UxColumnSelectionContext );


        /* Creation of scanNtSButton */
        scanNtSButton = XtVaCreateManagedWidget( "scanNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomWidget, cancelNtSButton,
                        RES_CONVERT( XmNlabelString, "Scan" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cancelNtSButton,
                        XmNwidth, 110,
                        XmNnavigationType, XmTAB_GROUP,
                        NULL );
        UxPutContext( scanNtSButton, (char *) UxColumnSelectionContext );


        /* Creation of scrolledWindow2 */
        scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
                        xmScrolledWindowWidgetClass,
                        columnSelection,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNrightOffset, 30,
                        XmNrightWidget, cancelNtSButton,
                        XmNnavigationType, XmNONE,
                        NULL );
        UxPutContext( scrolledWindow2, (char *) UxColumnSelectionContext );


        /* Creation of var1NtSList */
        var1NtSList = XtVaCreateManagedWidget( "var1NtSList",
                        xmListWidgetClass,
                        scrolledWindow2,
                        XmNlistSizePolicy, XmCONSTANT,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        NULL );
        UxPutContext( var1NtSList, (char *) UxColumnSelectionContext );


        /* Creation of scrolledWindow3 */
        scrolledWindow3 = XtVaCreateManagedWidget( "scrolledWindow3",
                        xmScrolledWindowWidgetClass,
                        columnSelection,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNleftOffset, 30,
                        XmNleftWidget, cancelNtSButton,
                        XmNnavigationType, XmNONE,
                        NULL );
        UxPutContext( scrolledWindow3, (char *) UxColumnSelectionContext );


        /* Creation of var2NtSList */
        var2NtSList = XtVaCreateManagedWidget( "var2NtSList",
                        xmListWidgetClass,
                        scrolledWindow3,
                        XmNlistSizePolicy, XmCONSTANT,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        NULL );
        UxPutContext( var2NtSList, (char *) UxColumnSelectionContext );


        /* Creation of label1 */
        label1 = XtVaCreateManagedWidget( "label1",
                        xmLabelWidgetClass,
                        columnSelection,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Column Expression:" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 3,
                        XmNleftWidget, scrolledWindow2,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 3,
                        XmNrightWidget, scrolledWindow3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( label1, (char *) UxColumnSelectionContext );


        /* Creation of exprNtSText */
        exprNtSText = XtVaCreateManagedWidget( "exprNtSText",
                        xmTextWidgetClass,
                        columnSelection,
                        XmNheight, 35,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 3,
                        XmNleftWidget, scrolledWindow2,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 3,
                        XmNrightWidget, scrolledWindow3,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, label1,
                        NULL );
        UxPutContext( exprNtSText, (char *) UxColumnSelectionContext );


        /* Creation of copyNtSButton */
        copyNtSButton = XtVaCreateManagedWidget( "copyNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNwidth, 110,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, ">> Copy >>" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cancelNtSButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 15,
                        XmNnavigationType, XmTAB_GROUP,
                        XmNtopWidget, exprNtSText,
                        NULL );
        UxPutContext( copyNtSButton, (char *) UxColumnSelectionContext );


        /* Creation of copyAllNtSButton */
        copyAllNtSButton = XtVaCreateManagedWidget( "copyAllNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNwidth, 110,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, ">> Copy All >>" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cancelNtSButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, copyNtSButton,
                        XmNnavigationType, XmTAB_GROUP,
                        NULL );
        UxPutContext( copyAllNtSButton, (char *) UxColumnSelectionContext );


        /* Creation of deleteNtSButton */
        deleteNtSButton = XtVaCreateManagedWidget( "deleteNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNwidth, 110,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Delete" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cancelNtSButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, copyAllNtSButton,
                        NULL );
        UxPutContext( deleteNtSButton, (char *) UxColumnSelectionContext );


        /* Creation of resetNtSButton */
        resetNtSButton = XtVaCreateManagedWidget( "resetNtSButton",
                        xmPushButtonWidgetClass,
                        columnSelection,
                        XmNwidth, 110,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cancelNtSButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, deleteNtSButton,
                        XmNnavigationType, XmTAB_GROUP,
                        NULL );
        UxPutContext( resetNtSButton, (char *) UxColumnSelectionContext );

        XtVaSetValues(columnSelection,
                        XmNcancelButton, cancelNtSButton,
                        XmNdefaultButton, scanNtSButton,
                        NULL );


        XtAddCallback( columnSelection, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxColumnSelectionContext);


        return ( columnSelection );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_columnSelection( swidget _Uxselection_parent )
{
        Widget                  rtrn;
        _UxCcolumnSelection     *UxContext;

        UxColumnSelectionContext = UxContext =
  (_UxCcolumnSelection *) UxNewContext( sizeof(_UxCcolumnSelection), False );

        selection_parent = _Uxselection_parent;

        rtrn = _Uxbuild_columnSelection();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

