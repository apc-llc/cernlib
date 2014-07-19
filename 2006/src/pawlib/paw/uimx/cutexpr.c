/*
 * $Id: cutexpr.c,v 1.2 1996/05/03 13:01:39 cremel Exp $
 *
 * $Log: cutexpr.c,v $
 * Revision 1.2  1996/05/03 13:01:39  cremel
 * cuts editor: add operator '*'
 *
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 13/03/94  15.48.50  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       cutExpr.c
       (Generated from interface file cutExpr.ii)
       Associated Header file: cutExpr.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/Form.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "hmotif/cutexpr.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_cutExpr()
{
        Widget          operatorMenu_p_shell;
        Widget          logicalMenu_p_shell;

        /* Creation of cutExpression */
        cutExpression = XtVaCreateManagedWidget( "cutExpression",
                        xmFormWidgetClass,
                        cut_parent,
                        XmNwidth, 645,
                        XmNx, 25,
                        XmNy, 25,
                        NULL );
        UxPutContext( cutExpression, (char *) UxCutExprContext );


        /* Creation of activeCeToggle */
        activeCeToggle = XtVaCreateManagedWidget( "activeCeToggle",
                        xmToggleButtonWidgetClass,
                        cutExpression,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNbottomOffset, 5,
                        NULL );
        UxPutContext( activeCeToggle, (char *) UxCutExprContext );


        /* Creation of countCeLabel */
        countCeLabel = XtVaCreateManagedWidget( "countCeLabel",
                        xmLabelWidgetClass,
                        cutExpression,
                        XmNwidth, 30,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "1" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNrecomputeSize, FALSE,
                        XmNleftWidget, activeCeToggle,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        NULL );
        UxPutContext( countCeLabel, (char *) UxCutExprContext );


        /* Creation of notCeButton */
        notCeButton = XtVaCreateManagedWidget( "notCeButton",
                        xmPushButtonWidgetClass,
                        cutExpression,
                        XmNwidth, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, countCeLabel,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNrecomputeSize, FALSE,
                        XmNbottomOffset, 5,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( notCeButton, (char *) UxCutExprContext );


        /* Creation of bracketCeLabel */
        bracketCeLabel = XtVaCreateManagedWidget( "bracketCeLabel",
                        xmLabelWidgetClass,
                        cutExpression,
                        XmNwidth, 40,
                        XmNheight, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, notCeButton,
                        XmNrecomputeSize, FALSE,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( bracketCeLabel, (char *) UxCutExprContext );


        /* Creation of expr1CeText */
        expr1CeText = XtVaCreateManagedWidget( "expr1CeText",
                        xmTextWidgetClass,
                        cutExpression,
                        XmNheight, 30,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNcolumns, 20,
                        XmNleftWidget, bracketCeLabel,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        NULL );
        UxPutContext( expr1CeText, (char *) UxCutExprContext );


        /* Creation of operatorMenu_p */
        operatorMenu_p_shell = XtVaCreatePopupShell ("operatorMenu_p_shell",
                        xmMenuShellWidgetClass, cutExpression,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        operatorMenu_p = XtVaCreateWidget( "operatorMenu_p",
                        xmRowColumnWidgetClass,
                        operatorMenu_p_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( operatorMenu_p, (char *) UxCutExprContext );


        /* Creation of operatorNULL */
        operatorNULL = XtVaCreateManagedWidget( "operatorNULL",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, " " ),
                        NULL );
        UxPutContext( operatorNULL, (char *) UxCutExprContext );


        /* Creation of operatorEQ */
        operatorEQ = XtVaCreateManagedWidget( "operatorEQ",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "=" ),
                        NULL );
        UxPutContext( operatorEQ, (char *) UxCutExprContext );


        /* Creation of operatorNE */
        operatorNE = XtVaCreateManagedWidget( "operatorNE",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "<>" ),
                        NULL );
        UxPutContext( operatorNE, (char *) UxCutExprContext );


        /* Creation of operatorGT */
        operatorGT = XtVaCreateManagedWidget( "operatorGT",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, ">" ),
                        NULL );
        UxPutContext( operatorGT, (char *) UxCutExprContext );


        /* Creation of operatorLT */
        operatorLT = XtVaCreateManagedWidget( "operatorLT",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "<" ),
                        NULL );
        UxPutContext( operatorLT, (char *) UxCutExprContext );


        /* Creation of operatorGE */
        operatorGE = XtVaCreateManagedWidget( "operatorGE",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, ">=" ),
                        NULL );
        UxPutContext( operatorGE, (char *) UxCutExprContext );


        /* Creation of operatorLE */
        operatorLE = XtVaCreateManagedWidget( "operatorLE",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "<=" ),
                        NULL );
        UxPutContext( operatorLE, (char *) UxCutExprContext );


        /* Creation of operatorCT */
        operatorCT = XtVaCreateManagedWidget( "operatorCT",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "#" ),
                        NULL );
        UxPutContext( operatorCT, (char *) UxCutExprContext );


        /* Creation of operatorW */
        operatorW = XtVaCreateManagedWidget( "operatorW",
                        xmPushButtonGadgetClass,
                        operatorMenu_p,
                        RES_CONVERT( XmNlabelString, "W" ),
                        NULL );
        UxPutContext( operatorW, (char *) UxCutExprContext );


        /* Creation of operatorMenu */
        operatorMenu = XtVaCreateManagedWidget( "operatorMenu",
                        xmRowColumnWidgetClass,
                        cutExpression,
                        XmNheight, 30,
                        XmNborderWidth, 0,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNspacing, 0,
                        XmNsubMenuId, operatorMenu_p,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, expr1CeText,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        NULL );
        UxPutContext( operatorMenu, (char *) UxCutExprContext );


        /* Creation of expr2CeText */
        expr2CeText = XtVaCreateManagedWidget( "expr2CeText",
                        xmTextWidgetClass,
                        cutExpression,
                        XmNheight, 30,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNleftOffset, 5,
                        XmNleftWidget, operatorMenu,
                        XmNcolumns, 20,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        NULL );
        UxPutContext( expr2CeText, (char *) UxCutExprContext );


        /* Creation of logicalMenu_p */
        logicalMenu_p_shell = XtVaCreatePopupShell ("logicalMenu_p_shell",
                        xmMenuShellWidgetClass, cutExpression,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        logicalMenu_p = XtVaCreateWidget( "logicalMenu_p",
                        xmRowColumnWidgetClass,
                        logicalMenu_p_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( logicalMenu_p, (char *) UxCutExprContext );


        /* Creation of logicalNULL */
        logicalNULL = XtVaCreateManagedWidget( "logicalNULL",
                        xmPushButtonGadgetClass,
                        logicalMenu_p,
                        RES_CONVERT( XmNlabelString, " " ),
                        NULL );
        UxPutContext( logicalNULL, (char *) UxCutExprContext );


        /* Creation of logicalAND */
        logicalAND = XtVaCreateManagedWidget( "logicalAND",
                        xmPushButtonGadgetClass,
                        logicalMenu_p,
                        RES_CONVERT( XmNlabelString, ".AND." ),
                        NULL );
        UxPutContext( logicalAND, (char *) UxCutExprContext );


        /* Creation of logicalOR */
        logicalOR = XtVaCreateManagedWidget( "logicalOR",
                        xmPushButtonGadgetClass,
                        logicalMenu_p,
                        RES_CONVERT( XmNlabelString, ".OR." ),
                        NULL );
        UxPutContext( logicalOR, (char *) UxCutExprContext );


        /* Creation of logicalMULT */
        logicalMULT = XtVaCreateManagedWidget( "logicalMULT",
                        xmPushButtonGadgetClass,
                        logicalMenu_p,
                        RES_CONVERT( XmNlabelString, "*" ),
                        NULL );
        UxPutContext( logicalMULT, (char *) UxCutExprContext );


        /* Creation of logicalMenu */
        logicalMenu = XtVaCreateManagedWidget( "logicalMenu",
                        xmRowColumnWidgetClass,
                        cutExpression,
                        XmNheight, 30,
                        XmNborderWidth, 0,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNspacing, 0,
                        XmNsubMenuId, logicalMenu_p,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, expr2CeText,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNtopOffset, 5,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );
        UxPutContext( logicalMenu, (char *) UxCutExprContext );


        XtAddCallback( cutExpression, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxCutExprContext);


        return ( cutExpression );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_cutExpr( swidget _Uxcut_parent )
{
        Widget                  rtrn;
        _UxCcutExpr             *UxContext;

        UxCutExprContext = UxContext =
                (_UxCcutExpr *) UxNewContext( sizeof(_UxCcutExpr), False );

        cut_parent = _Uxcut_parent;

        rtrn = _Uxbuild_cutExpr();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

