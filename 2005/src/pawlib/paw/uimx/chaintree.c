/*
 * $Id: chaintree.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: chaintree.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        chainTree.c
        (Generated from interface file chainTree.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushB.h>
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
        Widget  UxchainTree;
        Widget  UxseparatorGadget2;
} _UxCchainTree;

#define chainTree               UxChainTreeContext->UxchainTree
#define separatorGadget2        UxChainTreeContext->UxseparatorGadget2

static _UxCchainTree    *UxChainTreeContext;

Widget  closeChButton;
Widget  treeChWindow;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_chainTree();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_chainTree()
{
        Widget  chainTree_shell;

        chainTree_shell = XtVaCreatePopupShell( "chainTree_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 86,
                        XmNy, 95,
                        XmNwidth, 458,
                        XmNheight, 580,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "chainTree",
                        XmNiconName, "chainTree",
                        NULL );

        chainTree = XtVaCreateManagedWidget( "chainTree",
                        xmFormWidgetClass, chainTree_shell,
                        RES_CONVERT( XmNdialogTitle, "Chain Tree" ),
                        XmNheight, 580,
                        XmNwidth, 458,
                        XmNunitType, XmPIXELS,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( chainTree, (char *) UxChainTreeContext );

        closeChButton = XtVaCreateManagedWidget( "closeChButton",
                        xmPushButtonWidgetClass, chainTree,
                        XmNrightPosition, 60,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNwidth, 100,
                        XmNrightAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        NULL );

        UxPutContext( closeChButton, (char *) UxChainTreeContext );

        separatorGadget2 = XtVaCreateManagedWidget( "separatorGadget2",
                        xmSeparatorGadgetClass, chainTree,
                        XmNbottomWidget, closeChButton,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget2, (char *) UxChainTreeContext );

        treeChWindow = XtVaCreateManagedWidget( "treeChWindow",
                        xmScrolledWindowWidgetClass, chainTree,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separatorGadget2,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNscrollingPolicy, XmAUTOMATIC,
                        NULL );

        UxPutContext( treeChWindow, (char *) UxChainTreeContext );

        XtAddCallback( chainTree, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxChainTreeContext );



        return ( chainTree );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_chainTree()
{
        Widget                  rtrn;
        _UxCchainTree           *UxContext;

        UxChainTreeContext = UxContext =
                (_UxCchainTree *) XtMalloc( sizeof(_UxCchainTree) );

        rtrn = _Uxbuild_chainTree();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_chainTree()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_chainTree();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

