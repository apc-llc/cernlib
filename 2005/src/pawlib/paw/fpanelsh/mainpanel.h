/*
 * $Id: mainpanel.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: mainpanel.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.38.39  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       mainpanel.h
       This header file is included by mainpanel.c

*******************************************************************************/

#ifndef _MAINPANEL_INCLUDED
#define _MAINPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/CascadeBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/Frame.h>
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
        Widget  UxmainPanelShell;
        Widget  UxmainPanelFrame;
        Widget  UxmainWindow;
        Widget  UxmainForm;
        Widget  UxerrordefLabel;
        Widget  UxerrordefText;
        Widget  UxtoleranceLabel;
        Widget  UxtoleranceText;
        Widget  UxdevsLabel;
        Widget  UxdevsText;
        Widget  UxsetSubSeparator;
        Widget  UxoptionMenu_p3;
        Widget  UxoptionMenu_p3_b2;
        Widget  UxoptionMenu_p3_b3;
        Widget  UxoptionMenu_p3_b4;
        Widget  UxstrategyMenu;
        Widget  UxoptionMenu_p4;
        Widget  UxoptionMenu_p4_b2;
        Widget  UxoptionMenu_p4_b3;
        Widget  UxoptionMenu_p4_b4;
        Widget  UxoptionMenu_p4_b5;
        Widget  UxoptionMenu_p4_b6;
        Widget  UxprintoutMenu;
        Widget  UxupdateModeMenu_p1;
        Widget  UxupdateModeMenu_p1_b1;
        Widget  UxupdateModeMenu_p1_b2;
        Widget  UxupdateModeMenu_p1_b3;
        Widget  UxupdateModeMenu;
        Widget  UxsetZoneSeparator;
        Widget  UxoptnLabel;
        Widget  UxoptnText;
        Widget  UxhistoSeparator;
        Widget  UxffuncLabel;
        Widget  UxffuncText;
        Widget  UxnparLabel;
        Widget  UxnparText;
        Widget  UxfmodelLabel;
        Widget  UxfmodelText;
        Widget  UxffuncSeparator;
        Widget  UxfvalLabel;
        Widget  UxfvalText;
        Widget  UxfminLabel;
        Widget  UxfminText;
        Widget  UxfedmLabel;
        Widget  UxfedmText;
        Widget  UxstatSeparator;
        Widget  UxparZoneLabel;
        Widget  UxparScrolledWindow;
        Widget  UxparForm;
        Widget  UxhfMenuBar;
        Widget  UxfileMenu;
        Widget  UxfileMenu_save;
        Widget  UxfileMenu_input;
        Widget  UxfileMenu_store;
        Widget  UxfileMenu_recall;
        Widget  UxfileMenu_start;
        Widget  UxfileMenu_exit;
        Widget  UxfileMenu_top_b;
        Widget  UxminimizeMenu;
        Widget  UxminimizeMenu_b1;
        Widget  UxminimizeMenu1_b2;
        Widget  UxminimizeMenu1_b3;
        Widget  UxMinimizeMenu1_b4;
        Widget  UxminimizeMenu1_b5;
        Widget  UxminimizeMenu_top_b;
        Widget  UxexamineMenu;
        Widget  UxexamineMenu_b1;
        Widget  UxexamineMenu_b2;
        Widget  UxexamineMenu_b3;
        Widget  UxexamineMenu_b4;
        Widget  UxexamineMenu_b5;
        Widget  UxexamineMenu_b6;
        Widget  UxexamineMenu_top_b;
        Widget  UxscanMenu;
        Widget  UxscanMenu1_b1;
        Widget  UxscanMenu1_b2;
        Widget  UxscanMenu_top_b;
        Widget  UxtransMenu;
        Widget  UxtransMenu_vect;
        Widget  UxtransMenu_top_b;
        Widget  UxviewMenu;
        Widget  UxviewMenuLimits;
        Widget  UxviewMenuSteps;
        Widget  UxviewMenuEparab;
        Widget  UxviewMenuEminos;
        Widget  UxviewMenuGlobcc;
        Widget  UxviewMenu_top_b;
        Widget  UxhelpMenu;
        Widget  UxhelpMenu_panel;
        Widget  UxhelpMenu_minuit;
        Widget  UxhelpMenu_context;
        Widget  UxhelpMenu_top_b;
        int     UxendPanelEventLoop;
        int     UxinterrCompute;
        void    (*Uxgrupdate)(void *);
        int     UxpanelState;
        int     Uxhistodim;
        int     Uxhistonx;
        int     Uxhistony;
        Widget  UxhistoIdLabel;
        Widget  UxhistoIdText;
        Widget  UxnbinLabel;
        Widget  UxrangeLabel;
        Widget  UxrangeText;
        Widget  UxxvectLabel;
        Widget  UxxvectText;
        Widget  UxyvectLabel;
        Widget  UxyvectText;
        Widget  UxeyvectLabel;
        Widget  UxeyvectText;
        Widget  UxfileSelPanel;
        Widget  UxcontourPanel;
        Widget  UxscanPanel;
        Widget  UxvectPanel;
        Widget  UxparHeader;
        Widget  *UxparLines;
        Widget  UxclipWindow;
        Widget  UxhorlScrollBar;
        Widget  UxvertScrollBar;
        Dimension       UxscaleMinWidth;
        int     Uxstrategy;
        int     Uxprintout;
        int     Uxupdatemode;
        double  Uxerrordef;
        double  Uxtolerance;
        double  Uxdevs;
        Boolean Uxeparab_valid;
        Boolean Uxeminos_valid;
        Boolean Uxglobcc_valid;
        Boolean Uxlimits_shown;
        Boolean Uxsteps_shown;
        Boolean Uxeparab_shown;
        Boolean Uxeminos_shown;
        Boolean Uxglobcc_shown;
        TextStatusInfo  UxerrordefStat;
        TextStatusInfo  UxtoleranceStat;
        TextStatusInfo  UxdevsStat;
        TextStatusInfo  UxhistoIdStat;
        TextStatusInfo  UxrangeStat;
        TextStatusInfo  UxxvectStat;
        TextStatusInfo  UxyvectStat;
        TextStatusInfo  UxeyvectStat;
        TextStatusInfo  UxoptnStat;
        TextStatusInfo  UxffuncStat;
        TextStatusInfo  UxnparStat;
        TextStatusInfo  UxfmodelStat;
        Boolean UxvertScrollBarOn;
        Boolean UxhorlScrollBarOn;
        Dimension       UxwVertScrollBar;
        Dimension       UxhHorlScrollBar;
        Dimension       UxdScrolledWinSpacing;
        Dimension       UxdScrolledWinShadow;
        Dimension       UxparFormWid;
        Dimension       UxparFormHei;
        Dimension       UxparFormWidPix;
        Dimension       UxparFormHeiPix;
        Dimension       UxhNameText;
        Dimension       UxhBotSeparator;
        Dimension       UxhTopSeparator;
        Dimension       UxwActiveToggle;
        Dimension       UxwNumberValue;
        Dimension       UxwNameText;
        Dimension       UxwScale;
        Dimension       UxwScaleAdjustButton;
        Dimension       UxwValueText;
        Dimension       UxwFixToggle;
        Dimension       UxwLowLimitText;
        Dimension       UxwUppLimitText;
        Dimension       UxwStepText;
        Dimension       UxwEparabText;
        Dimension       UxwEminusText;
        Dimension       UxwEplusText;
        Dimension       UxwGlobccText;
        Dimension       UxwScaleMinMaxForm;
        Position        UxxScaleMinMax;
        int     Uxhistid;
        int     Uxlowbins[3];
        int     Uxuppbins[3];
        char    Uxoptn[30];
        char    Uxfexpr[30];
        char    Uxfmodel[30];
        void    *Uxfcn;
        void    *Uxufcn;
        int     Uxnpar;
        char    **Uxparname;
        double  *Uxvpar;
        double  *Uxvlow;
        double  *Uxvupp;
        double  *Uxvstep;
        double  *Uxveparab;
        double  *Uxveminus;
        double  *Uxveplus;
        double  *Uxpar_start;
        double  *Uxpar_store;
        int     *Uxpar_active;
        int     *Uxvfixrel;
        int     *Uxpar_enabled;
        double  *Uxvscalemin;
        double  *Uxvscalemax;
        int     Uxcont_num1;
        int     Uxcont_num2;
        int     Uxcont_npt;
        int     Uxcont_nfound;
        double  *Uxcont_xpt;
        double  *Uxcont_ypt;
        int     Uxcont_overlay;
        char    Uxcont_xvecnam[31];
        char    Uxcont_yvecnam[31];
        int     Uxscan_num;
        int     Uxscan_npt;
        double  Uxscan_xfrom;
        double  Uxscan_xto;
        double  *Uxscan_xpt;
        double  *Uxscan_ypt;
        char    Uxvec_pnam[31];
        char    Uxvec_snam[31];
        char    Uxvec_lnam[31];
        char    Uxvec_unam[31];
        char    Uxvec_enam[31];
        swidget UxUxParent;
} _UxCmainPanelShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCmainPanelShell      *UxMainPanelShellContext;
#define mainPanelShell          UxMainPanelShellContext->UxmainPanelShell
#define mainPanelFrame          UxMainPanelShellContext->UxmainPanelFrame
#define mainWindow              UxMainPanelShellContext->UxmainWindow
#define mainForm                UxMainPanelShellContext->UxmainForm
#define errordefLabel           UxMainPanelShellContext->UxerrordefLabel
#define errordefText            UxMainPanelShellContext->UxerrordefText
#define toleranceLabel          UxMainPanelShellContext->UxtoleranceLabel
#define toleranceText           UxMainPanelShellContext->UxtoleranceText
#define devsLabel               UxMainPanelShellContext->UxdevsLabel
#define devsText                UxMainPanelShellContext->UxdevsText
#define setSubSeparator         UxMainPanelShellContext->UxsetSubSeparator
#define optionMenu_p3           UxMainPanelShellContext->UxoptionMenu_p3
#define optionMenu_p3_b2        UxMainPanelShellContext->UxoptionMenu_p3_b2
#define optionMenu_p3_b3        UxMainPanelShellContext->UxoptionMenu_p3_b3
#define optionMenu_p3_b4        UxMainPanelShellContext->UxoptionMenu_p3_b4
#define strategyMenu            UxMainPanelShellContext->UxstrategyMenu
#define optionMenu_p4           UxMainPanelShellContext->UxoptionMenu_p4
#define optionMenu_p4_b2        UxMainPanelShellContext->UxoptionMenu_p4_b2
#define optionMenu_p4_b3        UxMainPanelShellContext->UxoptionMenu_p4_b3
#define optionMenu_p4_b4        UxMainPanelShellContext->UxoptionMenu_p4_b4
#define optionMenu_p4_b5        UxMainPanelShellContext->UxoptionMenu_p4_b5
#define optionMenu_p4_b6        UxMainPanelShellContext->UxoptionMenu_p4_b6
#define printoutMenu            UxMainPanelShellContext->UxprintoutMenu
#define updateModeMenu_p1       UxMainPanelShellContext->UxupdateModeMenu_p1
#define updateModeMenu_p1_b1    UxMainPanelShellContext->UxupdateModeMenu_p1_b1
#define updateModeMenu_p1_b2    UxMainPanelShellContext->UxupdateModeMenu_p1_b2
#define updateModeMenu_p1_b3    UxMainPanelShellContext->UxupdateModeMenu_p1_b3
#define updateModeMenu          UxMainPanelShellContext->UxupdateModeMenu
#define setZoneSeparator        UxMainPanelShellContext->UxsetZoneSeparator
#define optnLabel               UxMainPanelShellContext->UxoptnLabel
#define optnText                UxMainPanelShellContext->UxoptnText
#define histoSeparator          UxMainPanelShellContext->UxhistoSeparator
#define ffuncLabel              UxMainPanelShellContext->UxffuncLabel
#define ffuncText               UxMainPanelShellContext->UxffuncText
#define nparLabel               UxMainPanelShellContext->UxnparLabel
#define nparText                UxMainPanelShellContext->UxnparText
#define fmodelLabel             UxMainPanelShellContext->UxfmodelLabel
#define fmodelText              UxMainPanelShellContext->UxfmodelText
#define ffuncSeparator          UxMainPanelShellContext->UxffuncSeparator
#define fvalLabel               UxMainPanelShellContext->UxfvalLabel
#define fvalText                UxMainPanelShellContext->UxfvalText
#define fminLabel               UxMainPanelShellContext->UxfminLabel
#define fminText                UxMainPanelShellContext->UxfminText
#define fedmLabel               UxMainPanelShellContext->UxfedmLabel
#define fedmText                UxMainPanelShellContext->UxfedmText
#define statSeparator           UxMainPanelShellContext->UxstatSeparator
#define parZoneLabel            UxMainPanelShellContext->UxparZoneLabel
#define parScrolledWindow       UxMainPanelShellContext->UxparScrolledWindow
#define parForm                 UxMainPanelShellContext->UxparForm
#define hfMenuBar               UxMainPanelShellContext->UxhfMenuBar
#define fileMenu                UxMainPanelShellContext->UxfileMenu
#define fileMenu_save           UxMainPanelShellContext->UxfileMenu_save
#define fileMenu_input          UxMainPanelShellContext->UxfileMenu_input
#define fileMenu_store          UxMainPanelShellContext->UxfileMenu_store
#define fileMenu_recall         UxMainPanelShellContext->UxfileMenu_recall
#define fileMenu_start          UxMainPanelShellContext->UxfileMenu_start
#define fileMenu_exit           UxMainPanelShellContext->UxfileMenu_exit
#define fileMenu_top_b          UxMainPanelShellContext->UxfileMenu_top_b
#define minimizeMenu            UxMainPanelShellContext->UxminimizeMenu
#define minimizeMenu_b1         UxMainPanelShellContext->UxminimizeMenu_b1
#define minimizeMenu1_b2        UxMainPanelShellContext->UxminimizeMenu1_b2
#define minimizeMenu1_b3        UxMainPanelShellContext->UxminimizeMenu1_b3
#define MinimizeMenu1_b4        UxMainPanelShellContext->UxMinimizeMenu1_b4
#define minimizeMenu1_b5        UxMainPanelShellContext->UxminimizeMenu1_b5
#define minimizeMenu_top_b      UxMainPanelShellContext->UxminimizeMenu_top_b
#define examineMenu             UxMainPanelShellContext->UxexamineMenu
#define examineMenu_b1          UxMainPanelShellContext->UxexamineMenu_b1
#define examineMenu_b2          UxMainPanelShellContext->UxexamineMenu_b2
#define examineMenu_b3          UxMainPanelShellContext->UxexamineMenu_b3
#define examineMenu_b4          UxMainPanelShellContext->UxexamineMenu_b4
#define examineMenu_b5          UxMainPanelShellContext->UxexamineMenu_b5
#define examineMenu_b6          UxMainPanelShellContext->UxexamineMenu_b6
#define examineMenu_top_b       UxMainPanelShellContext->UxexamineMenu_top_b
#define scanMenu                UxMainPanelShellContext->UxscanMenu
#define scanMenu1_b1            UxMainPanelShellContext->UxscanMenu1_b1
#define scanMenu1_b2            UxMainPanelShellContext->UxscanMenu1_b2
#define scanMenu_top_b          UxMainPanelShellContext->UxscanMenu_top_b
#define transMenu               UxMainPanelShellContext->UxtransMenu
#define transMenu_vect          UxMainPanelShellContext->UxtransMenu_vect
#define transMenu_top_b         UxMainPanelShellContext->UxtransMenu_top_b
#define viewMenu                UxMainPanelShellContext->UxviewMenu
#define viewMenuLimits          UxMainPanelShellContext->UxviewMenuLimits
#define viewMenuSteps           UxMainPanelShellContext->UxviewMenuSteps
#define viewMenuEparab          UxMainPanelShellContext->UxviewMenuEparab
#define viewMenuEminos          UxMainPanelShellContext->UxviewMenuEminos
#define viewMenuGlobcc          UxMainPanelShellContext->UxviewMenuGlobcc
#define viewMenu_top_b          UxMainPanelShellContext->UxviewMenu_top_b
#define helpMenu                UxMainPanelShellContext->UxhelpMenu
#define helpMenu_panel          UxMainPanelShellContext->UxhelpMenu_panel
#define helpMenu_minuit         UxMainPanelShellContext->UxhelpMenu_minuit
#define helpMenu_context        UxMainPanelShellContext->UxhelpMenu_context
#define helpMenu_top_b          UxMainPanelShellContext->UxhelpMenu_top_b
#define endPanelEventLoop       UxMainPanelShellContext->UxendPanelEventLoop
#define interrCompute           UxMainPanelShellContext->UxinterrCompute
#define grupdate                UxMainPanelShellContext->Uxgrupdate
#define panelState              UxMainPanelShellContext->UxpanelState
#define histodim                UxMainPanelShellContext->Uxhistodim
#define histonx                 UxMainPanelShellContext->Uxhistonx
#define histony                 UxMainPanelShellContext->Uxhistony
#define histoIdLabel            UxMainPanelShellContext->UxhistoIdLabel
#define histoIdText             UxMainPanelShellContext->UxhistoIdText
#define nbinLabel               UxMainPanelShellContext->UxnbinLabel
#define rangeLabel              UxMainPanelShellContext->UxrangeLabel
#define rangeText               UxMainPanelShellContext->UxrangeText
#define xvectLabel              UxMainPanelShellContext->UxxvectLabel
#define xvectText               UxMainPanelShellContext->UxxvectText
#define yvectLabel              UxMainPanelShellContext->UxyvectLabel
#define yvectText               UxMainPanelShellContext->UxyvectText
#define eyvectLabel             UxMainPanelShellContext->UxeyvectLabel
#define eyvectText              UxMainPanelShellContext->UxeyvectText
#define fileSelPanel            UxMainPanelShellContext->UxfileSelPanel
#define contourPanel            UxMainPanelShellContext->UxcontourPanel
#define scanPanel               UxMainPanelShellContext->UxscanPanel
#define vectPanel               UxMainPanelShellContext->UxvectPanel
#define parHeader               UxMainPanelShellContext->UxparHeader
#define parLines                UxMainPanelShellContext->UxparLines
#define clipWindow              UxMainPanelShellContext->UxclipWindow
#define horlScrollBar           UxMainPanelShellContext->UxhorlScrollBar
#define vertScrollBar           UxMainPanelShellContext->UxvertScrollBar
#define scaleMinWidth           UxMainPanelShellContext->UxscaleMinWidth
#define strategy                UxMainPanelShellContext->Uxstrategy
#define printout                UxMainPanelShellContext->Uxprintout
#define updatemode              UxMainPanelShellContext->Uxupdatemode
#define errordef                UxMainPanelShellContext->Uxerrordef
#define tolerance               UxMainPanelShellContext->Uxtolerance
#define devs                    UxMainPanelShellContext->Uxdevs
#define eparab_valid            UxMainPanelShellContext->Uxeparab_valid
#define eminos_valid            UxMainPanelShellContext->Uxeminos_valid
#define globcc_valid            UxMainPanelShellContext->Uxglobcc_valid
#define limits_shown            UxMainPanelShellContext->Uxlimits_shown
#define steps_shown             UxMainPanelShellContext->Uxsteps_shown
#define eparab_shown            UxMainPanelShellContext->Uxeparab_shown
#define eminos_shown            UxMainPanelShellContext->Uxeminos_shown
#define globcc_shown            UxMainPanelShellContext->Uxglobcc_shown
#define errordefStat            UxMainPanelShellContext->UxerrordefStat
#define toleranceStat           UxMainPanelShellContext->UxtoleranceStat
#define devsStat                UxMainPanelShellContext->UxdevsStat
#define histoIdStat             UxMainPanelShellContext->UxhistoIdStat
#define rangeStat               UxMainPanelShellContext->UxrangeStat
#define xvectStat               UxMainPanelShellContext->UxxvectStat
#define yvectStat               UxMainPanelShellContext->UxyvectStat
#define eyvectStat              UxMainPanelShellContext->UxeyvectStat
#define optnStat                UxMainPanelShellContext->UxoptnStat
#define ffuncStat               UxMainPanelShellContext->UxffuncStat
#define nparStat                UxMainPanelShellContext->UxnparStat
#define fmodelStat              UxMainPanelShellContext->UxfmodelStat
#define vertScrollBarOn         UxMainPanelShellContext->UxvertScrollBarOn
#define horlScrollBarOn         UxMainPanelShellContext->UxhorlScrollBarOn
#define wVertScrollBar          UxMainPanelShellContext->UxwVertScrollBar
#define hHorlScrollBar          UxMainPanelShellContext->UxhHorlScrollBar
#define dScrolledWinSpacing     UxMainPanelShellContext->UxdScrolledWinSpacing
#define dScrolledWinShadow      UxMainPanelShellContext->UxdScrolledWinShadow
#define parFormWid              UxMainPanelShellContext->UxparFormWid
#define parFormHei              UxMainPanelShellContext->UxparFormHei
#define parFormWidPix           UxMainPanelShellContext->UxparFormWidPix
#define parFormHeiPix           UxMainPanelShellContext->UxparFormHeiPix
#define hNameText               UxMainPanelShellContext->UxhNameText
#define hBotSeparator           UxMainPanelShellContext->UxhBotSeparator
#define hTopSeparator           UxMainPanelShellContext->UxhTopSeparator
#define wActiveToggle           UxMainPanelShellContext->UxwActiveToggle
#define wNumberValue            UxMainPanelShellContext->UxwNumberValue
#define wNameText               UxMainPanelShellContext->UxwNameText
#define wScale                  UxMainPanelShellContext->UxwScale
#define wScaleAdjustButton      UxMainPanelShellContext->UxwScaleAdjustButton
#define wValueText              UxMainPanelShellContext->UxwValueText
#define wFixToggle              UxMainPanelShellContext->UxwFixToggle
#define wLowLimitText           UxMainPanelShellContext->UxwLowLimitText
#define wUppLimitText           UxMainPanelShellContext->UxwUppLimitText
#define wStepText               UxMainPanelShellContext->UxwStepText
#define wEparabText             UxMainPanelShellContext->UxwEparabText
#define wEminusText             UxMainPanelShellContext->UxwEminusText
#define wEplusText              UxMainPanelShellContext->UxwEplusText
#define wGlobccText             UxMainPanelShellContext->UxwGlobccText
#define wScaleMinMaxForm        UxMainPanelShellContext->UxwScaleMinMaxForm
#define xScaleMinMax            UxMainPanelShellContext->UxxScaleMinMax
#define histid                  UxMainPanelShellContext->Uxhistid
#define lowbins                 UxMainPanelShellContext->Uxlowbins
#define uppbins                 UxMainPanelShellContext->Uxuppbins
#define optn                    UxMainPanelShellContext->Uxoptn
#define fexpr                   UxMainPanelShellContext->Uxfexpr
#define fmodel                  UxMainPanelShellContext->Uxfmodel
#define fcn                     UxMainPanelShellContext->Uxfcn
#define ufcn                    UxMainPanelShellContext->Uxufcn
#define npar                    UxMainPanelShellContext->Uxnpar
#define parname                 UxMainPanelShellContext->Uxparname
#define vpar                    UxMainPanelShellContext->Uxvpar
#define vlow                    UxMainPanelShellContext->Uxvlow
#define vupp                    UxMainPanelShellContext->Uxvupp
#define vstep                   UxMainPanelShellContext->Uxvstep
#define veparab                 UxMainPanelShellContext->Uxveparab
#define veminus                 UxMainPanelShellContext->Uxveminus
#define veplus                  UxMainPanelShellContext->Uxveplus
#define par_start               UxMainPanelShellContext->Uxpar_start
#define par_store               UxMainPanelShellContext->Uxpar_store
#define par_active              UxMainPanelShellContext->Uxpar_active
#define vfixrel                 UxMainPanelShellContext->Uxvfixrel
#define par_enabled             UxMainPanelShellContext->Uxpar_enabled
#define vscalemin               UxMainPanelShellContext->Uxvscalemin
#define vscalemax               UxMainPanelShellContext->Uxvscalemax
#define cont_num1               UxMainPanelShellContext->Uxcont_num1
#define cont_num2               UxMainPanelShellContext->Uxcont_num2
#define cont_npt                UxMainPanelShellContext->Uxcont_npt
#define cont_nfound             UxMainPanelShellContext->Uxcont_nfound
#define cont_xpt                UxMainPanelShellContext->Uxcont_xpt
#define cont_ypt                UxMainPanelShellContext->Uxcont_ypt
#define cont_overlay            UxMainPanelShellContext->Uxcont_overlay
#define cont_xvecnam            UxMainPanelShellContext->Uxcont_xvecnam
#define cont_yvecnam            UxMainPanelShellContext->Uxcont_yvecnam
#define scan_num                UxMainPanelShellContext->Uxscan_num
#define scan_npt                UxMainPanelShellContext->Uxscan_npt
#define scan_xfrom              UxMainPanelShellContext->Uxscan_xfrom
#define scan_xto                UxMainPanelShellContext->Uxscan_xto
#define scan_xpt                UxMainPanelShellContext->Uxscan_xpt
#define scan_ypt                UxMainPanelShellContext->Uxscan_ypt
#define vec_pnam                UxMainPanelShellContext->Uxvec_pnam
#define vec_snam                UxMainPanelShellContext->Uxvec_snam
#define vec_lnam                UxMainPanelShellContext->Uxvec_lnam
#define vec_unam                UxMainPanelShellContext->Uxvec_unam
#define vec_enam                UxMainPanelShellContext->Uxvec_enam
#define UxParent                UxMainPanelShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_mainPanelShell( swidget _UxUxParent );

#endif  /* _MAINPANEL_INCLUDED */
