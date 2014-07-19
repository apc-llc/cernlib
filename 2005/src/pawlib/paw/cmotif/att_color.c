/*
 * $Id: att_color.c,v 1.1.1.1 1996/03/01 11:38:53 mclareni Exp $
 *
 * $Log: att_color.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:53  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 05/03/94  14.53.28  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *    Manage the Color Dialog Box                                            *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"
#include "hmotif/attstate.h"

#ifndef MIN
#define MIN(a,b) ( (a<b) ? (a) : (b) )
#endif

/* extern functions */
extern Widget  create_colorDialog();
extern void ixgetcol(int num,float *r,float *g,float *b);
#define ixsetco ixsetco_
extern void ixsetco(int *num,float *r,float *g,float *b);
extern void copy_color ( XColor *to,XColor *from);

/* extern widgets */
extern Widget ObjectRow;
extern Widget ColorButton;
extern Widget AttColorText,ColorUpArrow,ColorDownArrow;
extern Widget CloseAttColorBut,ApplyAttColorBut,ResetAttColorButton;
extern Display *UxDisplay;
extern int UxScreen;
extern Widget TargetRow,ContourToggle,SurfaceToggle,StatToggle,ZoneToggle;
extern Widget HistoToggle,BoxToggle,YAxisToggle,XAxisToggle,FunctionToggle,
   PictureToggle;
extern Widget widget_defineColor,NcolText;

/* global data */
Widget widget_color=(Widget) NULL;

/* global function */
void show_color (Widget w,caddr_t client_data,XmAnyCallbackStruct *call_data);
void UpdateAfterReset();

/* local function */
static void NewColor_cb (Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data);

static void ApplyColorDialog_cb(Widget w,caddr_t client_data,
                                XmAnyCallbackStruct *call_data);
static void ResetColorDialog_cb (Widget w,caddr_t client_data,
                                 XmAnyCallbackStruct *call_data);
static  void NewTarget_cb (Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);

/* local data */
static Colormap sys_cmap;
static int Change;
static XColor CurrentColor;
static unsigned long CurrentPixel;

/*****************************************************************************
 *                                                                           *
 *    Change the color of the arrow button after a Reset in Define Color     *
 *         call only by att_define_color:defineColor_cb                      *
 *****************************************************************************/
void UpdateAfterReset() {
   int num;

   num=atoi((char *)XmTextGetString(AttColorText));
   FindRgb(num,&CurrentColor);
   XStoreColor(UxDisplay,sys_cmap,&CurrentColor);
}

/*****************************************************************************
 *                                                                           *
 *    Change the color of the arrow button after a define color              *
 *         call only by att_define_color:defineColor_cb                      *
 *****************************************************************************/
void ChangeAttColor(XColor color,int num) {
   if (num==atoi((char *)XmTextGetString(AttColorText))) {
      copy_color(& CurrentColor,&color);
      XStoreColor(UxDisplay,sys_cmap,&CurrentColor);
   }
}

/*****************************************************************************
 *                                                                           *
 *     A new target has been choosen                                         *
 *                                                                           *
 *****************************************************************************/
static  void NewTarget_cb (Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data)
{
   char Svalue[6];
   Widget CurrentObject;
   TStateObject *data;
   XmAnyCallbackStruct cb;

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
   switch ((int ) client_data) {
      case 1: /* Surface */
         sprintf(Svalue,"%d",data->SurfaceColor);
         break;
      case 2: /* contour */
         sprintf(Svalue,"%d",data->ContourColor);
         break;
      case 3: /* stat */
         sprintf(Svalue,"%d",data->StatColor);
         break;
      case 4: /* Zone */
         sprintf(Svalue,"%d",data->ZoneColor);
         break;
      default :
         fprintf(stderr,"error in switch in NewTarget ");
   }

   XmTextSetString(AttColorText,Svalue);
   cb.reason=0;
   cb.event=(XEvent *)NULL;
   XtCallCallbacks(AttColorText,XmNactivateCallback,&cb);

}

/*****************************************************************************
 *                                                                           *
 *      A new color has been choosen                                         *
 *                                                                           *
 *****************************************************************************/
static  void NewColor_cb (Widget w,caddr_t client_data,
                          XmAnyCallbackStruct *call_data)
{
   int ColorNum;
   char SValue[5];
   int Ncol;
   Widget CurrentObject,CurrentTarget;
   TStateObject *data;
   char *ComStr;
   int *PtrColor;
   int surface1000;

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
   XtVaGetValues(TargetRow,XmNmenuHistory,&CurrentTarget,NULL);


   if (widget_defineColor) {
      Ncol=atoi((char *)XmTextGetString(NcolText));
   } else Ncol=8;
   ColorNum=atoi((char *)XmTextGetString(AttColorText));
   switch((int) client_data) {
      case 1: /* UpArrow */
         ColorNum++;
         break;
      case 2:/* DownArrow */
         ColorNum--;
         break;
      case 3: /* the text has been changed */
         break;
      default :
         fprintf(stderr,"Internal error in function NewColor_cb");
   }
   if (ColorNum>=Ncol) {
      ColorNum=Ncol-1;
      sprintf(SValue,"%d",ColorNum);
      XmTextSetString(AttColorText,SValue);
   }
   if (ColorNum<0) {
      ColorNum=0;
      sprintf(SValue,"%d",ColorNum);
      XmTextSetString(AttColorText,SValue);
   }

   if (((CurrentTarget==ZoneToggle) || (CurrentTarget==StatToggle))
                                    && (ColorNum>9)) {
      ColorNum=MIN(Ncol,9);
      sprintf(SValue,"%d",ColorNum);
      XmTextSetString(AttColorText,SValue);
   }


   sprintf(SValue,"%d",ColorNum);
   XmTextSetString(AttColorText,SValue);


   if (((call_data)->reason)) {
/* We don't do this if it is a call after a new Current has been selected */
/* update the value in ObjectState */

      if (CurrentTarget==SurfaceToggle) {
         PtrColor = &(data->SurfaceColor);
      } else if (CurrentTarget==ContourToggle) {
         PtrColor = &(data->ContourColor);
      } else if (CurrentTarget==ZoneToggle) {
         PtrColor = &(data->ZoneColor);
      } else if (CurrentTarget==StatToggle){
         PtrColor = &(data->StatColor);
      } else {
         fprintf(stderr,"Unknown widget in target row ");
         exit(1);
      }

      if (*PtrColor!=ColorNum) {
         *PtrColor=ColorNum;
         Change=True;

      } else
         Change=False;

      if (Change) {
         ComStr=XtCalloc(MAX_CMD_LENGTH,1);
         if (CurrentObject==BoxToggle) {
            if (CurrentTarget==SurfaceToggle) {
               data->ContourColor=0;
               surface1000=1000+data->SurfaceColor;
               sprintf(ComStr,"SET %sCOL %d.%d%d",data->FirstLetter,surface1000,
                       data->ZoneColor,data->StatColor);
            } else if (CurrentTarget==ContourToggle) {
               data->SurfaceColor=0;
               sprintf(ComStr,"SET %sCOL %d.%d%d",data->FirstLetter,
                       data->ContourColor,
                       data->ZoneColor,data->StatColor);
            } else if ((CurrentTarget==ZoneToggle) ||
                       (CurrentTarget==StatToggle)) {
               if (data->SurfaceColor) {
                  surface1000=1000+data->SurfaceColor;
                  sprintf(ComStr,"SET %sCOL %d.%d%d",data->FirstLetter,
                          surface1000,
                          data->ZoneColor,data->StatColor);
               } else {
                  sprintf(ComStr,"SET %sCOL %d.%d%d",data->FirstLetter,
                          data->ContourColor,
                          data->ZoneColor,data->StatColor);
               }
            } else
               fprintf(stderr,"Internal Error in NewColor_cb\n");

         } else {
            if (CurrentTarget==SurfaceToggle) {
               surface1000=1000+data->SurfaceColor;
               /* data->SurfaceColor+=1000;*/
               sprintf(ComStr,"SET %sCOL %d",data->FirstLetter,surface1000);
            } else if (CurrentTarget==ContourToggle) {

               sprintf(ComStr,"SET %sCOL %d",data->FirstLetter,
                       data->ContourColor);
            } else
               fprintf(stderr,"default in case in att_color");

         } /* else */
         execute_kuip_cmd(ComStr);
         if (appres.auto_refresh) {
            plot_current_histo();
         }
         XtFree(ComStr);
      } /* Change */
   }

   FindRgb(ColorNum,&CurrentColor);
   XStoreColor(UxDisplay,sys_cmap,&CurrentColor);
}

/*****************************************************************************
 *                                                                           *
 *    Manage the Apply Button                                                *
 *                                                                           *
 *****************************************************************************/
static  void ApplyColorDialog_cb(Widget w,caddr_t client_data,
                                 XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *            Manage the reset button                                        *
 *                                                                           *
 *****************************************************************************/
static  void ResetColorDialog_cb (Widget w,caddr_t client_data,
                                  XmAnyCallbackStruct *call_data)
{
   XmAnyCallbackStruct cb;
   Widget  CurrentObject;
   TStateObject *data;

   cb.reason=1;
   cb.event=(XEvent *)NULL;


   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);

   data->ZoneColor=data->StatColor=data->SurfaceColor=data->ContourColor=0;
   /* data->ContourColor is set to zero and will be set to 1 by the callback*/
   /*  data->SurfaceColor is set to 0 */
   XmToggleButtonSetState(ContourToggle,True,True);
   XmTextSetString(AttColorText,"1");
   XtCallCallbacks(AttColorText,XmNactivateCallback,&cb);
}

/*****************************************************************************
 *                                                                           *
 *     Init the Color Dialog Box and PopupInterface                          *
 *                                                                           *
 *****************************************************************************/
void show_color (Widget w,caddr_t client_data,XmAnyCallbackStruct *call_data)
{
   static MenuCbStruct *menu_a_settings;
   Widget CurrentObject;
   TStateObject *data;
   XmToggleButtonCallbackStruct CbToggle;
   XmString Label;
   char *LabelBis;
   char *LabelTer;
   Boolean ret;

   if (!widget_color) {

      widget_color= create_colorDialog();

      menu_a_settings = make_menuStruct(w, widget_color);
      InstallMwmCloseCallback(XtParent(widget_color),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_a_settings);
      /* setup of colors */
      sys_cmap=DefaultColormap(UxDisplay,UxScreen);

      if ( ! XAllocColorCells(UxDisplay,sys_cmap,False,NULL,0,
                              &CurrentPixel,1)) {
         km_inform_user(XtParent(w),"Unable to allocate new \
colors: restart server or close the application using color","");
         widget_color=0;
         return;
      }
      CurrentColor.pixel=CurrentPixel;
      CurrentColor.flags=DoRed | DoBlue | DoGreen;

      /* Init */
      XtVaSetValues(widget_color,XmNbackgroundPixmap,XmUNSPECIFIED_PIXMAP,NULL);
      XtVaSetValues(ColorUpArrow,XmNforeground,CurrentColor.pixel,NULL);
      XtVaSetValues(ColorDownArrow,XmNforeground,CurrentColor.pixel,NULL);

/****
 * Callback
 ******/

      XtAddCallback(ColorUpArrow,XmNarmCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 1);
      XtAddCallback(ColorDownArrow,XmNarmCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 2);
      XtAddCallback(AttColorText,XmNactivateCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 3);
      XtAddCallback(AttColorText,XmNlosingFocusCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 3);

      XtAddCallback(SurfaceToggle,XmNarmCallback,
                    (XtCallbackProc) NewTarget_cb,(int *) 1);
      XtAddCallback(ContourToggle,XmNarmCallback,
                    (XtCallbackProc) NewTarget_cb,(int *) 2);
      XtAddCallback(StatToggle,XmNarmCallback,
                    (XtCallbackProc) NewTarget_cb,(int *) 3);
      XtAddCallback(ZoneToggle,XmNarmCallback,
                    (XtCallbackProc) NewTarget_cb,(int *) 4);

      XtAddCallback(CloseAttColorBut,XmNactivateCallback,
                    (XtCallbackProc) cancel_cb,menu_a_settings);
      XtAddCallback(ApplyAttColorBut,XmNactivateCallback,
                    (XtCallbackProc) ApplyColorDialog_cb,NULL);
      XtAddCallback(ResetAttColorButton,XmNactivateCallback,
                    (XtCallbackProc) ResetColorDialog_cb,NULL);
      if (appres.auto_refresh) XtSetSensitive(ApplyAttColorBut,False);
      else XtSetSensitive(ApplyAttColorBut,True);

      FindRgb(1,&CurrentColor);
      XStoreColors(UxDisplay,sys_cmap,&CurrentColor,1);

      /* update the dialog title */
      XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
      XtVaGetValues(CurrentObject,XmNlabelString,&Label,NULL);
      LabelTer=XtCalloc(30,1);
      ret=XmStringGetLtoR(Label,XmSTRING_DEFAULT_CHARSET,&LabelBis);
      if (!ret)
         fprintf(stderr,"Unable to extract string in show_color\n");
      strcpy(LabelTer,LabelBis);
      XtVaSetValues(XtParent(widget_color),XmNtitle,strcat(LabelTer," Color")
                    ,NULL);
      XtFree(LabelTer);
      XtFree(LabelBis);
   }

   /* update the display */

   XmToggleButtonSetState(ContourToggle,True,True);
   CbToggle.event=(XEvent *)NULL;
   CbToggle.set=True;
   CbToggle.reason=0;
   XtCallCallbacks(ContourToggle,XmNarmCallback,&CbToggle);

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
   XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
   if (CurrentObject==BoxToggle) {
      XtSetSensitive(SurfaceToggle,True);
      XtSetSensitive(ZoneToggle,True);
      XtSetSensitive(StatToggle,True);
   } else {
      XtSetSensitive(ZoneToggle,False);
      XtSetSensitive(StatToggle,False);
      if ((CurrentObject==YAxisToggle) || (CurrentObject==XAxisToggle)) {
         XtSetSensitive(SurfaceToggle,False);
      } else {
         XtSetSensitive(SurfaceToggle,True);
      }
   }
   XtSetSensitive(w,False);
   XtManageChild(widget_color);
}

